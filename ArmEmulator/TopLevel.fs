namespace Emulator
module TopLevel = 
    open MultMem
    open CommonLex
    open CommonData
    open System.Text.RegularExpressions


    /// allows different modules to return different instruction types
    type Instr =
        // arithmetic instrucitons
        | IARITH of Arithmetic.ReturnInstr
        // bit arithmetic instructions
        // TODO: needs renaming
        | IBITARITH of BitArithmetic.InstDecomp
        // memory instructions
        | IMEM of Mem.ReturnInstr
        // multiple memory instructions (and branch & end)
        | IMULTMEM of MultMem.ReturnInstr
        // blank lines need to be counted
        | BLANKLINE

    
    /// allows different modules to return different error info
    /// by default all return string so this is not needed
    type ErrInstr =
        | ERRIARITH of Arithmetic.ErrInstr
        | ERRIBITARITH of BitArithmetic.ErrInstr
        | ERRIMEM of Mem.ErrInstr
        | ERRIMULTMEM of MultMem.ErrInstr
        | ERRTOPLEVEL of string
        | ERRLINE of ErrInstr * uint32

    type CondInstr = Condition * Instr

    /// Note that Instr in Mem and DP modules is NOT same as Instr in this module
    /// Instr here is all possible instruction values combined with a D.U.
    /// that tags the Instruction class
    /// Similarly ErrInstr
    /// Similarly IMatch here is combination of module IMatches
    let IMatch (ld: LineData) : Result<Parse<Instr>,ErrInstr> option =
        let pConv fr fe p = pResultInstrMap fr fe p |> Some
        match ld with
        | Arithmetic.IMatch pa -> pConv IARITH ERRIARITH pa
        | BitArithmetic.IMatch pa -> pConv IBITARITH ERRIBITARITH pa
        | Mem.IMatch pa -> pConv IMEM ERRIMEM pa
        | MultMem.IMatch pa -> pConv IMULTMEM ERRIMULTMEM pa
        | _ -> None


    ///Match the pattern using a cached compiled Regex
    let (|Match|_|) pattern input =
        if isNull input then None
        else
            let m = Regex.Match(input, pattern, RegexOptions.Compiled)
            if m.Success then
                [0..m.Groups.Count-1]
                |> List.map (fun i -> m.Groups.[i])
                |> Some
            else None

    ///Sets all registers to uppercase, ie r0 -> R0
    let uppercaseRegister str =
        let addUpper (strMatch: char) (splitList: string list) =
            let upperMatch = (strMatch|>string).ToUpper()
            let list1 =List.collect (fun a -> [a;upperMatch]) splitList
            list1.[0..(List.length list1)-2]
        match str with
        | Match @"[\[| |,](r1[0-5]|r[0-9])[ |,|\]]" [_; ex] -> 
            ex.Value
            |> fun y -> (Seq.toList y).[0]
            |> fun x -> str.Split(x)
            |> Seq.toList
            |> addUpper (Seq.toList ex.Value).[0]
            |> List.reduce (+)
        | _ -> str

    let opsToUpper operands =
        let uppercaseRegister matchStr str =
            let addUpper (strMatch: char) (splitList: string list) =
                let upperMatch = (strMatch|>string).ToUpper()
                let list1 =List.collect (fun a -> [a;upperMatch]) splitList
                list1.[0..(List.length list1)-2]
            let matcher strM = (sprintf @"[ |,]"+strM+"[ |,]") 
            match str with
            | Match (matcher matchStr) [_; ex] -> 
                ex.Value
                |> fun y -> (Seq.toList y).[0]
                |> fun x -> str.Split(x)
                |> Seq.toList
                |> addUpper (Seq.toList ex.Value).[0]
                |> List.reduce (+)
            | _ -> str
        let testLstLst:(Printf.StringFormat<string> list * string) list = 
            [([@"lsl"; @"Lsl"; @"lSl"; @"lsL"; @"LSl"; @"lSL"; @"LsL";], "LSL");
            ([@"lsr"; @"Lsr"; @"lSr"; @"lsR"; @"LSr"; @"lSR"; @"LsR";],"LSR");
            ([@"ror"; @"Ror"; @"rOr"; @"roR"; @"ROr"; @"rOR"; @"RoR";],"ROR");
            ([@"rrx"; @"Rrx"; @"rRx"; @"rrX"; @"RRx"; @"rRX"; @"RrX";],"RRX");
            ([@"asr"; @"Asr"; @"aSr"; @"asR"; @"ASr"; @"aSR"; @"AsR";],"ASR")]
        let testMap replacer operands testLst = 
            let uppercaseReplace matcher (replacer: string) (txt:string) = 
                txt
                |> fun t -> System.Text.RegularExpressions.Regex.Replace(t, (sprintf matcher), replacer);
            let trans = List.map (fun a -> uppercaseReplace a replacer operands) testLst
            match (List.filter (fun a -> a <> operands) trans).Length with 
            | 0 -> trans.[0]
            | _ -> (List.filter (fun a -> a <> operands) trans).[0]
        let upRegs = uppercaseRegister "(r1[0-5]|r[0-9])" operands
        let trans = List.map (fun (a,b) -> testMap b upRegs a) testLstLst
        match (List.filter (fun a -> a <> upRegs) trans).Length with 
        | 0 -> trans.[0]
        | _ -> (List.filter (fun a -> a <> upRegs) trans).[0]


    /// attempts to parse an individual line string
    /// given a symbol table and word load address
    /// returns either an error from any of the modules
    /// or on success, a parsed instruction
    let parseLine (symtab: SymbolTable) (loadAddr: WAddr) (asmLine:string) =
        /// put parameters into a LineData record
        let makeLineData (opcode: string) operands = {
            OpCode= opcode.ToUpper()
            Operands=
                String.concat " " operands
                |> opsToUpper
            Label=None
            LoadAddr = loadAddr
            SymTab = Some symtab
        }
        let removeWhitespace (txt:string) = 
            txt.Trim()
            |> fun t -> System.Text.RegularExpressions.Regex.Replace(t, @"\s+", " ");

        /// remove comments from string
        let removeComment (txt:string) =
            txt.Split(';')
            |> function 
                | [|x|] -> x 
                | [||] -> "" 
                | lineWithComment -> lineWithComment.[0]
        /// split line on whitespace into an array
        let splitIntoWords ( line:string ) =
            line.Split( ([||] : char array))
        /// define a parsed instruction corresponding to a blank line
        let blankInstr = (Ok {PInstr = BLANKLINE;PLabel = None;PCond = Cal;PSize = 0u;})
        /// try to parse 1st word, or 2nd word, as opcode
        /// If 2nd word is opcode 1st word must be label
        let matchLine words =
            let pNoLabel =
                match words with
                // if the line is blank, return BLANKLINE
                | "" :: _ -> Some blankInstr
                // otherwise, try and parse the line assuming it doesn't have a label
                | opc :: operands -> 
                    makeLineData opc operands 
                    |> IMatch
                | _ -> None
            match pNoLabel, words with
            // if parsing worked, return the result
            | Some pa, _ -> pa 
            // if the line is blank, return BLANKLINE
            | None, _ :: "" :: _ -> blankInstr
            // otherwise, assume the line has a label and try to parse that
            | None, label :: opc :: operands -> 
                match { makeLineData opc operands with Label=Some label} |> IMatch with
                | None -> 
                    Error (ERRTOPLEVEL 
                        (sprintf "Instruction not implemented: %s" (String.concat " " words)))
                | Some pa -> pa
            | _ -> 
                Error (ERRTOPLEVEL
                    (sprintf "Invalid instruction: %s" (String.concat " " words)))
        asmLine
        |> removeWhitespace
        |> removeComment
        |> splitIntoWords
        |> Array.toList
        |> matchLine


    /// initialise DataPath object before executing instructions
    /// takes optional flags/regs/mem to initialise with
    /// otherwise, all empty to begin with (false flags, zero regs, no memory allocated)
    let initDataPath flags regs mem = 
        let initFlags = 
            match flags with
            | Some f -> f
            | None -> {N=false;Z=false;C=false;V=false;} 
        let initRegs = 
            match regs with
            | Some r when Map.count r = 16 -> Ok r
            | None ->
                [0..15]
                |> List.map (fun i -> inverseRegNums.[i], 0u)
                |> Map.ofList
                |> Ok
            | _ -> Error (ERRTOPLEVEL "Invalid initial registers.")
        let initMem = 
            match mem with
            | Some m -> m
            | None -> Map.empty
        match initFlags, initRegs, initMem with
        | f, Ok r, m ->
            Ok {
                Fl = f;
                Regs = r;
                MM = m;
            }
        | _, Error s, _ -> Error s
    
    // map a function which returns its own error type
    // to one which returns a top-level error type
    // including the line number the error occurred on
    let mapErr lineNum fMap ins = 
        match ins with
        | Error s -> Error (ERRLINE (fMap s, lineNum))
        | Ok x -> Ok x

    // checks whether an instruction should be executed
    // based on condition code and current flags
    let checkCond flags cond =
        let n, z, c, v = flags.N, flags.Z, flags.C, flags.V
        match cond with 
        | Ceq -> z                // execute if Z=1
        | Cne -> not z            // execute if Z=0      
        | Cmi -> n                // execute if N=1
        | Cpl -> not n            // execute if N=0
        | Cvs -> v                // execute if V = 1
        | Cvc -> not v            // execute if V = 0            
        | Chs -> c                // execute if C = 1
        | Clo -> not c            // execute if C = 0            
        | Cge -> n = v            // execute if N = V            
        | Clt -> n = not v        // execute if N != V            
        | Chi -> c && (not z)     // execute if C = 1 and Z = 0
        | Cls -> (not c) || z     // execute if C = 0 or Z = 1
        | Cgt -> (not z) && (n = v)   // execute if Z = 0 and N = V 
        | Cle -> z && (n = not v)     // execute if Z = 1 and N != V"
        | Cnv -> true
        | Cal -> true

    // function to execute any parsed instruction from any module
    // returns the modified datapath plus symbol table (or an error)
    let execParsedLine ins d (symtab: SymbolTable) lineNum =
        /// executes a given instruction with the correct execution function f
        let exec' p f ins' err = 
            { PInstr = ins'; PLabel = p.PLabel; PCond = p.PCond; PSize = p.PSize }
            |> f
            |> mapErr lineNum err

        let includeSyms = Result.map (fun cpu -> cpu, symtab)
        // check if instruction should be executed
        match checkCond d.Fl ins.PCond with
        | true ->
            match ins with
            // Mem instructions modify both symbol table and datapath
            | {PInstr=IMEM ins';} as p -> 
                exec' p (Mem.execInstr d symtab) ins' ERRIMEM
            // the rest of the modules just modify datapath
            | {PInstr=IMULTMEM ins';} as p -> 
                exec' p (MultMem.execInstr d) ins' ERRIMULTMEM
                |> includeSyms
            | {PInstr=IARITH ins';} as p -> 
                exec' p (Arithmetic.execArithmeticInstr d) ins' ERRIARITH
                |> includeSyms
            | {PInstr=IBITARITH ins';} as p -> 
                exec' p (BitArithmetic.exeInstr d) ins' ERRIARITH
                |> includeSyms
            // if the line is blank, just return with no changes
            | {PInstr=BLANKLINE;} -> 
                Ok (d, symtab)
        // if the condition code says don't execute, return with no changes
        | false -> Ok (d, symtab)

    /// takes a list of lines as string
    /// and optionally, a symbol table
    /// parses each line using parseLine function
    /// returns a list of Ok parsed instructions, or errors
    /// plus the completed symbol table, if given
    let parseLines lines symtab = 
        // execute an instruction if it corresponds to an EQU instruction
        let execIfEqu = 
            function
            // need to check for EQU instructions
            | Ok ({PInstr = IMEM (Mem.LabelO _)} as p,syms) ->
                // create a dummy datapath
                let d = 
                    match initDataPath None None None with
                    | Ok d -> d
                    | Error _ -> failwithf "Should never happen."
                // execute the EQU instruction (datapath is not changed)
                execParsedLine p d syms 0u
                // use the updated symbol table and continue
                |> Result.map (snd)
                |> Result.map (fun s-> p,s)
            | i -> i
        // parses each line one by one, updating the symbol table as it goes
        let rec parseLines' lines parsedLines loadaddr (symtab': SymbolTable) = 
            let addLabel p =
                match p.PLabel, symtab' with
                | Some lab, syms -> p, (syms.Add lab)
                | _ -> p, symtab'
            match lines with
            // no more lines to parse, return (need to reverse list order)
            | [] -> List.rev parsedLines, symtab' 
            // otherwise, parse line and update symbol table
            | line :: rest ->
                parseLine symtab' (WA loadaddr) line
                |> Result.map addLabel
                |> execIfEqu
                |> function
                    | Ok (p, syms) -> parseLines' rest (Ok p :: parsedLines) (loadaddr + p.PSize) syms
                    // not sure how to increment size if error
                    | Error s -> parseLines' rest (Error s :: parsedLines) (loadaddr + 4u) symtab'
        parseLines' lines [] 0u symtab



    /// execute a list of parsed lines
    /// returns modified DataPath and symbol table
    /// binds any errors in the execution or parsed lines
    let execParsedLines parsedLines cpuData symtab = 
        // this puts each line of code in its correct memory location
        // starting at 0u, increasing by PSize each time
        let rec putCodeInMemory lines lineNum cpu' (insMap: Map<WAddr, Parse<Instr> * uint32>) currAddr = 
            match lines with
            | [] -> Ok (cpu', insMap)
            // ignore blank lines, but increment line number
            | Ok {PInstr = BLANKLINE;} :: rest -> 
                putCodeInMemory rest (lineNum+1u) cpu' insMap currAddr
            // also ignore EQU instructions,
            | Ok {PInstr = IMEM (Mem.LabelO (Ok {InstructionType = x}))} :: rest when x = Mem.EQU ->
                putCodeInMemory rest (lineNum+1u) cpu' insMap currAddr
            // otherwise, execute instruction as normal
            | Ok line :: rest ->
                {cpu' with MM = cpu'.MM.Add (WA currAddr, Code line.PInstr) }
                |> fun c ->
                    insMap.Add (WA currAddr, (line, lineNum))
                    |> fun iMap -> 
                        putCodeInMemory rest (lineNum + 1u) c iMap (currAddr + line.PSize)
            | Error s :: _ -> Error (ERRLINE (s, lineNum))

        let setProgCount cpu' = 
            {cpu' with Regs = cpu'.Regs.Add (R15, 8u)}

        let checkEnd cpu' (insMap: Map<WAddr, Parse<Instr> * uint32>) =
            cpu'.Regs.[R15] - 8u
            |> WA
            |> insMap.TryFind
            |> function
                | Some({PInstr=IMULTMEM (EndI _); PCond=cond}, _) 
                    when checkCond cpu'.Fl cond -> true
                | _ -> false
        let checkStop cpu' (insMap: Map<WAddr, Parse<Instr> * uint32>) =
            cpu'.Regs.[R15] - 8u
            |> WA
            |> insMap.TryFind
            |> function
                | None -> true
                | _ -> false

        let incrProgCount size cpu' =
            {cpu' with Regs = cpu'.Regs.Add (R15, cpu'.Regs.[R15] + size)}
        
        let checkBranch p = 
            match p with
            | {PInstr=IMULTMEM (BranchI _)} -> true
            | {PInstr=IBITARITH {Instruction = i; Dest = d}} 
                when (i = BitArithmetic.MOV || i = BitArithmetic.MVN)
                    && d = Some R15 -> true
            | _ -> false


        let rec execLines cpu' insMap symtab' branchCount = 
            // check if the program has reached the end, or if branch count exceeds limit
            match checkEnd cpu' insMap, branchCount > 1000u with
            // if so, return 
            | true, _ -> Ok (cpu', symtab')
            // otherwise, execute the next instruction
            | _, true -> 
                Error (ERRTOPLEVEL "Infinite loop detected. Branched more than 100,000 times.")
            | false, false ->
                // get the current PC value
                cpu'.Regs.[R15] - 8u
                // get the instruction at that PC value in memory
                |> fun a -> insMap.[WA a]
                |> fun (p, lineNum) -> 
                    // execute the instruction
                    execParsedLine p cpu' symtab' lineNum
                    |> Result.bind (
                        fun (newCpu, newSymTab) -> 
                            let branch = checkBranch p
                            let executed = checkCond cpu'.Fl p.PCond
                            // need to check if program is about to end
                            let nextCpu, newBranchCount = 
                                match branch && executed with
                                // if instruction branched, don't change PC
                                | true -> newCpu, branchCount + 1u
                                // otherwise, increment PC
                                | false -> incrProgCount p.PSize newCpu, branchCount
                            checkStop nextCpu insMap 
                            |> function
                                // if so, return
                                | true -> 
                                    Ok (newCpu, newSymTab)
                                // if not, increment PC and continue exection
                                | false -> execLines nextCpu insMap newSymTab newBranchCount
                    )
        setProgCount cpuData
        |> fun cpu -> putCodeInMemory parsedLines 0u cpu Map.empty 0u
        |> Result.bind (fun (cpu', insMap) -> 
            match insMap = Map.empty with
            | false -> execLines cpu' insMap symtab 0u
            | true -> Ok (cpu', symtab))


    /// takes a list of lines as string, a datapath and an optional symbol table
    /// does 2 passes of parsing to get symbol table contents correct
    /// then executes the instructions with the symbol table (if given)
    let parseThenExecLines lines cpuData symtab = 
        // first pass of parsing
        parseLines lines symtab
        // second pass of parsing
        |> snd
        |> parseLines lines
        // execute
        |> function
            | parsedLines, syms -> execParsedLines parsedLines cpuData syms
