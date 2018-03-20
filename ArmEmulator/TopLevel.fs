namespace Emulator
open MultMem
module TopLevel = 
    open CommonLex
    open CommonData

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

    let parseLine (symtab: SymbolTable option) (loadAddr: WAddr) (asmLine:string) =
        /// put parameters into a LineData record
        let makeLineData opcode operands = {
            OpCode=opcode
            Operands=String.concat "" operands
            Label=None
            LoadAddr = loadAddr
            SymTab = symtab
        }
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
        /// try to parse 1st word, or 2nd word, as opcode
        /// If 2nd word is opcode 1st word must be label
        let blankInstr = (Ok {
                                PInstr = BLANKLINE;
                                PLabel = None;
                                PCond = Cal;
                                PSize = 0u;})
        let matchLine words =
            let pNoLabel =
                match words with
                // if the line is blank, return BLANKLINE
                | "" :: _ -> Some blankInstr
                | opc :: operands -> 
                    makeLineData opc operands 
                    |> IMatch
                | _ -> None
            match pNoLabel, words with
            | Some pa, _ -> pa 
            // if the line is blank, return BLANKLINE
            | None, _ :: "" :: _ -> blankInstr
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
    // returns the modified datapath (or an error)
    let execParsedLine ins d (symtab: SymbolTable) lineNum =
        /// executes a given instruction with the correct execution function f
        let exec' p f ins' err = 
            { PInstr = ins'; PLabel = p.PLabel; PCond = p.PCond; PSize = p.PSize }
            |> f
            |> mapErr lineNum err

        // check if instruction should be executed
        match checkCond d.Fl ins.PCond with
        | true ->
            match ins with
            | {PInstr=IMEM ins';} as p -> 
                exec' p (Mem.execInstr d symtab) ins' ERRIMEM
            | {PInstr=IMULTMEM ins';} as p -> 
                exec' p (MultMem.execInstr d) ins' ERRIMULTMEM
                |> Result.map (fun cpu -> cpu, symtab)
            | {PInstr=IARITH ins';} as p -> 
                exec' p (Arithmetic.execArithmeticInstr d) ins' ERRIARITH
                |> Result.map (fun cpu -> cpu, symtab)
            | {PInstr=IBITARITH ins';} as p -> 
                exec' p (BitArithmetic.exeInstr d) ins' ERRIARITH
                |> Result.map (fun cpu -> cpu, symtab)
            | {PInstr=BLANKLINE;} -> 
                Ok (d, symtab)
        | false -> Ok (d, symtab)

    /// takes a list of lines as string
    /// and optionally, a symbol table
    /// parses each line using parseLine function
    /// returns a list of Ok parsed instructions, or errors
    /// plus the completed symbol table, if given
    let parseLines lines symtab = 
        // adds all labels in the parsed lines to the symbol table
        let rec createSymTab (symtab': SymbolTable option) parsedLines =
            match parsedLines, symtab' with
            | [], _ -> symtab'
            | (Ok line) :: rest, Some syms -> 
                match line.PLabel with
                | Some lab -> createSymTab (Some (syms.Add lab)) rest
                | None -> createSymTab symtab' rest
            | _ :: rest, _ ->  createSymTab symtab' rest
        // parses each line one by one, updating the symbol table as it goes
        // tail recursive so it can't return the final symbol table (maybe refactor?)
        let rec parseLines' lines loadaddr (symtab': SymbolTable option) = 
            let addLabel p =
                match p.PLabel, symtab' with
                | Some lab, Some syms -> p, Some (syms.Add lab)
                | _ -> p, symtab'
            match lines with
            // no more lines to parse, return empty
            | [] -> [] 
            | line :: rest ->
                parseLine symtab' (WA loadaddr) line
                |> Result.map addLabel
                |> function
                    | Ok (p, syms) -> Ok p :: parseLines' rest (loadaddr + p.PSize) syms
                    // not sure how to increment size if error
                    | Error s -> Error s :: parseLines' rest (loadaddr + 4u) symtab'
        parseLines' lines 0u symtab
        |> fun p -> p, createSymTab symtab p



    /// execute a list of parsed lines
    /// returns modified DataPath and symbol table
    /// binds any errors in the execution or parsed lines
    let execParsedLines parsedLines cpuData symtab = 
        // this puts each line of code in its correct memory location
        // starting at 0u, increasing by PSize each time
        let rec putCodeInMemory lines lineNum cpu' (insMap: Map<WAddr, Parse<Instr> * uint32>) currAddr = 
            match lines with
            | [] -> Ok (cpu', insMap)
            | Ok {PInstr = BLANKLINE;} :: rest -> putCodeInMemory rest (lineNum+1u) cpu' insMap currAddr
            | Ok line :: rest ->
                {cpu' with MM = cpu'.MM.Add (WA currAddr, Code line.PInstr) }
                |> fun c ->
                    insMap.Add (WA currAddr, (line, lineNum))
                    |> fun iMap -> putCodeInMemory rest (lineNum + 1u) c iMap (currAddr + line.PSize)
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
            | _ -> false


        let rec execLines cpu' insMap symtab' branchCount = 
            // check if the program has reached the end, or if branch count exceeds limit
            match checkEnd cpu' insMap, branchCount > 100000u with
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
                                | true -> Ok (newCpu, newSymTab)
                                // if not, increment PC and continue exection
                                | false -> execLines nextCpu insMap newSymTab newBranchCount
                    )
        setProgCount cpuData
        |> fun cpu -> putCodeInMemory parsedLines 0u cpu Map.empty 0u
        |> Result.bind (fun (cpu', insMap) -> execLines cpu' insMap symtab 0u)


    /// takes a list of lines as string, a datapath and an optional symbol table
    /// does 2 passes of parsing to get symbol table contents correct
    /// then executes the instructions with the symbol table (if given)
    let parseThenExecLines lines cpuData symtab = 
        // first pass of parsing
        parseLines lines symtab
        // second pass of parsing
        |> snd |> parseLines lines
        // execute
        |> function
            | parsedLines, Some syms -> execParsedLines parsedLines cpuData syms
            | parsedLines, None -> execParsedLines parsedLines cpuData Map.empty
