module TopLevel
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

    
    /// allows different modules to return different error info
    /// by default all return string so this is not needed
    type ErrInstr =
        | ERRIARITH of Arithmetic.ErrInstr
        | ERRIBITARITH of BitArithmetic.ErrInstr
        | ERRIMEM of Mem.ErrInstr
        | ERRIMULTMEM of MultMem.ErrInstr
        | ERRTOPLEVEL of string

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
            line.Split( ([||] : char array), 
                System.StringSplitOptions.RemoveEmptyEntries)
        /// try to parse 1st word, or 2nd word, as opcode
        /// If 2nd word is opcode 1st word must be label
        let matchLine words =
            let pNoLabel =
                match words with
                | opc :: operands -> 
                    makeLineData opc operands 
                    |> IMatch
                | _ -> None
            match pNoLabel, words with
            | Some pa, _ -> pa
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
    let mapErr fMap ins = 
        match ins with
        | Error s -> Error (fMap s)
        | Ok x -> Ok x

    // function to execute any parsed instruction from any module
    // returns the modified datapath (or an error)
    let execParsedLine ins d (symtab: SymbolTable) =
        /// executes a given instruction with the correct execution function f
        let exec' p f ins' err = 
            { PInstr = ins'; PLabel = p.PLabel; PCond = p.PCond; PSize = p.PSize }
            |> f
            |> mapErr err

        ins |> Result.bind (
            function
            | {PInstr=IMEM ins';} as p -> 
                exec' p (Mem.execInstr d symtab) ins' ERRIMULTMEM
            | {PInstr=IMULTMEM ins';} as p -> 
                exec' p (MultMem.execInstr d) ins' ERRIMULTMEM
                |> Result.map (fun cpu -> cpu, symtab)
            | {PInstr=IARITH ins';} as p -> 
                exec' p (Arithmetic.execArithmeticInstr d) ins' ERRIARITH
                |> Result.map (fun cpu -> cpu, symtab)
        )

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
        let rec execLines lines cpu' symtab' = 
            match lines with
            // no more lines to return, so return cpu and symtab
            | [] -> Ok (cpu', symtab')
            | line :: rest ->
                execParsedLine line cpu' symtab'
                |> Result.bind (fun (newCpu, newSymTab) -> execLines rest newCpu newSymTab)
        execLines parsedLines cpuData symtab


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
