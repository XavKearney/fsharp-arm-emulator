module MultMem

    open CommonLex

    /// sample specification for set of instructions

    // change these types as required

    /// instruction (dummy: must change)
    type Instr =  {MemDummy: Unit}

    /// parse error (dummy, but will do)
    type ErrInstr = string

    /// Defines the spec for LDM/STM instructions
    /// which access multiple memory locations
    /// and store on stack
    let multMemSpec = {
        InstrC = MEM
        Roots = ["LDM";"STM"]
        // for LDM: "", "IA" and "FD" are synonyms
        // for LDM: "EA" and "DB" are synonyms
        // for STM: "", "EA" and "IA" are synonyms
        // for STM: "FD" and "DB" are synonyms
        Suffixes = [""; "EA"; "IA"; "FD"; "DB"]
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand memSpec

    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse (ls: LineData) : Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =
            // this does the real work of parsing
            // dummy return for now
            Ok { PInstr={MemDummy=()}; PLabel = None ; PSize = 4u; PCond = pCond }
        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'



    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = parse