/// handles LDM/STM instructions
/// author: Xav Kearney
module MultMem
    
    open CommonLex
    open CommonData

    /// sample specification for set of instructions

    // change these types as required

    type multIns = LDM | STM
    type multDirection = FD | FA | ED | EA
    
    /// multiple memory access instruction type
    type multMemInstr = 
        {
            // load or store
            InsType: multIns;
            // stack direction
            Direction: multDirection;
            // target register: source/destination
            Target: RName;
            // optional writeback suffix '!'
            // writes the address of the final mem location back to target reg
            WriteBack: bool;
            // list of registers to store/load to/from
            RegList: RName list;
        }

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
        Suffixes = [""; "EA"; "ED"; "IA"; "FA"; "FD"; "DB"]
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand multMemSpec

    let makeLDM suf cond =



    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse (ls: LineData) : Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =
            match instrC, root with
            | MEM, LDM -> makeLDM suffix pCond
            | MEM, STM -> makeSTM suffic pCond
            | _ -> Error ("%s, %s not supported" instrC, root)
        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'

    // Ok { 
    //     // Normal (non-error) return from result monad
    //     // This is the instruction determined from opcode, suffix and parsing
    //     // the operands. Not done in the sample.
    //     // Note the record type returned must be written by the module author.
    //     PInstr={DPDummy=()}; 


    //     // This is normally the line label as contained in
    //     // ls together with the label's value which is normally
    //     // ls.LoadAddr. Some type conversion is needed since the
    //     // label value is a number and not necessarily a word address
    //     // it does not have to be div by 4, though it usually is
    //     PLabel = ls.Label |> Option.map (fun lab -> lab, la) ; 


    //     // this is the number of bytes taken by the instruction
    //     // word loaded into memory. For arm instructions it is always 4 bytes. 
    //     // For data definition DCD etc it is variable.
    //     //  For EQU (which does not affect memory) it is 0
    //     PSize = 4u; 

    //     // the instruction condition is detected in the opcode and opCodeExpand                 
    //     // has already calculated condition already in the opcode map.
    //     // this part never changes
    //     PCond = pCond 
    //     }

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = parse