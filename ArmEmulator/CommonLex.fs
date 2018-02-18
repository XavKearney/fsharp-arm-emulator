module CommonLex

    open CommonData
    
    /// ARM execution conditions
    type Condition =

        | Ceq
        | Cne
        | Cmi
        | Cpl
        | Chi
        | Chs
        | Clo
        | Cls
        | Cge
        | Cgt
        | Cle
        | Clt
        | Cvs
        | Cvc
        | Cnv // the "never executed" condition NV - not often used!
        | Cal // the "always executed condition "AL". Used by default on no condition

    /// classes of instructions (example, add/change this is needed)
    type InstrClass = | DP | MEM | ADR | LABEL 

    /// specification of set of instructions
    type OpSpec = {
        InstrC: InstrClass
        Roots: string list
        Suffixes: string list
    }

    type SymbolTable = Map<string,uint32>

    /// result returned from instruction-specific module parsing
    /// an instruction class. If symbol definitions are found in a 
    /// symbol table then a complete parse will be output
    /// otherwise some fields will be None
    type Parse<'INS> = {
            /// value representing instruction. NB type varies with instruction class
            PInstr: 'INS 
            /// name and value of label defined on this line, if one is.
            PLabel: (string * uint32) option 
            /// number of bytes in memory taken up by this instruction
            PSize: uint32 
            /// execution condition for instruction
            PCond: Condition
        }

    /// data given to instruction-specific parse function
    type LineData = {
        /// memory address this instruction is loaded. Must be word address
        LoadAddr: WAddr 
        /// name of label defined on this line, if one exists
        Label: string option 
        /// table of symbols with defined values. 
        /// if this is given we are phase 2 and all symbols should be defined
        /// if this is not given we are phase 1 and no symbols are defined
        SymTab: SymbolTable option
        /// opcode string
        OpCode: string
        /// string of all the operands
        Operands: string
    }


    /// Strings with corresponding execution condition
    /// Note some conditions have multiple strings
    /// Note "" is a valid condition string (always execute condition)
    let condMap = [ "EQ",Ceq ; "NE",Cne ; "MI",Cmi ; "PL",Cpl ; "HI", Chi ; 
                    "HS",Chs ; "LO",Clo ; "LS",Cls ; "GE",Cge ; "GT", Cgt ; 
                    "LE", Cle ; "LT", Clt ; "VS",Cvs ;  "VC",Cvc ;
                    "NV",Cnv ; "AL",Cal ; "",Cal; "",Cal] |> Map.ofList

    /// list of all strings representing execution conditions
    /// includes ""
    let condStrings = 
        condMap
        |> Map.toList
        |> List.map fst
        |> List.distinct    

    /// generate all possible opcode strings for given specification
    /// each string is paired with info about instruction
    /// and the three parts of the opcode
    let opCodeExpand (spec: OpSpec) 
        //    opcode    class        root    suffix   instr cond
        : Map<string, InstrClass * (string * string * Condition)> =
        spec.Roots
        |> List.collect (fun r -> 
            spec.Suffixes
            |> List.collect (fun s -> 
                condStrings
                |> List.map (fun c -> r+s+c, (spec.InstrC,(r,s, condMap.[c])))))
                |> Map.ofList

    /// function used to change PInstr field of a Result<Parse<'INS>,'E>
    /// the output has this field mapped with fMap
    /// or if Error has this value chnaged by fMapE
    let pResultInstrMap fMap fMapE paRes =
        match paRes with
        | Ok ({PInstr=ins} as pr) -> 
            // Note subtle point. {pr with Pinst = ...} will not work here
            // That is because applying fmap changes the type of PInstr
            // and therefore the type of the record.
            Ok {
            PInstr = fMap ins 
            PLabel = pr.PLabel
            PCond = pr.PCond
            PSize = pr.PSize
            }
        | Error e -> Error (fMapE e)