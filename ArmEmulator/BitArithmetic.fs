// lex/parse and execute a given subset of instructions
// subset is some data processing instructions
module BitArithmetic



    open CommonLex
    open CommonData
    open System.Text.RegularExpressions




// Types //




    /// instructions
    type InstRoots =  MOV | MVN | AND | ORR | EOR | BIC | LSL | LSR | ASR
                    | ROR | RRX | TST | TEQ 

    /// parse error
    type ErrInstr = string

    type LitOrReg = Nm of uint32 | Rg of RName

    /// Flexible opperator, can either be a number or a register with an optional shift
    type FlexOp = 
        | Num of uint32
        | RegShiftOp of RName*(InstRoots*LitOrReg Option) Option

    type RegOrFlexOp =
        | Reg of RName option
        | Flex of FlexOp option

    /// Infromation needed for instruction execution
    /// Part of the return from parse
    type InstDecomp = { instruction: InstRoots
                        suff: string
                        opA: RName Option
                        opB: RegOrFlexOp Option
                        opC: RegOrFlexOp Option
                        }




// Maps




    /// map of all possible opcodes recognised
    let opCodes = 
        /// sample specification for set of instructions
        let dPSpec = {
            InstrC = BITARITH
            Roots = ["MOV" ; "MVN" ; "AND" ; "ORR" ; "EOR" ; "BIC" ;
                    "LSL" ; "LSR" ; "ASR" ; "ROR" ; "RRX" ; "TST" ; "TEQ"]

            Suffixes = [""; "S"]
        }
        opCodeExpand dPSpec

    /// map used to convert strings into instruction values 
    let instrNames = 
        Map.ofList [ 
            "MOV",MOV ; "MVN",MVN ; "AND",AND ; "ORR",ORR ; "BIC",BIC ; "LSL",LSL
            "LSR",LSR ; "ASR",ASR ; "ROR",ROR ; "RRX",RRX ; "TST",TST ; "TEQ",TEQ ]

    /// Map of allowed shifts
    let allowedShifts = 
        Map.ofList ["LSL",LSL ; "ASR",ASR ; "LSE",LSR ; "ROR",ROR ; "RRX",RRX]




// check litteral 
// need to include expressions




    let (|FirstMatch|_|) pattern input =
        let m = Regex.Match(input,pattern)
        match m.Success with
        | true -> Some (m.Value)
        | false -> None

    let (|CheckLit|_|) input = 
        match input with
        | FirstMatch @"^(-?0[xX][a-fA-F0-9]+)$" x -> Some (int x)
        | FirstMatch @"^(-?&[a-fA-F0-9]+)$" x -> Some (int ("0x"+x.[1..])) 
        | FirstMatch @"^(-?0[bB][0-1]+)$" x -> Some (int x)
        | FirstMatch @"^(-?[0-9]+)$" x -> Some (int x)
        | _ -> None

    /// checks if an integer can be created by rotating an 8 bit number in a 32 bit word 
    let allowedLiterals num =
        let valid =
            [0..2..30] 
            |> List.allPairs [(uint32 num)] 
            |> List.map (fun (n,r) -> (n >>> r) ||| (n <<< (32-r)))
            |> List.collect (fun n -> [n < 256u])
            |> List.contains true
        match int64 num with
        | x when ((x < 2147483647L) && (x > -2147483648L)) ->
            match valid with 
            | true -> Some (uint32 num)
            | false -> None
        | _ -> None

    // need to deal with hex and bin numbers
    /// converts string to some litteral or none
    /// string number must start with #
    let toLit (str : string) =   
        match str.[0] with
        | '#' -> 
            match str.[1..] with
            | CheckLit n -> 
                match n >= 0 with
                | true -> allowedLiterals n
                | false ->
                    match allowedLiterals (-n) with
                    | Some _ -> Some (uint32 n)
                    | _ -> None
            | _ -> None
        | _ -> None





// parse instruction




    /// returns the instruction line parsed into its seprate components given 
    /// the root, operands and suffix 
    let parseInstr (root : string) (operands : string) (suffix : string) =

        let ops = operands.Split(',') 
                    |> Array.map (fun str -> str.Trim())
                    |> Array.filter (fun str -> str <> "")
            
        /// converts string to some valid register or none
        let toReg str = Map.tryFind str regNames

        /// converts string to some shift with shift value or none
        /// if RRX then shift value is none
        let toShift (opp : string) =
            let shiftSplit = opp.Split(' ')
                            |> Array.map (fun str -> str.Trim())
                            |> Array.filter (fun str -> str <> "")
            match shiftSplit.Length with
            | 1 -> 
                let cShift = Map.tryFind shiftSplit.[0] allowedShifts
                match cShift with
                | Some shift when shift=RRX -> Some (shift,None)
                | _ -> None 
            | 2 ->
                let cShift = Map.tryFind shiftSplit.[0] allowedShifts
                let cReg = Map.tryFind shiftSplit.[1] regNames
                let cLit = toLit shiftSplit.[1]
                match cShift,cReg,cLit with 
                | Some shift,_,_ when shift = RRX -> None
                | Some shift, Some r, None -> Some (shift,Some (Rg r))
                | Some shift,None,Some lit -> Some (shift,Some (Nm lit))
                | _ -> None
            | _ -> None

        /// converts string to a valid literal or register or is none
        let toLitReg str =                                  
            let checkLit = toLit str
            let checkReg = toReg str
            match checkLit,checkReg with
            | Some lit, None -> Some (Nm lit)
            | None, Some reg -> Some (Rg reg)
            | _ -> None

        /// converts array of strings to a flexible opperator (literal or register with optional shift)
        let toFlexOp (strArr : string array) = 
            match strArr with
            | [|reg ; shift|] -> 
                match toReg reg, toShift shift with
                | Some r, Some s -> Some (RegShiftOp (r,Some s))
                | _ -> None
            | [|regOrLit|] ->
                match toLitReg regOrLit with
                | Some (Nm lit) -> Some (Num lit)
                | Some (Rg r) -> Some (RegShiftOp (r,None))
                | _ -> None
            | _ -> None

        let baseInstr = { instruction = instrNames.[root]
                          suff = suffix
                          opA = None
                          opB = None
                          opC = None
                        }
            
        // need to deal with literal followed by shift
        match instrNames.[root] with 
        | MOV | MVN | TST | TEQ when (ops.Length = 2) || (ops.Length = 3)
            -> Ok {baseInstr with opA = toReg ops.[0] ; opB = Some (Flex (toFlexOp ops.[1..]))}

        | AND | ORR | EOR | BIC when (ops.Length = 3) || (ops.Length = 4) 
            -> Ok {baseInstr with opA = toReg ops.[0] ; opB = Some (Reg (toReg ops.[1])) ; opC = Some (Flex (toFlexOp ops.[2..]))}

        | LSL | LSR | ASR | ROR when ops.Length = 2
            -> Ok {baseInstr with opA = toReg ops.[0] ; opB = Some (Reg (toReg ops.[1])) ; opC = Some (Reg (toReg ops.[2]))}

        | RRX when ops.Length = 2
            -> Ok {baseInstr with opA = toReg ops.[0] ; opB = Some(Reg (toReg ops.[1]))}

        | _ -> Error "Should not happen"



    // need to deal with upper and lower case
    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse (ls: LineData) =
        let parse' (instrC, (root,suffix,pCond)) =
            let pLab = 
                match ls.Label,ls.LoadAddr with
                | Some lab, WA addr -> Some (lab,addr)
                | _ -> None 
            match Map.containsKey root instrNames with
            | true -> 
                match parseInstr root ls.Operands suffix with 
                | Ok pInst -> Ok { PInstr=pInst; PLabel = pLab; PSize = 4u; PCond = pCond }
                | _ -> Error "Parse Error"
            | false -> Error "Parse Error"
        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse