module Arithmetic
    open CommonData
    open CommonLex
    open System.Text.RegularExpressions

    // DUs for the instructions in Arithmetic
    type ArithInstrType = ADD | ADC | SUB | SBC | RSB | RSC
    type CompInstrType = CMP | CMN

    // DU for all shift operations
    type Operations = LSL | ASR | LSR | ROR | RRX

    // Map of all shift operations
    let operationNames = 
        Map.ofList [
            "LSL",LSL; "ASR",ASR; "LSR",LSR; "ROR",ROR; "RRX",RRX
        ]
    
    // Reverse map
    let operationStrings = 
        operationNames
        |> Map.toList
        |> List.map (fun (s,rn)-> (rn,s)) 
        |> Map.ofList

    // DU for flex op 2
    type Op2Types = Literal of uint32 | Register of RName | RegisterShift of RName * Operations * int32 | RegisterRegisterShift of RName * Operations * RName

    // Main arithmetic instruction type
    type ArithInstr = 
        {
            // Arithmetic type
            InstrType: ArithInstrType option;
            // Whether S suffix is set
            SuffixSet: bool
            // Destination register
            Target: RName;
            // First operation -> must be a register
            Op1: RName;
            // Second Operation -> Flexible operand 2
            Op2: Op2Types;
        }

    // CMP and CMN instruction type
    type CompInstr = 
        {
            InstrType: CompInstrType option;
            Op1: RName;
            Op2: Op2Types;
        }

    // Defines the spec for all the Arithmetic instructions
    let CompSpec = {
        InstrC = COMP
        Roots = ["CMP";"CMN"]
        Suffixes = [""]
    }

    let ArithSpec = {
        InstrC = ARITH
        Roots = ["ADD";"ADC";"SUB";"SBC";"RSB";"RSC"]
        Suffixes = [""; "S"]
    }

    // DU return type for parse to return the 2 possible instruction types
    type ReturnInstr = | ArithI of ArithInstr | CompI of CompInstr

    // Map of all possible opcodes
    let arithOpCodes = opCodeExpand ArithSpec
    let compOpCodes = opCodeExpand CompSpec


    // #### START - Active pattern code ####
    
    // Flexible operand 2 string parsing -> Returns group matches
    let (|FlexParse|_|) pattern input = 
        let flexMatch = Regex.Match(input, pattern)
        match flexMatch.Success with
        | true -> Some (List.tail [for strMatch in flexMatch.Groups -> strMatch.Value ])
        | false -> None

    // Matches the first character of a string and returns the rest
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            // Return string after pattern
            Some(s.Substring(p.Length))
        else
            None

    // #### END - Active pattern code ####

    
    /// FlexOp2 code that performs shifts on flexible operand 2 
    let flexOp2 op2 cpuData = 
        match op2 with
        //Literal integer
        | Literal integer -> integer
        
        // Contents of specified register
        | Register reg -> 
            match cpuData with 
            | {Fl = _ ; Regs = regmap } -> Map.find reg regmap
        
        // Register Shift
        | RegisterShift (reg, op, integer) -> 
            match op with
            | LSL _ -> 
                match cpuData with 
                | {Fl = _ ; Regs = regmap } -> 
                    if uint32 integer < 32u then Map.find reg regmap <<< integer else uint32 0
            
            | LSR _ -> 
                match cpuData with 
                | {Fl = _ ; Regs = regmap } -> 
                    if uint32 integer < 32u then Map.find reg regmap >>> integer else uint32 0

            | ASR _ -> 
                match cpuData with
                | {Fl = _ ; Regs = regmap } -> 
                    if uint32 integer < 32u then uint32(int32(Map.find reg regmap) >>> integer) else uint32 0
            
            | RRX _ -> 
                match cpuData with 
                | {Fl = flag ; Regs = regmap } -> 
                    match flag with
                    | {N = _ ; C = carry ; Z = _ ; V = _} -> 
                        let carryNum = match carry with | true -> uint32(1) | false -> uint32(0)
                        (Map.find reg regmap >>> 1) + (carryNum <<< 31)

            | ROR _ ->
                match cpuData with
                | {Fl = _ ; Regs = regmap } -> 
                    (Map.find reg regmap <<< (32 - integer)) ||| (Map.find reg regmap >>> integer)

        // Register with register shift
        | RegisterRegisterShift (reg, op, reg2) ->
            let integer = 
                match cpuData with 
                | {Fl = _ ; Regs = regmap } -> 
                    int32(Map.find reg2 regmap)

            match op with
            | LSL _ -> 
                match cpuData with 
                | {Fl = _ ; Regs = regmap } -> 
                    if uint32 integer < 32u then Map.find reg regmap <<< integer else uint32 0

            | LSR _ -> 
                match cpuData with 
                | {Fl = _ ; Regs = regmap } -> 
                    if uint32 integer < 32u then Map.find reg regmap >>> integer else uint32 0

            | ASR _ -> 
                match cpuData with
                | {Fl = _ ; Regs = regmap } -> 
                    if uint32 integer < 32u then uint32(int32(Map.find reg regmap) >>> integer) else uint32 0

            | RRX _ -> 
                match cpuData with 
                | {Fl = flag ; Regs = regmap } -> 
                    match flag with
                    | {N = _ ; C = carry ; Z = _ ; V = _} -> 
                        let carryNum = match carry with | true -> uint32(1) | false -> uint32(0)
                        (Map.find reg regmap >>> 1) + (carryNum <<< 31)

            | ROR _ ->
                match cpuData with
                | {Fl = _ ; Regs = regmap } -> 
                    (Map.find reg regmap <<< (32 - integer)) ||| (Map.find reg regmap >>> integer)

    /// Checks if input conforms to restrictions on literals
    let check32BitBound input =
        match int64 input with
        | x when x > int64 System.Int32.MaxValue -> Error ("Invalid 32 bit number")
        | x when x < int64 System.Int32.MinValue -> Error ("Invalid 32 bit number")
        | x -> 
            let rotates = [0;2;4;6;8;10;12;14;16;18;20;22;24;26;28;30]
            let valid = 
                List.map (fun a -> uint32((x <<< (32-a)) + (x >>> a))) rotates
                |> List.collect (fun b -> [b <= uint32(255)])
                |> List.contains true 
                
            match valid with
            | true -> Ok x
            | false -> Error ("Invalid 32 bit number")

    /// Recursively split an expression and evaluate
    /// Combine expression in BIDMAD format
    let recursiveSplit expression (symTable:SymbolTable option) =
        // Lift result type up through recursive function
        let lift op a b =
            match a,b with
            | Ok x, Ok y -> Ok (op x y)
            | _, _ -> Error ("Invalid 32 bit number")

        match expression with
        | FlexParse "([+*-]{2})" _ ->
            Error ("Invalid expression")
        
        | FlexParse "([+*-])$" _ ->
            Error ("Invalid expression")

        | FlexParse "^([*])" _ ->
            Error ("Invalid expression")
            
        | _ ->   
            let rec recursiveSplit' expression = 
                if String.exists (fun c -> c='+') expression then
                    expression.Split('+')
                    |> Array.map (fun s-> s.Trim()) 
                    |> Array.toList
                    |> List.map ((fun s -> if s = "" then "0" else s) >> recursiveSplit')
                    |> List.reduce (lift (+))

                elif String.exists (fun c -> c='-') expression then
                    expression.Split('-')
                    |> Array.map (fun s -> s.Trim())
                    |> Array.toList
                    |> List.map ((fun s -> if s = "" then "0" else s) >> recursiveSplit')
                    |> List.reduce (lift (-))

                elif String.exists (fun c -> c='*') expression then
                    expression.Split('*')
                    |> Array.map (fun s-> s.Trim()) 
                    |> Array.toList
                    |> List.map recursiveSplit'
                    |> List.reduce (lift (*))

                else
                    match expression with
                    // Binary string
                    | FlexParse "^(-?0b[0-1]+)$" [binStr] -> 
                        match binStr.Length with
                        | x when x > 38 -> Error ("Op2 is not a valid 32 bit number")
                        | _ -> 
                            match check32BitBound binStr with
                            | Ok binInt -> Ok (binInt)
                            | _ -> Error ("Op2 is not a valid 32 bit number")  

                    // Hex string
                    | FlexParse "^(-?0x[0-9A-F]+|-?&[0-9A-F]+)$" [hexStr] -> 
                        match hexStr.Length with
                        | x when x > 14 -> Error ("Op2 is not a valid 32 bit number")
                        | _ -> 
                            match hexStr with
                            | Prefix "&" hexOut ->
                                let finalHex = "0x" + hexOut
                                match check32BitBound finalHex with
                                | Ok hexInt -> Ok (hexInt)
                                | _ -> Error ("Invalid 32 bit number")
                            | _ ->
                                match check32BitBound hexStr with
                                | Ok hexInt -> Ok (hexInt)
                                | _ -> Error ("Invalid 32 bit number")

                    // Decimal string
                    | FlexParse "^(-?[0-9]+)$" [numStr] ->
                        match numStr.Length with
                        | x when x > 14 -> Error ("Op2 is not a valid 32 bit number")
                        | _ ->
                            match check32BitBound numStr with
                            | Ok numInt -> Ok (numInt)
                            | _ -> Error ("Invalid 32 bit number")
                    
                    // Otherwise could be symbol or otherwise invalid expression
                    | _ -> 
                        match symTable with
                        | Some symMap -> 
                            match symMap.TryFind expression with
                            | Some sym -> Ok (int64 sym)
                            | _ -> Error ("Symbol does not exist")
                        | _ -> Error ("Invalid op2 expression")

            recursiveSplit' expression

    /// Parse a line for CMP and CMN instructions
    let parseCompLine (line:string) (symTable:SymbolTable option) = 
        // List of operands (split by comma)       
        let opList = line.Split(',') |> Array.map (fun s-> s.Trim()) |> Array.toList

        match opList.Length with
        // If length 2 then there is no shift op -> must be literal or register
        | 2 ->
            let op1Str = opList.[0]
            let op2Str = opList.[1]

            match regNames.TryFind op1Str with
            | Some op1 -> 
                match op2Str with
                | Prefix "#" op2Num -> 
                    let op2 = recursiveSplit op2Num symTable
                    match op2 with
                    | Ok op2Out -> Ok (op1, Literal (uint32 op2Out))
                    | Error err -> Error (err) 
                | _ ->
                    match regNames.TryFind op2Str with
                    | Some op2 -> Ok (op1, Register op2)                 
                    | _ -> Error("Op2 is not a valid register or expression")
            
            | _ -> Error ("Op1 is not a valid register")
        
        // Should have a shift operation
        | 3 -> 
            let op1Str = opList.[0]
            let op2Str = opList.[1]
            let flexStr = opList.[2]

            match regNames.TryFind op1Str with
            | Some op1 -> 
                match regNames.TryFind op2Str with
                | Some op2 -> 
                    match flexStr with
                    | FlexParse "^([A-Z]+)\s+(.*)?$" [shiftOpStr; regLitStr] ->
                        match operationNames.TryFind shiftOpStr with
                        | Some shiftOp ->
                            match shiftOp with
                            // RRX should have no shift amount specified
                            | RRX ->
                                Error ("RRX always has a shift of 1")
                            | _ ->
                                match regLitStr.Trim() with
                                | Prefix "#" op2Num -> 
                                    let flexVal = recursiveSplit op2Num symTable
                                    match flexVal with
                                    | Ok flexOut -> Ok (op1, RegisterShift (op2, shiftOp, int32 flexOut))
                                    | Error err -> Error (err) 
                                | _ ->
                                    match regNames.TryFind regLitStr with
                                    | Some flexOut -> Ok (op1, RegisterRegisterShift (op2, shiftOp, flexOut))                 
                                    | _ -> Error ("Flex is not a valid register or expression")
                        | _ -> Error ("Shift op is invalid")
                    // Special case for RRX -> There is no shift amount specified
                    | FlexParse "^(RRX)\s*$" [shiftOpStr] ->
                        match operationNames.TryFind shiftOpStr with
                        | Some shiftOp -> 
                            let flexVal = recursiveSplit "1" symTable
                            match flexVal with
                            | Ok flexOut -> Ok (op1, RegisterShift (op2, shiftOp, int32 flexOut))
                            | Error err -> Error (err)
                        | _ -> Error ("Shfit op invalid")
                    | _ -> Error ("Flex is invalid")        
                | _ -> Error ("Op2 is an invalid register")
            | _ -> Error ("Op1 is an invalid register")
        | _ -> Error ("Invalid compare instruction")

    /// Parse string for arithmetic 
    let parseArithLine (line:string) symTable = 
        let opList = line.Split(',') |> Array.map (fun s-> s.Trim()) |> Array.toList

        match opList.Length with
        | 3 ->
            let destStr = opList.[0]
            let op1Str = opList.[1]
            let op2Str = opList.[2]

            match regNames.TryFind destStr with
            | Some dest ->
                match regNames.TryFind op1Str with
                | Some op1 ->
                    match op2Str with
                    | Prefix "#" op2Str ->
                        let op2Int = recursiveSplit op2Str symTable
                        match op2Int with
                        | Ok op2Val -> Ok (dest, op1, Literal (uint32 op2Val))
                        | Error err -> Error (err) 
                    | _ ->
                        match regNames.TryFind op2Str with
                        | Some op2Reg -> Ok (dest, op1, Register op2Reg)              
                        | _ -> Error ("Op2 is not a valid register or expression")

                | _ -> Error ("Op1 is not a valid register")

            | _ -> Error ("Destination is not a valid register")
        
        | 4 -> 
            let destStr = opList.[0]
            let op1Str = opList.[1]
            let op2Str = opList.[2]
            let flexStr = opList.[3]

            match regNames.TryFind destStr with
            | Some dest -> 
                match regNames.TryFind op1Str with
                | Some op1 -> 
                    match regNames.TryFind op2Str with
                    | Some op2 ->
                        match flexStr with
                        | FlexParse "^([A-Z]+)\s+(R[0-9]+|#-?0b[0-1]+|#-?0x[0-9A-F]+|#-?&[0-9A-F]+|#-?[0-9]+)$" [shiftOpStr;regLitStr] ->
                            match operationNames.TryFind shiftOpStr with
                            | Some shiftOp ->
                                match shiftOp with
                                | RRX ->
                                    Error ("RRX always has a shift of 1")
                                | _ ->
                                    match regLitStr.Trim() with
                                    | Prefix "#" shiftNumStr -> 
                                        let shiftInt = recursiveSplit shiftNumStr symTable
                                        match shiftInt with
                                        | Ok shiftVal -> Ok (dest, op1, RegisterShift (op2, shiftOp, int32 shiftVal))
                                        | Error err -> Error (err) 
                                    | _ ->
                                        match regNames.TryFind regLitStr with
                                        | Some shiftReg -> Ok (dest, op1, RegisterRegisterShift (op2, shiftOp, shiftReg))               
                                        | _ -> Error ("Op2 is not a valid register or expression")

                            | _ -> Error ("Invalid shift operation")
                        | FlexParse "^(RRX)\s*$" [shiftOpStr] ->
                            match operationNames.TryFind shiftOpStr with
                            | Some shiftOp -> 
                                let flexVal = recursiveSplit "1" symTable
                                match flexVal with
                                | Ok flexOut -> Ok (dest, op1, RegisterShift (op2, shiftOp, int32 flexOut))
                                | Error err -> Error (err)
                            | _ -> Error ("Shfit op invalid")
                        | _ -> Error ("Invalid flex op 2")
                    | _ -> Error ("Invalid op2 register")
                | _ -> Error ("Invalid op1 register") 
            | _ -> Error ("Invalid destination register")
        | _ -> Error ("Invalid arithmetic instruction")
        
    /// Make an arithmeric operand string from random input values
    let makeArithInstr root (suffix:string) operands symTable =
        // Makes final instruction from baseInstr after checking conditions
        let makeInstr (ins:ArithInstr) = 
            match ins.InstrType, ins.Target, ins.Op1, ins.Op2 with
            // Restrictions on target register
            | Some (ADD), R15, R13, _ | Some (ADD), R15, _, Register R13->
                Error ("Target register cannot be PC if op1 or op2 is R13")          
            | Some (ADC), R15, _, _ | Some (RSB), R15, _, _ | Some (RSC), R15, _, _ | Some (SBC), R15, _, _  ->
                Error ("Target register cannot be PC for ADC, RSB, SBC and RSC")
            | Some (ADC), R13, _, _ | Some (RSB), R13, _, _ | Some (RSC), R13, _, _ | Some (SBC), R13, _, _ ->
                Error ("Target register cannot be SP for ADC, RSB, SBC and RSC")
            | Some (SUB), R13, op1, _ when op1 <> R13 ->
                Error ("Target register can only be SP if op1 is SP")
            | Some (ADD), R13, op1, _ when op1 <> R13 ->
                Error ("Target register can only be SP if op1 is SP")
            | _ ->
                 Ok(ins)
        
        match parseArithLine operands symTable with
        | Ok (dest, op1, op2) -> 
            // Converts suffix string into bool option
            let suffType = suffix.EndsWith('S') 

            // Creates basic ArithInstr type
            let baseArithInstr = {
                InstrType = None;
                SuffixSet = suffType;
                Target = dest;
                Op1 = op1;
                Op2 = op2;
            }

            match root with
            | "ADD" -> makeInstr {baseArithInstr with InstrType = Some(ADD);}
            | "ADC" -> makeInstr {baseArithInstr with InstrType = Some(ADC);}
            | "SUB" -> makeInstr {baseArithInstr with InstrType = Some(SUB);}
            | "SBC" -> makeInstr {baseArithInstr with InstrType = Some(SBC);}
            | "RSB" -> makeInstr {baseArithInstr with InstrType = Some(RSB);}
            | "RSC" -> makeInstr {baseArithInstr with InstrType = Some(RSC);}
            | _ -> Error ("Opcode is invalid or not supported in this module")
        
        | Error err -> Error err

    /// Make comparison op string from random input values
    let makeCompInstr root operands symTable = 
        let makeInstr ins = 
            Ok(ins)

        match parseCompLine operands symTable with
        | Ok (op1, op2) -> 
            let baseCompInstr = {
                InstrType = None;
                Op1 = op1;
                Op2 = op2;
            }

            match root with
            | "CMP" -> makeInstr {baseCompInstr with InstrType = Some(CMP);}
            | "CMN" -> makeInstr {baseCompInstr with InstrType = Some(CMN);}
            | _ -> Error ("Opcode is invalid or not supported in this module")

        | Error err -> Error err




    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    let parse ls =
        let parse' (instrC, (root,suffix,pCond)) =
            let (WA la) = ls.LoadAddr
            let symTable = ls.SymTab
            match instrC with
            | ARITH -> 
                match makeArithInstr root suffix ls.Operands symTable with
                | Ok (pinstr) -> Ok {
                        PInstr = ArithI pinstr;
                        PLabel = ls.Label |> Option.map (fun lab -> lab, la); 
                        PSize = 4u; 
                        PCond = pCond;
                    }
                | Error s -> Error s
            | COMP ->
                match makeCompInstr root ls.Operands symTable with
                | Ok (pinstr) -> Ok {
                        PInstr = CompI pinstr;
                        PLabel = ls.Label |> Option.map (fun lab -> lab, la); 
                        PSize = 4u; 
                        PCond = pCond;
                    }
                | Error s -> Error s 
            | _ -> Error ("Instruction class not supported.")
        
        
        let arithInstr = Map.tryFind ls.OpCode arithOpCodes
        let compInstr = Map.tryFind ls.OpCode compOpCodes

        match arithInstr, compInstr with
        | Some (ari), None -> Some(parse' ari)
        | None, Some (com) -> Some(parse' com)
        | _ -> None
   

    /// Execute an arithmetic instruction
    /// Performs arithmetic on given cpuData
    let doArithmetic (input: Parse<ReturnInstr>) cpuData = 
        // Register map
        let regMap = cpuData.Regs

        // Sets the flags from the instructions
        // Pipe input and output all the way through to set each individual flag
        // and compound the result
        let setFlags result op1Num op2Num flags = 
            result 
            |> function
            | x when int32 x = 0 ->
                let newFlags = {flags with Z = true}
                x, newFlags
            | x ->
                let newFlags = {flags with Z = false}
                x, newFlags
            |> function
            | (x, flags) when int32 x < 0 ->
                let newFlags = {flags with N = true}
                x, newFlags
            | (x, flags) -> 
                let newFlags = {flags with N = false}
                x, newFlags
            |> function 
            | (x, flags) when (int64 (int32 op1Num + int32 op2Num)) <> (int64 (int32 op1Num) + int64 (int32 op2Num)) ->
                let newFlags = {flags with V = true}
                x, newFlags
            | (x, flags) ->
                let newFlags = {flags with V = false}
                x, newFlags
            |> function
            | (x, flags) when (uint64 op1Num + uint64 op2Num) <> (uint64 (op1Num + op2Num)) -> 
                let newFlags = {flags with C = true}                           
                x, newFlags
            | (x, flags) -> 
                let newFlags = {flags with C = false}
                x, newFlags
            |> Ok


        // Actual logic that performs the instruction
        let arithLogic opcode suffix target op1 op2 (flags:Flags)= 
            // Register target
            let targetVal = regMap.TryFind target
            let op1Val = regMap.TryFind op1
            let op2Val = 
                match op2 with
                    | Literal num -> Some num
                    | Register reg -> regMap.TryFind reg
                    | RegisterShift (op2, shift, num) -> Some (flexOp2 (RegisterShift (op2, shift, num)) cpuData)
                    | RegisterRegisterShift (op2, shift, num) -> Some (flexOp2 (RegisterRegisterShift(op2, shift, num)) cpuData)
            let carryVal = 
                match cpuData.Fl.C with
                | true -> 1u
                | false -> 0u

            // Perform the actual logic operation depending on the opcode
            let logicOp = 
                match targetVal, op1Val, op2Val with
                    | Some _, Some op1Num, Some op2Num -> 
                        match opcode with
                        | ADD -> 
                            let result = op1Num + op2Num
                            match suffix with
                            | true -> 
                                setFlags result op1Num op2Num flags
                            | false ->
                                Ok (result, flags)
                        | ADC -> 
                            let extra = op2Num + carryVal
                            let result = op1Num + extra
                            match suffix with
                            | true -> 
                               setFlags result op1Num extra flags
                            | false ->
                                Ok (result, flags)
                        | SUB -> 
                            let result = op1Num - op2Num
                            match suffix with
                            | true -> 
                                match op2Num with
                                | 0u ->
                                    setFlags result op1Num (~~~op2Num) flags
                                | _ ->
                                    setFlags result op1Num (~~~op2Num + 1u) flags
                            | false ->
                                Ok (result, flags)
                        | SBC -> 
                            let extra = 
                                match carryVal with
                                | 0u ->
                                    op2Num + 1u
                                | _ ->
                                    op2Num
                            let result = op1Num - extra
                            match suffix with
                            | true -> 
                                match extra with
                                | 0u ->
                                    setFlags result op1Num (~~~extra) flags
                                | _ ->
                                    setFlags result op1Num (~~~extra + 1u) flags
                                
                            | false ->
                                Ok (result, flags)
                        | RSB -> 
                            match int32 op2Num with
                            // visUAL restriction that op2 has to be a positive number
                            | x when x < 0 ->
                                Error ("Invalid immediate operand value")
                            | _ ->
                                let result = op2Num - op1Num
                                match suffix with
                                | true -> 
                                    match op2Num with
                                    | 0u ->
                                        setFlags result op2Num (~~~op1Num) flags
                                    | _ ->
                                        setFlags result op2Num (~~~op1Num + 1u) flags
                                | false ->
                                    Ok (result, flags)
                        | RSC -> 
                            match int32 op2Num with
                            | x when x < 0 ->
                                Error ("Invalid immediate operand value")
                            | _ ->
                                let extra = 
                                    match carryVal with
                                    | 0u ->
                                        op1Num + 1u
                                    | _ ->
                                        op1Num
                                let result = op2Num - extra
                                match suffix with
                                | true -> 
                                    match op2Num with
                                    | 0u ->
                                        setFlags result op2Num (~~~extra) flags
                                    | _ ->
                                        setFlags result op2Num (~~~extra + 1u) flags
                                | false ->
                                    Ok (result, flags)
                    | _ -> Error ("The instruction is invalid")

            match logicOp with
            | Ok regVal -> 
                let outVal, outFlags = regVal
                {Regs = Map.add target (uint32 outVal) regMap; Fl = outFlags; MM = cpuData.MM}
            | Error _ -> failwithf "The instruction is invalid"

        // Computational code for CMP and CMN
        // Has the same basic stucture as for the above function with small
        // necessary changed to reflect CompI type
        let compLogic opcode op1 op2 (flags:Flags) = 
            let op1Val = regMap.TryFind op1
            let op2Val = 
                match op2 with
                    | Literal num -> Some num
                    | Register reg -> regMap.TryFind reg
                    | RegisterShift (op2, shift, num) -> Some (flexOp2 (RegisterShift (op2, shift, num)) cpuData)
                    | RegisterRegisterShift (op2, shift, num) -> Some (flexOp2 (RegisterRegisterShift(op2, shift, num)) cpuData)
            
            let logicOp = 
                match op1Val, op2Val with
                    | Some op1Num, Some op2Num -> 
                        match opcode with
                        | CMP -> 
                            let result = op1Num - op2Num
                            match op2Num with
                            | 0u ->
                                setFlags result op1Num (~~~op2Num) flags
                            | _ ->
                                setFlags result op1Num (~~~op2Num + 1u) flags
                        | CMN -> 
                            let result = op1Num + op2Num
                            setFlags result op1Num op2Num flags

                    | _ -> Error ("The instruction is invalid")
            
            match logicOp with
            | Ok regVal -> 
                let _, outFlags = regVal
                {Regs = regMap; Fl = outFlags; MM = cpuData.MM}
            | Error _ -> failwithf "The instruction is invalid"

        

        let instr = input.PInstr

        // Get instruction from cpuData and perform the corresponding 
        match instr with
        | ArithI instr ->
            let arithInstr = instr.InstrType
            let suffix = instr.SuffixSet
            let target = instr.Target
            let op1 = instr.Op1
            let op2 = instr.Op2
            let flags = cpuData.Fl

            match arithInstr with
            | Some(ins) -> arithLogic ins suffix target op1 op2 flags
            | None -> failwithf "No instruction specified"
        
        | CompI instr ->
            let compInstr = instr.InstrType
            let op1 = instr.Op1
            let op2 = instr.Op2
            let flags = cpuData.Fl

            match compInstr with
            | Some (ins) -> compLogic ins op1 op2 flags
            | None -> failwithf "No instruction specified"


    let (|IMatch|_|) = parse


    


