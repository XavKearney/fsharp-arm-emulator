module Arithmetic
    open CommonData
    open CommonLex
    open System.Text.RegularExpressions
    open System.Linq.Expressions
    open VisualTest
    open VisualTest
    open Microsoft.VisualBasic.CompilerServices
    open System.Threading

    type ArithInstrType = ADD | ADC | SUB | SBC | RSB | RSC

    type CompInstrType = CMP | CMN

    type OpCode = Target of RName | Value of uint32

    type Operations = LSL | ASR | LSR | ROR | RRX

    let operationNames = 
        Map.ofList [
            "LSL",LSL; "ASR",ASR; "LSR",LSR; "ROR",ROR; "RRX",RRX
        ]

    let operationStrings = 
        operationNames
        |> Map.toList
        |> List.map (fun (s,rn)-> (rn,s)) 
        |> Map.ofList

    type Op2Types = Literal of uint32 | Register of RName | RegisterShift of RName * Operations * int32 | RegisterRegisterShift of RName * Operations * RName

    type ParseReturn = Arith of RName * RName * Op2Types | Compare of RName * Op2Types

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

    type CompInstr = 
        {
            InstrType: CompInstrType option;
            Op1: RName;
            Op2: Op2Types;
        }

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

    type ReturnInstr = | ArithI of ArithInstr | CompI of CompInstr

    let arithOpCodes = opCodeExpand ArithSpec
    let compOpCodes = opCodeExpand CompSpec


    // #### START - Active pattern code ####
    
    // Flexible operand 2 string parsing
    let (|FlexParse|_|) pattern input = 
        let flexMatch = Regex.Match(input, pattern)
        match flexMatch.Success with
        | true -> Some (List.tail [for strMatch in flexMatch.Groups -> strMatch.Value ])
        | false -> None

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
        
        | RegisterShift (reg, op, integer) -> 
            // Register shift with various possible operations
            match op with
            // Logical shift left
            | LSL _ -> 
                match cpuData with 
                | {Fl = _ ; Regs = regmap } -> Map.find reg regmap <<< integer
            
            // Logical shift right
            | LSR _ -> 
                match cpuData with 
                | {Fl = _ ; Regs = regmap } -> Map.find reg regmap >>> integer

            // Arithmetic Shift Right
            | ASR _ -> 
                match cpuData with
                | {Fl = _ ; Regs = regmap } -> uint32(int32(Map.find reg regmap) >>> integer)
            
            // RRX puts 1 in MSB if C is true
            | RRX _ -> 
                match cpuData with 
                | {Fl = flag ; Regs = regmap } -> 
                    match flag with
                    | {N = _ ; C = carry ; Z = _ ; V = _} -> 
                        let carryNum = match carry with | true -> uint32(1) | false -> uint32(0)
                        (Map.find reg regmap >>> 1) + (carryNum <<< 31)
                
            // Rotate right
            | ROR _ ->
                match cpuData with
                | {Fl = _ ; Regs = regmap } -> (Map.find reg regmap <<< (32 - integer)) + (Map.find reg regmap >>> integer)

        
        | RegisterRegisterShift (reg, op, reg2) ->
            let integer = match cpuData with 
                          | {Fl = _ ; Regs = regmap } -> int32(Map.find reg2 regmap &&& uint32(31))

            // Register shift with various possible operations
            match op with
            // Logical shift left
            | LSL _ -> 
                match cpuData with 
                | {Fl = _ ; Regs = regmap } -> Map.find reg regmap <<< integer
            
            // Logical shift right
            | LSR _ -> 
                match cpuData with 
                | {Fl = _ ; Regs = regmap } -> Map.find reg regmap >>> integer

            // Arithmetic Shift Right
            | ASR _ -> 
                match cpuData with
                | {Fl = _ ; Regs = regmap } -> uint32(int32(Map.find reg regmap) >>> integer)
            
            // RRX puts 1 in MSB if C is true
            | RRX _ -> 
                match cpuData with 
                | {Fl = flag ; Regs = regmap } -> 
                    match flag with
                    | {N = _ ; C = carry ; Z = _ ; V = _} -> 
                        let carryNum = match carry with | true -> uint32(1) | false -> uint32(0)
                        (Map.find reg regmap >>> 1) + (carryNum <<< 31)
                
            // Rotate right
            | ROR _ ->
                match cpuData with
                | {Fl = _ ; Regs = regmap } -> (Map.find reg regmap <<< (32 - integer)) + (Map.find reg regmap >>> integer)


    let check32BitBound input =
        match int64 input with
        | x when x > int64 System.Int32.MaxValue -> Error ("Invalid 32 bit number")
        | x when x < int64 System.Int32.MinValue -> Error ("Invalid 32 bit number")
        | x -> Ok x

    let recursiveSplit expression =
        let lift op a b =
            match a,b with
            | Ok x, Ok y -> Ok (op x y)
            | _, _ -> Error ("Invalid 32 bit number")
        
        let rec recursiveSplit' expression = 
            if String.exists (fun c -> c='+') expression then
                expression.Split('+')
                |> Array.map (fun s-> s.Trim()) 
                |> Array.toList
                |> List.map recursiveSplit'
                |> List.reduce (lift (+))

            elif String.exists (fun c -> c='-') expression then
                expression.Split('-')
                |> Array.map (fun s-> s.Trim()) 
                |> Array.toList
                |> List.map recursiveSplit'
                |> List.reduce (lift (-))

            elif String.exists (fun c -> c='-') expression then
                expression.Split('*')
                |> Array.map (fun s-> s.Trim()) 
                |> Array.toList
                |> List.map recursiveSplit'
                |> List.reduce (lift (*))

            else
                match expression with
                | FlexParse "^(-?0b[0-1]+)$" [binStr] -> 
                    match binStr.Length with
                    | x when x > 38 -> Error ("Op2 is not a valid 32 bit number")
                    | _ -> 
                        match check32BitBound binStr with
                        | Ok binInt -> Ok (binInt)
                        | _ -> Error ("Op2 is not a valid 32 bit number")  

                | FlexParse "^(-?0x[0-9A-F]+)$" [hexStr] -> 
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

                | FlexParse "^(-?[0-9]+)$" [numStr] ->
                    match numStr.Length with
                    | x when x > 14 -> Error ("Op2 is not a valid 32 bit number")
                    | _ ->
                        match check32BitBound numStr with
                        | Ok numInt -> Ok (numInt)
                        | _ -> Error ("Invalid 32 bit number")
                | _ -> Error ("Invalid op2 expression")

        recursiveSplit' expression


    let parseOpsLine (line:string) = 
        
        let opList = line.Split(',') |> Array.map (fun s-> s.Trim()) |> Array.toList

        match opList.Length with
        | 2 ->
            let op1Str = opList.[0]
            let op2Str = opList.[1]

            match regNames.TryFind op1Str with
            | Some op1 -> 
                match op2Str with
                | Prefix "#" op2Num -> 
                    let op2 = recursiveSplit op2Num
                    match op2 with
                    | Ok op2Out -> Ok (Compare (op1, Literal (uint32 op2Out)))
                    | Error err -> Error (err) 
                | _ ->
                    match regNames.TryFind op2Str with
                    | Some op2 -> Ok (Compare (op1, Register op2))                  
                    | _ -> Error("Op2 is not a valid register or expression")
            
            | _ -> Error ("Op1 is not a valid register")
        
        | 3 -> 
            let destOrOp1Str = opList.[0]
            let op1OrOp2Str = opList.[1]
            let op2OrFlexStr = opList.[2]

            match regNames.TryFind destOrOp1Str with
            | Some destOrOp1 -> 
                match regNames.TryFind op1OrOp2Str with
                | Some op1OrOp2 -> 
                    match op2OrFlexStr with
                    | FlexParse "^([A-Z]+)\s+(R[0-9]+|#-?0b[0-1]+|#-?0x[0-9A-F]+|#-?&[0-9A-F]+|#-?[0-9]+)$" [shiftOpStr;regLitStr] ->
                        match operationNames.TryFind shiftOpStr with
                        | Some shiftOp ->
                            match regLitStr with
                            | Prefix "#" op2Num -> 
                                let op2 = recursiveSplit op2Num
                                match op2 with
                                | Ok op2Out -> Ok (Compare (destOrOp1, RegisterShift (op1OrOp2, shiftOp, int32 op2Out)))
                                | Error err -> Error (err) 
                            | _ ->
                                match regNames.TryFind regLitStr with
                                | Some op2Out -> Ok (Compare (destOrOp1, RegisterRegisterShift (op1OrOp2, shiftOp, op2Out)))                 
                                | _ -> Error ("Op2 is not a valid register or expression")

                        | _ -> Error ("Shift op is invalid")
                    | _ ->
                        match op2OrFlexStr with
                        | Prefix "#" op2Num -> 
                            let op2 = recursiveSplit op2Num
                            match op2 with
                            | Ok op2Out -> Ok (Arith (destOrOp1, op1OrOp2, Literal (uint32 op2Out)))
                            | Error err -> Error (err) 
                        | _ ->
                            match regNames.TryFind op2OrFlexStr with
                            | Some op2Out -> Ok (Arith (destOrOp1, op1OrOp2, Register op2Out))                
                            | _ -> Error("Op2 is not a valid register or expression")
                | _ -> Error ("Op1 or op2 is an invalid register")
            | _ -> Error ("Op1 or destination is an invalid register")


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
                                match regLitStr with
                                | Prefix "#" shiftNumStr -> 
                                    let shiftInt = recursiveSplit shiftNumStr
                                    match shiftInt with
                                    | Ok shiftVal -> Ok (Arith (dest, op1, RegisterShift (op2, shiftOp, int32 shiftVal)))
                                    | Error err -> Error (err) 
                                | _ ->
                                    match regNames.TryFind regLitStr with
                                    | Some shiftReg -> Ok (Arith (dest, op1, RegisterRegisterShift (op2, shiftOp, shiftReg)))                 
                                    | _ -> Error ("Op2 is not a valid register or expression")

                            | _ -> Error ("Invalid shift operation")
                        | _ -> Error ("Invalid flex op 2")
                    | _ -> Error ("Invalid op2 register")
                | _ -> Error ("Invalid op1 register") 
            | _ -> Error ("Invalid destination register")
        | _ -> Error ("Not setup yet")
        

    let makeInstr root (suffix:string) operands =
        // Makes final instruction from baseInstr
        let makeInstr ins = 
            ins
        
        match parseOpsLine operands with
        | Ok (Arith (dest, op1, op2)) -> 
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
            | "ADD" -> Ok (ArithI (makeInstr {baseArithInstr with InstrType = Some(ADD);}))
            | "ADC" -> Ok (ArithI (makeInstr {baseArithInstr with InstrType = Some(ADC);}))
            | "SUB" -> Ok (ArithI (makeInstr {baseArithInstr with InstrType = Some(SUB);}))
            | "SBC" -> Ok (ArithI (makeInstr {baseArithInstr with InstrType = Some(SBC);}))
            | "RSB" -> Ok (ArithI (makeInstr {baseArithInstr with InstrType = Some(RSB);}))
            | "RSC" -> Ok (ArithI (makeInstr {baseArithInstr with InstrType = Some(RSC);}))
            | _ -> Error ("Opcode is invalid or not supported in this module")

        | Ok (Compare(op1, op2)) -> 
            let baseCompInstr = {
                InstrType = None;
                Op1 = op1;
                Op2 = op2;
            }

            match root with
            | "CMP" -> Ok (CompI (makeInstr {baseCompInstr with InstrType = Some(CMP);}))
            | "CMN" -> Ok (CompI (makeInstr {baseCompInstr with InstrType = Some(CMN);}))
            | _ -> Error ("Opcode is invalid or not supported in this module")
        
        | Error err -> Error err




    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse ls =
        let parse' (instrC, (root,suffix,pCond)) =
            let (WA la) = ls.LoadAddr
            match instrC with
            | ARITH -> 
                match makeInstr root suffix ls.Operands with
                | Ok (ArithI pinstr) -> 
                    Ok {
                        PInstr = ArithI pinstr;
                        PLabel = ls.Label |> Option.map (fun lab -> lab, la); 
                        PSize = 4u; 
                        PCond = pCond;
                    }
                | Ok (CompI pinstr) ->
                    Ok {
                        PInstr = CompI pinstr;
                        PLabel = ls.Label |> Option.map (fun lab -> lab, la); 
                        PSize = 4u; 
                        PCond = pCond;
                    }          
                | Error s -> Error s
            | _ -> Error ("Instruction class not supported.")
        Map.tryFind ls.OpCode arithOpCodes
        |> Option.map parse'
   

    /// Execute an arithmetic instruction
    /// Performs arithmetic on given cpuData
    let doArithmetic (input: Parse<ReturnInstr>) cpuData = 
        // Register map
        let regMap = cpuData.Regs

        // Actual logic that performs the instruction
        let arithLogic opcode target op1 op2 = 
            // Register target
            let targetVal = regMap.TryFind target
            let op1Val = regMap.TryFind op1
            let op2Val = 
                match op2 with
                    | Literal num -> Some num
                    | Register reg -> regMap.TryFind reg
                    | RegisterShift (op2, shift, num) -> Some (flexOp2 (RegisterShift (op2, shift, num)) cpuData)
                    | RegisterRegisterShift (op2, shift, num) -> Some (flexOp2 (RegisterRegisterShift(op2, shift, num)) cpuData)
            let carryVal = match cpuData.Fl.C with
                           | true -> 1u
                           | false -> 0u

            let logicOp = 
                match targetVal, op1Val, op2Val with
                    | Some _, Some op1Num, Some op2Num -> 
                        match opcode with
                        | ADD -> Ok (op1Num + op2Num)
                        | ADC -> Ok (op1Num + op2Num + carryVal)
                        | SUB -> Ok (op1Num - op2Num)
                        | SBC -> Ok (op1Num - op2Num + (carryVal - 1u))
                        | RSB -> Ok (op2Num - op1Num)
                        | RSC -> Ok (op2Num - op1Num + (carryVal - 1u))
                    | _ -> Error ("The instruction is invalid")

            match logicOp with
            | Ok regVal -> Map.add target regVal regMap
            | Error _ -> failwithf "The instruction is invalid"

        let instr = input.PInstr

        match instr with
        | ArithI instr ->
            let arithInstr = instr.InstrType
            let suffix = instr.SuffixSet
            let target = instr.Target
            let op1 = instr.Op1
            let op2 = instr.Op2

            match arithInstr with
            | Some(ins) -> arithLogic ins target op1 op2
            | None -> failwithf "No instruction specified"
        | _ -> 
            failwithf "Not completed comp instruction"


    let (|IMatch|_|) = parse


    


