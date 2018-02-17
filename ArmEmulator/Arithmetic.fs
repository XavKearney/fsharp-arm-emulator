module Arithmetic
    open CommonData
    open CommonLex
    open System.Text.RegularExpressions

    type ArithInstrType = ADD | ADC | SUB | SBC | RSB | RSC

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

    let ArithSpec = {
        InstrC = ARITH
        Roots = ["ADD";"ADC";"SUB";"SBC";"RSB";"RSC"]
        Suffixes = [""; "S"]
    }

    let opCodes = opCodeExpand ArithSpec

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




    let parseOpsLine (line:string) = 

        // Flexible operand 2 string parsing
        let (|FlexParse|_|) pattern input = 
            let flexMatch = Regex.Match(input, pattern)
            match flexMatch.Success with
            | true -> Some (List.tail [for strMatch in flexMatch.Groups -> strMatch.Value ])
            | false -> None

        let (|Prefix|_|) (p:string) (s:string) =
            if s.StartsWith(p) then
                // Return string after #
                Some(s.Substring(1))
            else
                None


        match line with
        // Regex match captures target, op1 and then everything following the last comma
        | FlexParse "(R[0-9]+),\s*(R[0-9]+),\s*(.*)" [targetStr; op1Str; op2] ->
            match regNames.TryFind targetStr with
            | Some target -> 
                match regNames.TryFind op1Str with
                | Some op1 -> 
                    let op2List = op2.Split(",")
                    match op2List.Length with
                    | 1 -> 
                        match op2 with
                        | FlexParse "(R[0-9]+|#-?0b[0-1]+|#-?0x[0-9A-F]+|#-?[0-9]+)$" [op2Val] -> 
                            match op2Val with
                            | Prefix "#" op2Num -> Ok (target, op1, Literal (uint32 (int32 op2Num)))
                            | _ -> 
                                match regNames.TryFind op2Val with
                                | Some op2Reg -> Ok (target, op1, Register op2Reg)
                                | None -> Error ("Op2 is not a valid register")

                        | _ -> Error ("Flex op 2 has invalid format")
                    | 2 ->
                        match op2 with
                        | FlexParse "(R[0-9]+),\s*([A-Z]+)\s+(R[0-9]+|#-?0b[0-1]+|#-?0x[0-9A-F]+|#-?[0-9]+)$" [op2Val; shift; shiftVal] ->
                            match regNames.TryFind op2Val with
                            | Some op2Reg -> 
                                match operationNames.TryFind shift with
                                | Some shiftOp -> 
                                    match shiftVal with
                                    | Prefix "#" shiftNum -> Ok (target, op1, RegisterShift(op2Reg, shiftOp, int32 shiftNum))
                                    | _ -> 
                                        match regNames.TryFind shiftVal with
                                        | Some shiftReg -> Ok (target, op1, RegisterRegisterShift(op2Reg, shiftOp, shiftReg))
                                        | None -> Error ("Shift op register is invalid")

                                | None -> Error ("Shift operation is invalid")
                            | None -> Error ("Op2 is not a valid register")
                        | _ -> Error ("Flex op 2 has invalid format")
                    | _ -> Error ("Flex op 2 has invalid format")
                | _ -> Error ("Op1 register is invalid")
            | _ -> Error ("Target register is invalid") 
        | _ -> Error ("The instruction is invalid")

        
        

    let makeArithInstr root (suffix:string) operands =
        // Makes final instruction from baseInstr
        let makeInstr ins = 
            Ok(ins)
        
        match parseOpsLine operands with
        | Ok (dest, op1, op2) -> 
            // Converts suffix string into bool option
            let suffType = suffix.EndsWith('S') 

            // Creates basic ArithInstr type
            let baseInstr = {
                InstrType = None;
                SuffixSet = suffType;
                Target = dest;
                Op1 = op1;
                Op2 = op2;
            }

            match root with
            | "ADD" -> makeInstr {baseInstr with InstrType = Some(ADD);}
            | "ADC" -> makeInstr {baseInstr with InstrType = Some(ADC);}
            | "SUB" -> makeInstr {baseInstr with InstrType = Some(SUB);}
            | "SBC" -> makeInstr {baseInstr with InstrType = Some(SBC);}
            | "RSB" -> makeInstr {baseInstr with InstrType = Some(RSB);}
            | "RSC" -> makeInstr {baseInstr with InstrType = Some(RSC);}
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
                match makeArithInstr root suffix ls.Operands with
                | Ok pinstr -> Ok {
                        PInstr = pinstr;
                        PLabel = ls.Label |> Option.map (fun lab -> lab, la); 
                        PSize = 4u; 
                        PCond = pCond;
                    }
                | Error s -> Error s
            | _ -> Error ("Instruction class not supported.")
        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'
   

    /// Execute an arithmetic instruction
    /// Performs arithmetic on given cpuData
    let doArithmetic input cpuData = 
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
        let arithInstr = instr.InstrType
        let suffix = instr.SuffixSet
        let target = instr.Target
        let op1 = instr.Op1
        let op2 = instr.Op2

        match arithInstr with
        | Some(ins) -> arithLogic ins target op1 op2
        | None -> failwithf "No instruction specified"


    let (|IMatch|_|) = parse


    


