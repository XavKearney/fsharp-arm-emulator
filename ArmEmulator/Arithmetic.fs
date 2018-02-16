module Arithmetic
    open CommonData
    open CommonLex
    open System.Text.RegularExpressions

    type ArithInstrType = ADD | ADC | SUB | SBC | RSB | RSC

    type OpCode = Target of RName | Value of uint32

    type Operations = LSL | ASR | LSR | ROR | RRX

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
            | true -> Some "hello"
            | false -> None

        // Get list of operands/destination register
        let operandsList = line.Split(",")
                           |> Array.map (fun str -> str.Trim())


        match operandsList.Length with
        | 3 | 4 ->
            // Destination register string
            let destinationStr = operandsList.[0]
            // Op1 string
            let op1Str = operandsList.[1]

            // Op2 string
            let op2Str = 
                match operandsList.Length with
                | 3 -> operandsList.[2]
                | _ -> String.concat "," operandsList.[2..]

            // Partial active pattern match to see flex op 2 type - pattern match of '#'
            let (|Prefix|_|) (p:string) (s:string) =
                if s.StartsWith(p) then
                    // Return string after #
                    Some(s.Substring(1))
                else
                    None
           
            match regNames.TryFind destinationStr with
            | Some dest -> match regNames.TryFind op1Str with
                           | Some op1 -> match op2Str with
                                         | Prefix "#" op2 ->
                                            // Convert string to int for negative numbers and then force uint type
                                            // Fail if the input number is not valid
                                            try Ok (dest, op1, Value (uint32 (int32 op2))) with
                                            | _ -> Error ("Invalid 32 bit number")
                 
                                         | _ -> match regNames.TryFind op2Str with
                                                | Some op2 -> Ok (dest, op1, Target op2)
                                                | None -> Error ("Op2 is not a valid register")                                 
                           | None -> Error ("Op1 is not a valid register")
            | None -> Error ("Destination register not valid")
        
        | _ -> Error ("Operand list is invalid")

        
        

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
            let op2Val = match op2 with
                         | Target reg -> regMap.TryFind reg
                         | Value num -> Some num
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


    


