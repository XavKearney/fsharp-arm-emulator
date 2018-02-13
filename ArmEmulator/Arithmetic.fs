module Arithmetic
    open CommonData
    open CommonLex

    type ArithInstrType = ADD | ADC | SUB | SBC | RSB | RSC

    type OpCode = Target of RName | Value of uint32

    type ArithInstr = 
        {
            // Arithmetic type
            InstrType: ArithInstrType option;
            // Whether S suffix is set
            SuffixSet: bool option
            // Destination register
            Target: RName;
            // First operation -> must be a register
            Op1: RName;
            // Second Operation -> Can be both register or value
            Op2: OpCode;
        }

    let ArithSpec = {
        InstrC = ARITH
        Roots = ["ADD";"ADC";"SUB";"SBC";"RSB";"RSC"]
        Suffixes = [""; "S"]
    }

    let opCodes = opCodeExpand ArithSpec


    let parseOpsLine (line:string) = 
        // Get list of operands/destination register
        let operandsList = line.Split(",")
                           |> Array.map (fun str -> str.Trim())

        match operandsList.Length with
        | 3 ->
            // Destination register string
            let destinationStr = operandsList.[0]
            // Op1 string
            let op1Str = operandsList.[1]
            // Op2 string
            let op2Str = operandsList.[2]

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

        
        

    let makeArithInstr root suffix operands =
        // Makes final instruction from baseInstr
        let makeInstr ins = 
            Ok(ins)
        
        match parseOpsLine operands with
        | Ok (dest, op1, op2) -> 
            // Converts suffix string into bool option
            let suffType = match suffix with
                           | "S" -> Some(true)
                           | "" -> Some(false)
                           | _ -> None

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
    let parse ls=
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
   
    let (|IMatch|_|) = parse


    


