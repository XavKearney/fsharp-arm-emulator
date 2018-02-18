module MultMemTests
    open CommonData
    open CommonLex
    open Arithmetic
    open Expecto
    open FsCheck
    open VisualTest.Visual
    open VisualTest.VTest
    open VisualTest.VCommon

    /// take a function f, test name
    /// and list of (input, output) tuples
    /// create an Expecto testList
    /// with unit tests, testing each case
    let makeTestList testFun name testVals =
        let makeTest inp outp =
            let testName = (sprintf "%s: %A" name inp)
            testCase testName <| fun () ->
                Expect.equal (testFun inp) outp testName
        List.map (fun (input, output) -> makeTest input output) testVals
        |> testList (sprintf "%s Test List" name) 


    [<Tests>]
    let testParseOpsLine = 
        makeTestList parseOpsLine "parseOpsLine Unit Tests" 
            [
                ("R7,R3,R9", Ok (R7, R3, Register R9));
                ("R0, R12, R1", Ok (R0, R12, Register R1));
                ("R,R3,R9", Error "The instruction is invalid");
                ("R7, R20, R9", Error "Op1 register is invalid");
                ("R7, R3, R20", Error "Op2 is not a valid register");
                ("R7,R3,#-2", Ok (R7, R3, Literal (uint32 -2)));
                ("R0, R12, #0b11", Ok (R0, R12, Literal (uint32 3)));
                ("R0, R12, #0x11", Ok (R0, R12, Literal (uint32 17)));
                ("R7,R3", Error "The instruction is invalid");
                ("R7,R3,R5,R9,R1", Error "Flex op 2 has invalid format");
                ("R7,,R3", Error "The instruction is invalid");
                ("R7,R3,", Error "Flex op 2 has invalid format");
                (",R7,R3", Error "The instruction is invalid");
                ("R7,R3,#0b12", Error ("Flex op 2 has invalid format"));
                ("R7,R3,#11111111111111111111", Error ("Invalid 32 bit number"));
                ("R7,R3,#abc", Error ("Flex op 2 has invalid format"));
                ("R7,R3,#0xFFFFFFFF2", Error ("Invalid 32 bit number"));
                ("R7,R3,#2, LSL #1", Error ("Flex op 2 has invalid format"));
                ("R7,R3,R0, LSL #1", Ok (R7,R3, RegisterShift (R0, LSL, 1)));
                ("R7,R3,R0, LSP #1", Error ("Shift operation is invalid"));
                ("R7,R3,R0, ASR R8", Ok (R7,R3, RegisterRegisterShift (R0, ASR, R8)));
                ("R7,R3,R0, LSL R30", Error ("Shift op register is invalid"));
            ]
    
    let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

    let makeInstrString opcode suffix target op1 op2 = 
        let opcodeStr = 
            match opcode with
            | ADD -> "ADD"
            | ADC -> "ADC"
            | SUB -> "SUB"
            | SBC -> "SBC"
            | RSB -> "RSB"
            | RSC -> "RSC"

        let suffixStr = if suffix then "S" else "" 

        let targetStr = regStrings.[target]
        let op1Str = regStrings.[op1]
        let op2Str = match op2 with
                     | Literal num -> "#" + string num
                     | Register reg -> regStrings.[reg]
                     | RegisterShift (op2, shift, num) -> 
                        regStrings.[op2] + "," + operationStrings.[shift] + " #" + string num
                     | RegisterRegisterShift (op2, shift, reg) -> 
                        regStrings.[op2] + "," + operationStrings.[shift] + " " + regStrings.[reg]
        
        let operandStr = targetStr + "," + op1Str + "," + op2Str
        
        opcodeStr, suffixStr, operandStr

    
    [<Tests>]
    let testParse = 
        let makeTestLineData wa opcode suffix target op1 op2 = 
            
            let opcodeStr, suffixStr, operandStr = makeInstrString opcode suffix target op1 op2
  
            {
                LoadAddr = wa;
                Label = None;
                SymTab = None;
                OpCode = opcodeStr + suffixStr;
                Operands = operandStr
            }

        testPropertyWithConfig config "Test Parse" <| 
        fun wa opcode suffix target op1 op2 ->
            let ls = makeTestLineData wa opcode suffix target op1 op2

            let expected = match opcode with
                           | _ -> Some (Ok {
                                        PInstr = 
                                            {
                                            InstrType = Some opcode;
                                            SuffixSet = suffix;
                                            Target = target;
                                            Op1 = op1;
                                            Op2 = op2;
                                            } 
                                        PLabel = None;
                                        PSize = 4u;
                                        PCond = Cal;
                                     })

            
            let result = parse ls
            Expect.equal result expected "Parse Test"

    //[<Tests>]
    let testExec =
        let makeTestExecStr opcode suffix target op1 op2 = 
            let opcodeStr, suffixStr, operandStr = makeInstrString opcode suffix target op1 op2
            //Return full instruction string of opcode + suffix + operands
            opcodeStr + suffixStr + " " + operandStr


        //testPropertyWithConfig config "Test Arithmetic Execution" <|
        //fun opcode suffix target op1 op2 ->
            //let instrStr = makeTestExecStr opcode suffix target op1 op2
            
        testList "Trial visual tests"
            [
            VisualFrameworkTest defaultParas
            vTest "SUB test" "SUB R0, R0, #1" "0000" [R 0, -1]
            ]
        

