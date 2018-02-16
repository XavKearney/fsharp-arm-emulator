module MultMemTests
    open CommonData
    open CommonLex
    open Arithmetic
    open Expecto
    open FsCheck

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
                ("R7,R3,R9", Ok (R7, R3, Target R9));
                ("R0, R12, R1", Ok (R0, R12, Target R1));
                ("R,R3,R9", Error "Destination register not valid");
                ("R7, R20, R9", Error "Op1 is not a valid register");
                ("R7, R3, R20", Error "Op2 is not a valid register");
                ("R7,R3,#-2", Ok (R7, R3, Value (uint32 -2)));
                ("R0, R12, #0b11", Ok (R0, R12, Value (uint32 3)));
                ("R0, R12, #0x11", Ok (R0, R12, Value (uint32 17)));
                ("R7,R3", Error "Operand list is invalid");
                ("R7,R3,R5,R9,R1", Error "Operand list is invalid");
                ("R7,,R3", Error "Op1 is not a valid register");
                ("R7,R3,", Error "Op2 is not a valid register");
                (",R7,R3", Error "Destination register not valid");
                ("R7,R3,#0b12", Error ("Invalid 32 bit number"));
                ("R7,R3,#1111111111111", Error ("Invalid 32 bit number"));
                ("R7,R3,#abc", Error ("Invalid 32 bit number"));
            ]
    
    let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    
    [<Tests>]
    let testParse = 
        let makeTestLineData wa opcode suffix target op1 op2 = 
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
                         | Value num -> "#" + string num
                         | Target reg -> regStrings.[reg]
            
            let operandStr = targetStr + "," + op1Str + "," + op2Str
  
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
