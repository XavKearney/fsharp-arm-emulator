module ArithmeticTests
    open CommonData
    open CommonLex
    open Arithmetic
    open Expecto
    open VisualTest.Visual
    open VisualTest.VTest
    open VisualTest.VCommon

    /// take a function f, test name
    /// and list of (input, output) tuples
    /// create an Expecto testList
    /// with unit tests, testing each case
    let makeTestList testFun symTable name testVals =
        let makeTest inp outp =
            let testName = (sprintf "%s: %A" name inp)
            testCase testName <| fun () ->
                Expect.equal (testFun inp symTable) outp testName
        List.map (fun (input, output) -> makeTest input output) testVals
        |> testList (sprintf "%s Test List" name)

    
    // Taken from xk-mult-mem -> Generate list of random values
    let genRandomUint32List (min,max) count =
        let rnd = System.Random()
        List.init count (fun _ -> rnd.Next (min, max))
        |> List.map uint32 


    [<Tests>]
    let testParseArithLine = 
        // Unit tests for ParseArithLine
        let symTable = Some (["test", uint32 2] |> Map.ofList)
        makeTestList parseArithLine symTable "parseArithLine Unit Tests" 
            [
                ("R7,R3,R9", Ok (R7, R3, Register R9));
                ("R0, R12, R1", Ok (R0, R12, Register R1));
                ("R,R3,R9", Error "Destination is not a valid register");
                ("R7, R20, R9", Error "Op1 is not a valid register");
                ("R7, R3, R20", Error "Op2 is not a valid register or expression");
                ("R7,R3,#-2", Ok (R7, R3, Literal (uint32 -2)));
                ("R0, R12, #0b11", Ok (R0, R12, Literal (uint32 3)));
                ("R0, R12, #0x11", Ok (R0, R12, Literal (uint32 17)));
                ("R7,R3", Error "Invalid arithmetic instruction");
                ("R7,R3,R5,R9,R1", Error "Invalid arithmetic instruction");
                ("R7,,R3", Error "Op1 is not a valid register");
                ("R7,R3,", Error "Op2 is not a valid register or expression");
                (",R7,R3", Error "Destination is not a valid register");
                ("R7,R3,#0b12", Error ("Invalid literal at end of expression."));
                ("R7,R3,#11111111111111111111", Error ("Invalid literal at end of expression."));
                ("R7,R3,#abc", Error ("Invalid literal at end of expression."));
                ("R7,R3,#0xFFFFFFFF2", Error ("Invalid literal at end of expression."));
                ("R7,R3,#2, LSL #1", Error ("Invalid op2 register"));
                ("R7,R3,R0, LSL #1", Ok (R7,R3, RegisterShift (R0, LSL, 1)));
                ("R7,R3,R0, LSP #1", Error ("Invalid shift operation"));
                ("R7,R3,R0, ASR R8", Ok (R7,R3, RegisterRegisterShift (R0, ASR, R8)));
                ("R7,R3,R0, LSL R30", Error ("Op2 is not a valid register or expression"));
                ("R7,R3,R0, RRX R30", Error ("RRX always has a shift of 1"));
                

                ("R0, R12, #3*6-1", Ok (R0, R12, Literal (uint32 17)));
                ("R0, R12, #3+6*2", Ok (R0, R12, Literal (uint32 15)));
                ("R0, R12, #-3*6+1", Ok (R0, R12, Literal (uint32 -17)));
                ("R0, R12, #-3*6+-1", Error ("Invalid expression."));
                ("R0, R12, #0b11*6+1", Ok (R0, R12, Literal (uint32 19)));
                ("R0, R12, #0b12*6+1", Error ("Invalid literal in expression."));
                ("R0, R12, #3-2-", Error ("Invalid expression."));
                ("R7,R3,#0b111101001101101", Error ("Invalid 32 bit number"));
                ("R7,R3,#0b111111110", Error ("Invalid 32 bit number"));
                ("R7,R3,#0b1111111100", Ok (R7, R3, Literal (uint32 1020)));
                ("R0, R12, #+1", Ok (R0, R12, Literal (uint32 1)));
                ("R0, R12, #*6", Error ("Expression cannot start with *.")); 
                ("R0, R12, #&F", Ok (R0, R12, Literal (uint32 15)));
                ("R0, R12, #-97  +   1", Ok (R0, R12, Literal (uint32 -96)));     
                ("R0, R12, #2*-3", Error ("Invalid expression.")); 
                ("R0, R12, #test", Ok (R0, R12, Literal (uint32 2)));
                ("R0, R12, R0, RRX", Ok (R0, R12, RegisterShift (R0, RRX, int32 1)));
                ("R0, R12, R0, RRX #1", Error ("RRX always has a shift of 1"));

                ("R0, R12, #8*8*8*8*23+1", Error ("Invalid 32 bit number"));


            ]

    [<Tests>]
    let testParseCompLine =
        // Unit tests for ParseCompLine
        let symTable = Some (["test", uint32 2] |> Map.ofList)
        makeTestList parseCompLine symTable "parseCompLine Unit Tests"
            [
                ("R7,R3", Ok (R7, Register R3));
                ("R7,fd", Error ("Op2 is not a valid register or expression"));
                ("R7,fd,,", Error ("Invalid compare instruction"));
                ("Radfsa", Error ("Invalid compare instruction"));
                ("R7,#test*4+2", Ok (R7, Literal (uint32 10)));
                ("R7,#lol", Error ("Invalid literal at end of expression."));
                ("R7,#test*&A", Ok (R7, Literal (uint32 20)));
                ("R7,#test*4+2, LSL #5", Error ("Op2 is an invalid register"));
                ("R7,R1, LSL #1", Ok (R7, RegisterShift (R1, LSL, int32 1)));
                ("R7,R1, LSP #5", Error ("Shift op is invalid"));
                ("R7,R1, LSL R8", Ok (R7, RegisterRegisterShift (R1, LSL, R8)));
                ("R0, R0, RRX", Ok (R0, RegisterShift (R0, RRX, int32 1)));
                ("R0, R0, RRX #100", Error ("RRX always has a shift of 1"));
            ]

    let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

    /// Make instruction string for arithmetic from random input values
    let makeArithInstrString opcode suffix target op1 op2 = 
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
                        match shift with
                        | RRX ->
                            regStrings.[op2] + "," + operationStrings.[shift]
                        | _ ->
                            regStrings.[op2] + "," + operationStrings.[shift] + " #" + string num
                     | RegisterRegisterShift (op2, shift, reg) -> 
                        match shift with
                        | RRX ->
                            regStrings.[op2] + "," + operationStrings.[shift]
                        | _ ->
                            regStrings.[op2] + "," + operationStrings.[shift] + " " + regStrings.[reg]
        
        let operandStr = targetStr + "," + op1Str + "," + op2Str
        
        opcodeStr, suffixStr, operandStr

    
    //[<Tests>]
    let testArithParse = 
        // Test parse function for arithmetic opcodes
        let makeTestLineData wa opcode suffix target op1 op2 = 
            
            let opcodeStr, suffixStr, operandStr = makeArithInstrString opcode suffix target op1 op2
  
            {
                LoadAddr = wa;
                Label = None;
                SymTab = None;
                OpCode = opcodeStr + suffixStr;
                Operands = operandStr
            }

        testPropertyWithConfig config "Test Arith Parse" <| 
        fun wa opcode suffix target op1 op2 ->
            let ls = makeTestLineData wa opcode suffix target op1 op2

            let expected = 
                match opcode, target, op1, op2 with
                // Restrictions on target register
                | ADD, R15, R13, _ | ADD, R15, _, Register R13->
                    Some (Error ("Target register cannot be PC if op1 or op2 is R13"))
                | ADC, R15, _, _ | RSB, R15, _, _ | RSC, R15, _, _ | SBC, R15, _, _  ->
                    Some (Error ("Target register cannot be PC for ADC, RSB, SBC and RSC"))
                | ADC, R13, _, _ | RSB, R13, _, _ | RSC, R13, _, _ | SBC, R13, _, _ ->
                    Some (Error ("Target register cannot be SP for ADC, RSB, SBC and RSC"))
                | SUB, R13, op1, _ when op1 <> R13 ->
                    Some (Error ("Target register can only be SP if op1 is SP"))
                | ADD, R13, op1, _ when op1 <> R13 ->
                    Some (Error ("Target register can only be SP if op1 is SP"))
                // Restriction on RRX
                | _, _, _, RegisterShift(op2Reg, RRX, _) | _, _, _, RegisterRegisterShift(op2Reg, RRX, _) ->
                    Some (Ok {
                        PInstr = ArithI
                            {
                            InstrType = Some opcode;
                            SuffixSet = suffix;
                            Target = target;
                            Op1 = op1;
                            Op2 = RegisterShift(op2Reg, RRX, 1)
                            } 
                        PLabel = None;
                        PSize = 4u;
                        PCond = Cal;
                     })
                | _ -> 
                    Some (Ok {
                        PInstr = ArithI
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
            Expect.equal result expected "Parse Arith Test"

    // Make Comparison instruction string from random input values
    let makeCompInstrString opcode op1 op2 = 
        let opcodeStr = 
            match opcode with
            | CMP -> "CMP"
            | CMN -> "CMN"

        let op1Str = regStrings.[op1]
        let op2Str = match op2 with
                     | Literal num -> "#" + string num
                     | Register reg -> regStrings.[reg]
                     | RegisterShift (op2, shift, num) ->
                        match shift with
                        | RRX ->
                            regStrings.[op2] + "," + operationStrings.[shift]
                        | _ -> 
                            regStrings.[op2] + "," + operationStrings.[shift] + " #" + string num
                     | RegisterRegisterShift (op2, shift, reg) -> 
                        match shift with
                        | RRX ->
                            regStrings.[op2] + "," + operationStrings.[shift]
                        | _ ->
                            regStrings.[op2] + "," + operationStrings.[shift] + " " + regStrings.[reg]
        
        let operandStr = op1Str + "," + op2Str
        
        opcodeStr, operandStr


   // [<Tests>]
    let testCompParse = 
        // Test parse function for comparison instructions
        let makeTestLineData wa opcode op1 op2 = 
            
            let opcodeStr, operandStr = makeCompInstrString opcode op1 op2
  
            {
                LoadAddr = wa;
                Label = None;
                SymTab = None;
                OpCode = opcodeStr;
                Operands = operandStr
            }

        testPropertyWithConfig config "Test Comp Parse" <| 
        fun wa opcode op1 op2 ->
            let ls = makeTestLineData wa opcode op1 op2

            let expected = 
                match opcode, op1, op2 with
                | _, _, RegisterShift(op2Reg, RRX, _) | _, _, RegisterRegisterShift(op2Reg, RRX, _) ->
                    Some (Ok {
                        PInstr = CompI
                            {
                            InstrType = Some opcode;
                            Op1 = op1;
                            Op2 = RegisterShift(op2Reg, RRX, 1)
                            } 
                        PLabel = None;
                        PSize = 4u;
                        PCond = Cal;
                     })
                | _ ->
                    Some (Ok {
                        PInstr = CompI
                            {
                            InstrType = Some opcode;
                            Op1 = op1;
                            Op2 = op2;
                            } 
                        PLabel = None;
                        PSize = 4u;
                        PCond = Cal;
                     })

            
            let result = parse ls
            Expect.equal result expected "Parse Comp Test"



    //[<Tests>]
    let testArithExec =
        let makeTestExecStr opcode suffix target op1 op2 = 
            let opcodeStr, suffixStr, operandStr = makeArithInstrString opcode suffix target op1 op2
            //Return full instruction string of opcode + suffix + operands
            opcodeStr + suffixStr + " " + operandStr


        testPropertyWithConfig config "Test Arithmetic Execution" <|
        fun opcode suffix target op1 op2 (flags: CommonData.Flags) ->
            let instrStr = makeTestExecStr opcode suffix target op1 op2
            
            // Place restrictions on input data
            // visUAL has a lot of restrictions on inputs, so this next section
            // conforms to these.
            let valid = 
                match opcode, target, op1, op2, flags with
                | _, _, _, _, f when f.N && f.Z -> false
                | ADD, R15, R13, _, _ | ADD, R15, _, Register R13, _ -> false
                
                // Dest can only be R13 if op1 is also R13
                | SUB, R13, op1, _, _ when op1 <> R13 -> false
                | ADD, R13, op1, _, _ when op1 <> R13 -> false
                
                // There are also restrictions on op2.  These restrictions are removed in my implementation
                | SUB, R13, R13, _, _ -> false 
                | ADD, R13, R13, _, _ -> false
                
                // ADC, RSB, RSC, SBC target cannot be SP or PC
                | ADC, R15, _, _, _ | RSB, R15, _, _, _ | RSC, R15, _, _, _ | SBC, R15, _, _, _ -> false
                | ADC, R13, _, _, _ | RSB, R13, _, _, _ | RSC, R13, _, _, _ | SBC, R13, _, _, _ -> false
                // ADC, RSB, RSC, SBC first operand cannot be SP or PC   
                | ADC, _, R15, _, _ -> false
                | RSB, _, R15, _, _ -> false
                | RSC, _, R15, _, _ -> false
                | SBC, _, R15, _, _ -> false
                | ADC, _, R13, _, _ -> false
                | RSB, _, R13, _, _ -> false
                | RSC, _, R13, _, _ -> false
                | SBC, _, R13, _, _ -> false
                // ADC, RSB, RSC, ADD, SUB, SBC second operand cannot be SP or PC
                | ADC, _, _, Register R15, _ -> false
                | RSB, _, _, Register R15, _ -> false
                | RSC, _, _, Register R15, _ -> false
                | ADD, _, _, Register R15, _ -> false
                | SUB, _, _, Register R15, _ -> false
                | SBC, _, _, Register R15, _ -> false
                | ADC, _, _, Register R13, _ -> false
                | RSB, _, _, Register R13, _ -> false
                | RSC, _, _, Register R13, _ -> false
                | ADD, _, _, Register R13, _ -> false
                | SUB, _, _, Register R13, _ -> false
                | SBC, _, _, Register R13, _ -> false

                | ADC, _, _, RegisterShift (R15,_, _), _ -> false
                | RSB, _, _, RegisterShift (R15,_, _), _ -> false
                | RSC, _, _, RegisterShift (R15,_, _), _ -> false
                | ADD, _, _, RegisterShift (R15,_, _), _ -> false
                | SUB, _, _, RegisterShift (R15,_, _), _ -> false
                | SBC, _, _, RegisterShift (R15,_, _), _ -> false
                | ADC, _, _, RegisterShift (R13,_, _), _ -> false
                | RSB, _, _, RegisterShift (R13,_, _), _ -> false
                | RSC, _, _, RegisterShift (R13,_, _), _ -> false
                | ADD, _, _, RegisterShift (R13,_, _), _ -> false
                | SUB, _, _, RegisterShift (R13,_, _), _ -> false
                | SBC, _, _, RegisterShift (R13,_, _), _ -> false

                | ADC, _, _, RegisterRegisterShift (R15,_, _), _ -> false
                | RSB, _, _, RegisterRegisterShift (R15,_, _), _ -> false
                | RSC, _, _, RegisterRegisterShift (R15,_, _), _ -> false
                | ADD, _, _, RegisterRegisterShift (R15,_, _), _ -> false
                | SUB, _, _, RegisterRegisterShift (R15,_, _), _ -> false
                | SBC, _, _, RegisterRegisterShift (R15,_, _), _ -> false
                | ADC, _, _, RegisterRegisterShift (R13,_, _), _ -> false
                | RSB, _, _, RegisterRegisterShift (R13,_, _), _ -> false
                | RSC, _, _, RegisterRegisterShift (R13,_, _), _ -> false
                | ADD, _, _, RegisterRegisterShift (R13,_, _), _ -> false
                | SUB, _, _, RegisterRegisterShift (R13,_, _), _ -> false
                | SBC, _, _, RegisterRegisterShift (R13,_, _), _ -> false
                
                // RegisterRegisterShift cannot have PC as the shift value
                | _, _, _, RegisterRegisterShift (_, _ , R15), _ -> false

                // ADC, RSB, RSC target cannot be R13 or R15
                | ADC, R13, _, _, _ | ADC, R15, _, _, _ -> false
                | RSB, R13, _, _, _ | RSB, R15, _, _, _ -> false
                | RSC, R13, _, _, _ | RSC, R15, _, _, _ -> false

                | SUB, _, R15, _, _ -> false
                | SUB, _, _, RegisterShift(R15, _, _), _ -> false
                
                // visUAL restriction on ADC with RRX -> This is not in my implementation
                | ADC, _, _, RegisterShift(_, RRX, _), _ -> false
                | ADC, _, _, RegisterRegisterShift(_, RRX, _), _ -> false

                // Don't make PC target for tests due to infinte looping issues
                | _, R15, _, _, _ -> false

                // PC is different between visual and local so remove from testing
                | _, _, R15, _, _ | _, _, _, Register R15, _ -> false

                // RSC and RSB prove difficult to test with runtime errors for negative second operand
                // Comment the below line to perform some automated testing
                // NOTE: Will likely result in runtime errors in its current form
                
                | RSC, _, _, _, _ | RSB, _, _, _, _ -> false
                
                // Shift of more than 32 will be 0 in my implementation
                | _, _, _, RegisterShift(_,_, x), _ when uint32 x >= 32u -> false
                | _, _, _, RegisterRegisterShift(_), _ -> false

                //| SUB, _, _, _, _ | SBC, _, _, _, _ -> false
                //| ADD, _, _, _, _ | ADC, _, _, _, _ -> false

                | _ -> true

            match valid with
            | false -> ()
            | true -> 
                let randomRegs = 
                    genRandomUint32List (-0x7FFFFFFF, 0x7FFFFFFF) 15
                
                let testParas = {
                    defaultParas with
                        InitRegs = randomRegs; 
                        //initialise flags
                        InitFlags = {FN=flags.N;FZ=flags.Z; FC=flags.C;FV=flags.V}
                    }
                
                let randomRegsLocal = randomRegs |> fun lst -> List.concat [lst; [0u;]]
                let initTestRegs = List.mapi (fun i x -> (inverseRegNums.[i], x)) randomRegsLocal |> Map.ofList;

                let parsed = {
                    PInstr = ArithI
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
                }

                // Run visualTest with flagsOut to compare to my implementation
                let visFlags, visRegs, _ = RunVisualWithFlagsOut testParas instrStr

                let flagsActual = {
                    N = visFlags.FN;
                    Z = visFlags.FZ;
                    C = visFlags.FC;
                    V = visFlags.FV;
                }

                // get the values of the registers from VisUAL in ascending order
                let regsActual = 
                    visRegs.Regs
                    |> List.sortBy (fun (r, _) -> r)
                    |> List.map (fun (_, i) -> uint32 i)

                let cpuData = {
                    Fl = flags;
                    Regs = initTestRegs
                    MM = Map.empty;
                }

                // Use my code with the same input values to obtain hopefully the same output
                let results = execArithmeticInstr cpuData parsed

                match results with
                | Ok resCpu ->
                    let localRegs = 
                        resCpu.Regs
                        |> Map.toList
                        |> List.map (fun (_, x) -> x)

                    // Expecto checks
                    Expect.equal regsActual.[..regsActual.Length - 2] localRegs.[..localRegs.Length - 2] "Registers"
                    Expect.equal flagsActual resCpu.Fl "Flags"
                | Error _ -> ()

    //[<Tests>]
    let testCompExec =
        let makeTestExecStr opcode op1 op2 = 
            let opcodeStr, operandStr = makeCompInstrString opcode op1 op2
            //Return full instruction string of opcode + suffix + operands
            opcodeStr + " " + operandStr


        testPropertyWithConfig config "Test Comparison Execution" <|
        fun opcode op1 op2 (flags: CommonData.Flags) ->
            let instrStr = makeTestExecStr opcode op1 op2
            
            let valid = 
                match opcode, op1, op2, flags with
                | _, _, _, f when f.N && f.Z -> false
                
                | CMP, _, Register R15, _ -> false
                | CMN, _, Register R15, _ -> false
                | CMP, _, Register R13, _ -> false
                | CMN, _, Register R13, _ -> false

                | CMP, _, RegisterShift (R15,_,_), _ -> false
                | CMN, _, RegisterShift (R15,_,_), _ -> false
                | CMP, _, RegisterShift (R13,_,_), _ -> false
                | CMN, _, RegisterShift (R13,_,_), _ -> false

                | CMP, _, RegisterRegisterShift (R15,_,_), _ -> false
                | CMN, _, RegisterRegisterShift (R15,_,_), _ -> false
                | CMP, _, RegisterRegisterShift (R13,_,_), _ -> false
                | CMN, _, RegisterRegisterShift (R13,_,_), _ -> false

                | CMP, _, RegisterRegisterShift (_,_,R15), _ -> false
                | CMN, _, RegisterRegisterShift (_,_,R15), _ -> false

                // Shift of more than 32 will be 0 in my implementation
                | _, _, RegisterShift(_,_, x), _ when uint32 x >= 32u -> false
                | _, _, RegisterRegisterShift(_), _ -> false

                // First operand cannot be PC
                | _, R15, _, _ -> false
               
                | _ -> true

            match valid with
            | false -> ()
            | true -> 
                let randomRegs = 
                    genRandomUint32List (-0x7FFFFFFF, 0x7FFFFFFF) 15
                
                let testParas = {
                    defaultParas with
                        InitRegs = randomRegs; 
                        //initialise flags
                        InitFlags = {FN=flags.N;FZ=flags.Z; FC=flags.C;FV=flags.V}
                    }
                
                let randomRegsLocal = randomRegs |> fun lst -> List.concat [lst; [0u;]]
                let initTestRegs = List.mapi (fun i x -> (inverseRegNums.[i], x)) randomRegsLocal |> Map.ofList;

                let parsed = {
                    PInstr = CompI
                            {
                            InstrType = Some opcode;
                            Op1 = op1;
                            Op2 = op2;
                            }
                    PLabel = None;
                    PSize = 4u;
                    PCond = Cal;
                }

                let visFlags, _, _ = RunVisualWithFlagsOut testParas instrStr

                let flagsActual = {
                    N = visFlags.FN;
                    Z = visFlags.FZ;
                    C = visFlags.FC;
                    V = visFlags.FV;
                }

                let cpuData = {
                    Fl = flags;
                    Regs = initTestRegs
                    MM = Map.empty;
                }

                let results = execArithmeticInstr cpuData parsed

                match results with
                | Ok resCpu -> 
                    Expect.equal flagsActual resCpu.Fl "Flags"
                | Error _ -> ()
        

