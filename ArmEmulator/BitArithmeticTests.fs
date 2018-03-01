module BitArithmeticTests

    open BitArithmetic
    open CommonData
    open CommonLex
    open VisualTest.VTest
    open Expecto
    open VisualTest.VCommon

    // credit Xav kearney for this function
    /// takes a function f, test name
    /// and list of (input, output) tuples
    /// create an Expecto testList
    /// with unit tests, testing each case
    let makeUnitTestList f name inOutLst=
        let makeTest inp outp =
            let testName = (sprintf "%s: %A" name inp)
            testCase testName <| fun () ->
                Expect.equal (f inp) outp testName
        List.map (fun (i, o) -> makeTest i o) inOutLst
        |> testList (sprintf "%s Test List" name)        


    let ld = {Label = None ; LoadAddr = WA 0u ; OpCode = "" ; Operands = "" ; SymTab = None}





// test parser





    [<Tests>]
    let testParse = 
        makeUnitTestList parse "parse tests" 
            [
                // test valid input

                // tests large acceptable input
                {ld with OpCode = "MOV" ; Operands = "R0, #4080"},
                    Some (Ok {PInstr = {instruction = MOV
                                        suff = ""
                                        opA = Some R0
                                        opB = Some (Flex (Some (Num 4080u)))
                                        opC = None}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})

                // tests suffix
                {ld with OpCode = "MOVS" ; Operands = "R0, #12"},
                    Some (Ok {PInstr = {instruction = MOV
                                        suff = "S"
                                        opA = Some R0
                                        opB = Some (Flex (Some (Num 12u)))
                                        opC = None}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})

                // tests flexible opperator
                {ld with OpCode = "MOVS" ; Operands = "R0, R1, LSL #137"},
                    Some (Ok {PInstr = {instruction = MOV
                                        suff = "S"
                                        opA = Some R0
                                        opB = Some (Flex (Some (RegShiftOp (R1,Some (LSL,Some (Nm 137u))))))
                                        opC = None}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})
                
                // test AND with flexible opperator
                {ld with OpCode = "ANDS" ; Operands = "R0, R1, R7, ASR R8 "},
                    Some (Ok {PInstr = {instruction = AND
                                        suff = "S"
                                        opA = Some R0
                                        opB = Some (Reg (Some R1))
                                        opC = Some (Flex (Some (RegShiftOp (R7,Some(ASR,Some (Rg R8))))))}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})

                // test EOR with flexible opperator RRX
                {ld with OpCode = "EORS" ; Operands = "R0, R1, R7, RRX"},
                    Some (Ok {PInstr = {instruction = EOR
                                        suff = "S"
                                        opA = Some R0
                                        opB = Some (Reg (Some R1))
                                        opC = Some (Flex (Some (RegShiftOp (R7,Some(RRX,None)))))}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})                              



                // testing invalid inputs

                // tests large unacceptable input
                {ld with OpCode = "MOV" ; Operands = "R0, #4081"},
                    Some (Ok {PInstr = {instruction = MOV
                                        suff = ""
                                        opA = Some R0
                                        opB = Some (Flex None)
                                        opC = None}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})


                // tests ,,
                {ld with OpCode = "AND" ; Operands = "R0, , R7, ASR R8 "},Some (Error "Parse error")                            

                //tests invalid destination
                {ld with OpCode = "AND" ; Operands = "R16, #4, R7, RRX"},
                    Some (Ok {PInstr = {instruction = AND
                                        suff = ""
                                        opA = None
                                        opB = Some (Reg None)
                                        opC = Some (Flex (Some (RegShiftOp (R7,Some(RRX,None)))))}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})

                //tests invalid first opperand
                {ld with OpCode = "AND" ; Operands = "R0, #4, R7, RRX"},
                    Some (Ok {PInstr = {instruction = AND
                                        suff = ""
                                        opA = Some R0
                                        opB = Some (Reg None)
                                        opC = Some (Flex (Some (RegShiftOp (R7,Some(RRX,None)))))}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})


                // tests invalid second opperand
                {ld with OpCode = "AND" ; Operands = "R0, R1, #5, ASR R8 "},
                    Some (Ok {PInstr = {instruction = AND
                                        suff = ""
                                        opA = Some R0
                                        opB = Some (Reg (Some R1))
                                        opC = Some (Flex None)}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})         


                // tests invalid shift 
                {ld with OpCode = "AND" ; Operands = "R0, R1, R3, LSK R8 "},
                    Some (Ok {PInstr = {instruction = AND
                                        suff = ""
                                        opA = Some R0
                                        opB = Some (Reg (Some R1))
                                        opC = Some (Flex None)}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})


                // // tests invalid shift opperand
                {ld with OpCode = "AND" ; Operands = "R0, R1, R3, ROR R17, #13 "},Some (Error "Parse error")                                                                                                

            ]




// test VisualTest works





    [<Tests>]
    let tests = 
        testList "Minimal Visual Unit Tests"
            [
            // MOV tests with decimals
            vTest "MOV test 1" "MOV R0, #1" "0000" [R 0, 1]
            vTest "MOV test 2" "MOVS R1, #0" "0100" [R 1, 0]
            vTest "MOV test 3" "MOV R2, #137" "0000" [R 2, 137]
            vTest "MOV test 4" "MOV R3, #4080" "0000" [R 3, 4080]
            // MOV tests with hex numbers
            vTest "MOV test 5" "MOV R4, #0x0" "0000" [R 4, 0]
            vTest "MOV tes 6" "MOV R5, #0xA" "0000" [R 5, 10]
            vTest "MOV test 7" "MOV R6, #0x2300" "0000" [R 6, 8960]
            // MOV tests with hex binary
            vTest "MOV test 8" "MOV R4, #0b0" "0000" [R 4, 0]
            vTest "MOV test 9" "MOV R0, #0b1010" "0000" [R 0, 10]
            vTest "MOV test 10" "MOV R0, #0b10001100000000" "0000" [R 0, 8960]

            // // AND tests with decimals (commented out because failing)
            // vTest "AND test 1" "AND R2, R1, R0" "0000" [R 0, -2]
            // vTest "AND test 2" "AND R3, R2, R0" "0000" [R 0, -2]
            // vTest "AND test 3" "AND R2, R2, R0" "0000" [R 2, -138]
            // vTest "AND test 4" "AND R3, R3, R3" "0000" [R 3, -4081]  
            ]
    




// testing execute function (exeInstr)

   



    let makeUnitTestListExe f name inOutLst=
        let makeTest inp outp =
            let testName = (sprintf "%s: %A" name inp)
            testCase testName <| fun () ->
                Expect.equal (f inp) outp testName
        List.map (fun (i, o) -> makeTest i o) inOutLst
        |> testList (sprintf "%s Test List" name)

    /// takes the output form execute instructions and changes it into a form that can be compared to visual testing
    /// needs the register of intrest to be specified to check the destination
    let convExe rName exeOut =

        let boolToStrInt = function
        | true -> "1"
        | false -> "0"

        match exeOut with 
            | Ok instExe ->
                let flgs = 
                    List.map boolToStrInt [instExe.Fl.N;instExe.Fl.Z;instExe.Fl.C;instExe.Fl.V]
                    |> List.reduce (+)
                let specRegConts = 
                    Map.find rName instExe.Regs
                    |> int32
                flgs,[R rName.RegNum,specRegConts]
            | _ -> failwithf "exeOut is an error"

    let parseThenExe cpuData destReg = parse >> exeInstr cpuData >> convExe destReg


    [<Tests>]
    let testsExe = 
        testList "Execution tests"
            [
            // MOV tests with decimals
            // vTest "MOV test 1" "MOV R0, #1" "0000" (parseThenExe `cpuData` R0) 
            vTest "MOV test 2" "MOVS R1, #0" "0100" [R 1, 0]
            vTest "MOV test 3" "MOV R2, #137" "0000" [R 2, 137]
            vTest "MOV test 4" "MOV R3, #4080" "0000" [R 3, 4080]
            // MOV tests with hex numbers
            vTest "MOV test 5" "MOV R4, #0x0" "0000" [R 4, 0]
            vTest "MOV tes 6" "MOV R5, #0xA" "0000" [R 5, 10]
            vTest "MOV test 7" "MOV R6, #0x2300" "0000" [R 6, 8960]
            // MOV tests with hex binary
            vTest "MOV test 8" "MOV R4, #0b0" "0000" [R 4, 0]
            vTest "MOV test 9" "MOV R0, #0b1010" "0000" [R 0, 10]
            vTest "MOV test 10" "MOV R0, #0b10001100000000" "0000" [R 0, 8960]

            // // AND tests with decimals (commented out because failing)
            // vTest "AND test 1" "AND R2, R1, R0" "0000" [R 0, -2]
            // vTest "AND test 2" "AND R3, R2, R0" "0000" [R 0, -2]
            // vTest "AND test 3" "AND R2, R2, R0" "0000" [R 2, -138]
            // vTest "AND test 4" "AND R3, R3, R3" "0000" [R 3, -4081]  
            ]














    // let flags', regs', mem' = RunVisualWithFlagsOut defaultParas "MOV R4, #0"

    // let getFlagsReg input =
    //     match input with
    //     | flgs, regs, _ -> flgs,regs