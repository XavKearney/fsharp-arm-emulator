module BitArithmeticTests

    open BitArithmetic
    open CommonData
    open CommonLex
    open VisualTest.VTest
    open Expecto
    open VisualTest.VCommon
    open VisualTest.Visual

    // credit xav for this function
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


                // // tests invalid second opperand
                {ld with OpCode = "AND" ; Operands = "R0, R1, #5, ASR R8 "},
                    Some (Ok {PInstr = {instruction = AND
                                        suff = ""
                                        opA = Some R0
                                        opB = Some (Reg (Some R1))
                                        opC = Some (Flex None)}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})                                              

            ]
             


    // [<Tests>]
    // let testExecute = 
    //     makeUnitTestList exeInstr "parse tests" 
    //         [
                

    //         ]