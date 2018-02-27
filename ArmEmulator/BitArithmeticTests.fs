module BitArithmeticTests

    open BitArithmetic
    open CommonData
    open CommonLex
    open VisualTest.VTest
    open Expecto
    open VisualTest.VCommon
    open VisualTest.Visual

    // credit to XK egal eyes i.e Jeff Kenny
    /// take a function f, test name
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


    /// unit tests for parseMemOps function
    /// takes a string, corresponding to ls.Operands
    /// returns either an error or
    /// target register, writeback (bool) and a register list
    [<Tests>]
    let testParseMemOpsUnit = 
        makeUnitTestList parse "parse tests" 
            [
                // test valid input for MOV
                // tests usual input
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

                // test that should not work
                {ld with OpCode = "AND" ; Operands = "R0, , R7, ASR R8 "},Some (Error "Parse Error")                            


            ] 




// Some (Ok {PInstr = {instruction = AND
//                                   suff = ""
//                                   opA = Some R0
//                                   opB = Some (Reg (Some R1))
//                                   opC = Some (Flex (Some (RegShiftOp (R3,Some (RRX, None)))))}
//                     PLabel = None
//                     PSize = 4u
//                     PCond = Ceq})

    // [<Tests>]
    // let tests = 
    //     testList "Minimal Visual Unit Tests"
    //         [
    //         // MOV tests with decimals
    //         vTest "MOV test 1" "MOV R0, #1" "0000" [R 0, 1]
    //         vTest "MOV test 2" "MOVS R1, #0" "0000" [R 1, 0]
    //         vTest "MOV test 3" "MOV R2, #137" "0000" [R 2, 137]
    //         vTest "MOV test 4" "MOV R3, #4080" "0000" [R 3, 4080]
    //         // MOV tests with hex numbers
    //         vTest "MOV test 5" "MOV R4, #0x0" "0000" [R 4, 0]
    //         vTest "MOV tes 6" "MOV R5, #0xA" "0000" [R 5, 10]
    //         vTest "MOV test 7" "MOV R6, #0x2300" "0000" [R 6, 8960]
    //         // MOV tests with hex binary
    //         vTest "MOV test 8" "MOV R4, #0b0" "0000" [R 4, 0]
    //         vTest "MOV test 9" "MOV R0, #0b1010" "0000" [R 0, 10]
    //         vTest "MOV test 10" "MOV R0, #0b10001100000000" "0000" [R 0, 8960]
            
    //         ]
    
    // let flags, regs, mem = RunVisualWithFlagsOut defaultParas "MOV R4, #0"
    
    // let convReg (vOut : VisOutput) = 
    //     let regs = vOut.Regs
    //     match regs with
    //     | [reg, num] -> 
   
    // // MVN tests with decimals
    // vTest "MVN test" "MVN R0, #1" "0000" [R 0, -2]
    // vTest "MVNS test" "MVNS R1, #0" "0000" [R 1, 0]
    // vTest "MVN test" "MVN R2, #137" "0000" [R 2, -138]
    // vTest "MVN test" "MVN R3, #4080" "0000" [R 3, -4081]
    // // MVN tests with hex numbers
    // vTest "MVN test" "MVNS R4, #0x0" "0000" [R 4, -1]
    // vTest "MVN test" "MVNS R5, #0xA" "0000" [R 5, -11]
    // vTest "MVN test" "MVNS R6, #0x2300" "0000" [R 6, -8961]
    // // MVN tests with hex binary
    // vTest "MVN test" "MVNS R4, #0b0" "0000" [R 4, -1]
    // vTest "MVN test" "MVNS R0, #0b1010" "0000" [R 0, -11]
    // vTest "MVN test" "MVNS R0, #0b10001100000000" "0000" [R 0, -8961]





    // // AND tests with decimals
    // vTest "AND test" "AND R2, R1, R0" "0000" [R 0, -2]
    // vTest "AND test" "AND R3, R2, R0" "0000" [R 0, -2]
    // vTest "AND test" "AND R2, R2, R0" "0000" [R 2, -138]
    // vTest "AND test" "AND R3, R3, R3" "0000" [R 3, -4081]
    // // AND tests with hex numbers
    // vTest "AND test" "ANDS R2, R1, R0" "0000" [R 4, -1]
    // vTest "AND test" "ANDS R3, R1, R1" "0000" [R 5, -11]
    // vTest "AND test" "ANDS R6, R0, R3" "0000" [R 6, -8961]
    // // AND tests with hex binary
    // vTest "AND test" "ANDS R4, R1, #7" "0000" [R 4, -1]
    // vTest "AND test" "ANDS R0, R1, #0xA" "0000" [R 0, -11]
    // vTest "AND test" "ANDS R0, R0, #0b12" "0000" [R 0, -8961]



    // (*
    //     /// instructions
    // type InstRoots =  MOV | MVN | AND | ORR | EOR | BIC | LSL | LSR | ASR
    //                 | ROR | RRX | TST | TEQ 
    // *)