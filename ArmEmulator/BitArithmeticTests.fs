module BitArithmeticTests

    open BitArithmetic
    open CommonData
    open CommonLex
    open VisualTest.VTest
    open Expecto
    open VisualTest.VCommon


    let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

    let genRandomUint32List (min,max) count =
        let rnd = System.Random()
        List.init count (fun _ -> rnd.Next (min, max))
        |> List.map uint32

    let testSymTab = [("testLab", 37u); ("otherLab", 94u)] |> Map.ofList

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








    // testing input constants:

    let makeUnitTestListLit f fTest name inLst=
        let makeTest inp inpTest =
            let testName = (sprintf "%s: %A" name inp)
            testCase testName <| fun () ->
                Expect.equal (f inp testSymTab) (fTest inpTest) testName
        List.map (fun (i, inTest) -> makeTest i inTest) inLst
        |> testList (sprintf "%s Test List" name) 



    /// function from tick 3 feedback
    /// used to test against toLit
    let checkLiteral (lit:uint32) =
        let valid0 =
            let rotMask n = (0xFFu >>> n) ||| (0xFFu <<< 32 - n)
            [0..2..30] 
            |> List.map rotMask 
            |> List.exists (fun mask -> (mask &&& lit) = lit)
            |> function  
                    | true -> Some lit 
                    | false -> None
        let valid1 =        
            let rotMask n = (0xFFu >>> n) ||| (0xFFu <<< 32 - n)
            [0..2..30] 
            |> List.map rotMask 
            |> List.exists (fun mask -> (mask &&& (~~~ lit)) = lit)
            |> function  
                    | true -> Some lit 
                    | false -> None

        match valid0, valid1 with
        | Some x,_ -> Ok x
        | _, Some x -> Ok x    
        | _ -> Error "Litteral can't be created by rotating an 8 bit number in a 32 bit word "  


    /// unit tests for literals
    /// used to test corner cases
    [<Tests>]
    let testLit = 
        makeUnitTestListLit toLit (fun x -> x) "Literal tests" 
            [
                // positives
                "#0", Ok 0u
                "#1", Ok 1u 
                "#1+0", Ok 1u
                "#2147483647", Ok 2147483647u
                //"#9148140018481", Error " " // test very large numbers


                // negatives
                "#-1", Ok 4294967295u
                "#-2147483648", Ok 2147483648u
                "#-2147483649", Ok 2147483647u
                
            ]


    // Random tests for literals
    // tests 10000 random literals 
    [<Tests>]
    let testLitRan = 
        let genRandTestPairs lstLength =
            let rnd = System.Random()
            List.init lstLength (fun _ -> rnd.Next(-2147483648, 2147483647))
            |> List.map (fun num -> "#" + num.ToString(), uint32 num) 

        makeUnitTestListLit toLit checkLiteral "Random literal tests" (genRandTestPairs 10000)








// test parser



    let ld = {Label = None ; LoadAddr = WA 0u ; OpCode = "" ; Operands = "" ; SymTab = Some testSymTab }

    /// unit tests for parser
    [<Tests>]
    let testParse = 
        makeUnitTestList parse "parse tests" 
            [
                // test valid input

                // tests MOV instruction
                {ld with OpCode = "MOV" ; Operands = "R0, #-1"},
                    Some (Ok {PInstr = {Instruction = MOV
                                        Suff = NA
                                        Dest = Some R0
                                        Op1 =  Ok (Literal 4294967295u)
                                        Op2 = Error ""}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})

                // tests large acceptable input
                {ld with OpCode = "MOV" ; Operands = "R0, #4080"},
                    Some (Ok {PInstr = {Instruction = MOV
                                        Suff = NA
                                        Dest = Some R0
                                        Op1 =  Ok (Literal 4080u)
                                        Op2 = Error ""}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})

                // tests suffix
                {ld with OpCode = "MOVS" ; Operands = "R0, #12"},
                    Some (Ok {PInstr = {Instruction = MOV
                                        Suff = S
                                        Dest = Some R0
                                        Op1 = Ok (Literal 12u)
                                        Op2 = Error ""}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})

                // tests flexible opperator
                {ld with OpCode = "MOVS" ; Operands = "R0, R1, LSL #137"},
                    Some (Ok {PInstr = {Instruction = MOV
                                        Suff = S
                                        Dest = Some R0
                                        Op1 = Ok (RegShiftLit (R1,Lsl,137u))
                                        Op2 = Error ""}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})
                
                // test AND with flexible opperator
                {ld with OpCode = "ANDS" ; Operands = "R0, R1, R7, ASR R8 "},
                    Some (Ok {PInstr = {Instruction = AND
                                        Suff = S
                                        Dest = Some R0
                                        Op1 = Ok (Register R1)
                                        Op2 = Ok (RegShiftReg (R7,Asr,R8))}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})

                // test BIC with flexible opperator
                {ld with OpCode = "BICS" ; Operands = "R12, R3, R7, ROR #8 "},
                    Some (Ok {PInstr = {Instruction = BIC
                                        Suff = S
                                        Dest = Some R12
                                        Op1 = Ok (Register R3)
                                        Op2 = Ok (RegShiftLit (R7,Ror,8u))}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})                              

                // test EOR with flexible opperator RRX
                {ld with OpCode = "EORS" ; Operands = "R0, R1, R7, RRX"},
                    Some (Ok {PInstr = {Instruction = EOR
                                        Suff = S
                                        Dest = Some R0
                                        Op1 = Ok (Register R1)
                                        Op2 = Ok (RegRRX R7)}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})                              



                // testing invalid inputs

                // tests large unacceptable input
                {ld with OpCode = "MOV" ; Operands = "R0, #4081"},
                    Some (Ok {PInstr = {Instruction = MOV
                                        Suff = NA
                                        Dest = Some R0
                                        Op1 = Error "Opperand is not valid literal or register"
                                        Op2 = Error ""}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})


                // tests ,,
                {ld with OpCode = "AND" ; Operands = "R0, , R7, ASR R8 "},Some (Error "Parse error")                            

                //tests invalid destination
                {ld with OpCode = "AND" ; Operands = "R16, #4, R7, RRX"},
                    Some (Ok {PInstr = {Instruction = AND
                                        Suff = NA
                                        Dest = None
                                        Op1 = Ok (Literal 4u)
                                        Op2 = Ok (RegRRX R7)}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})

                //tests invalid first opperand
                {ld with OpCode = "AND" ; Operands = "R0, #4, R7, RRX"},
                    Some (Ok {PInstr = {Instruction = AND
                                        Suff = NA
                                        Dest = Some R0
                                        Op1 = Ok (Literal 4u)
                                        Op2 = Ok (RegRRX R7)}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})


                // tests invalid second opperand
                {ld with OpCode = "AND" ; Operands = "R0, R1, #5, ASR R8 "},
                    Some (Ok {PInstr = {Instruction = AND
                                        Suff = NA
                                        Dest = Some R0
                                        Op1 = Ok (Register R1)
                                        Op2 =  Error "Invalid target register for shift opperator"}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})         


                // tests invalid shift 
                {ld with OpCode = "AND" ; Operands = "R0, R1, R3, LSK R8 "},
                    Some (Ok {PInstr = {Instruction = AND
                                        Suff = NA
                                        Dest = Some R0
                                        Op1 = Ok (Register R1)
                                        Op2 = Error "Invalid shift instruction"}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})


                // test BIC with flexible opperator
                {ld with OpCode = "BICS" ; Operands = "R13, R3, R7, ROR #8 "},
                    Some (Ok {PInstr = {Instruction = BIC
                                        Suff = S
                                        Dest = None
                                        Op1 = Ok (Register R3)
                                        Op2 = Ok (RegShiftLit (R7,Ror,8u))}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})                              
                              


                // LSL test
                {ld with OpCode = "LSL" ; Operands = "R2, R11, #-4"},
                    Some (Ok {PInstr = {Instruction = LSL
                                        Suff = NA
                                        Dest = Some R2
                                        Op1 =  Ok (Register R11)
                                        Op2 = Ok (Literal 4294967292u)}
                              PLabel = None
                              PSize = 4u
                              PCond = Cal})


                // // tests invalid shift opperand
                {ld with OpCode = "AND" ; Operands = "R0, R1, R3, ROR R17, #13 "},Some (Error "Parse error")                                                                                                                  

            ]


    [<Tests>]
    // tests property that ROR by integer multiple of 32 is equal to its selft
    let RORtest = 
        testPropertyWithConfig config  "Property Test for ROR instruction" <| 
        fun n ->
            let rndMultof32 = 32 * System.Random().Next(10)
            let rorBy32 = doShift n Ror (uint32 rndMultof32)
            Expect.equal (fst rorBy32) n "ROR by 32 is equal to its selft"



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

            // AND tests with decimals
            vTest "AND test 1" "AND R2, R1, R0" "0000" [R 0, -2]
            vTest "AND test 2" "AND R3, R2, R0" "0000" [R 0, -2]
            vTest "AND test 3" "AND R2, R2, R0" "0000" [R 2, -138]
            vTest "AND test 4" "AND R3, R3, R3" "0000" [R 3, -4081]  
            ]

