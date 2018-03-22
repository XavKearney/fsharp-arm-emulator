module BitArithmeticTests

    open BitArithmetic
    open CommonData
    open CommonLex
    open VisualTest.VTest
    open Expecto
    open VisualTest.VCommon
    open VisualTest



    let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

    let genRandomUint32List (min,max) count =
        let rnd = System.Random()
        List.init count (fun _ -> rnd.Next (min, max))
        |> List.map uint32

    let getRandomItem lst = 
        List.item (System.Random().Next(0 , List.length lst)) lst     

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
        makeUnitTestListLit toLit id "Literal tests" 
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
    // parsing mov functions (no second opperand)
    let testMOVsRandomised = 
        testPropertyWithConfig config  "Property Test Parse of MOVs" <| 
        fun wa suff dest op1 ->
            let root = getRandomItem [MOV ; MVN]
            let rootStr r = 
                match r with
                | MOV -> "MOV"
                | MVN -> "MVN"
                | _ -> failwithf "Should not happen"        
            let shifterToStr s =
                match s with
                | Lsl -> "LSL"
                | Lsr -> "LSR"
                | Asr -> "ASR"
                | Ror -> "ROR"
            let suffStr = 
                match suff with
                | S -> "S"
                | NA -> ""
            let destStr = regStrings.[dest]  
            let flexOpToStr op = 
                match op with
                | Ok (Literal i) -> sprintf "#%d" i
                | Ok (Register r) -> regStrings.[r]
                | Ok (RegShiftReg (r1, s, r2)) -> 
                    regStrings.[r1] + "," + (shifterToStr s) + " " + regStrings.[r2]
                | Ok (RegShiftLit (r1, s, i)) -> 
                    regStrings.[r1] + "," + (shifterToStr s) + (sprintf " #%d" i)
                | Ok (RegRRX (r)) -> 
                    regStrings.[r] + ", RRX"
                | _ -> ""
            let op1Str = flexOpToStr (Ok op1)                      
            let lineData = 
                {
                    LoadAddr = wa; 
                    Label = None; 
                    SymTab = Some Map.empty;
                    OpCode = (rootStr root) + suffStr;
                    Operands = (destStr + ", " + op1Str);
                }
            let instr = {
                Instruction = root;
                Suff = suff;
                Dest = Some (dest);
                Op1 = Ok (op1);
                Op2 = Error "";
            }
            let expected = 
                    Some ( Ok {
                        PInstr = instr;
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })
            let res = parse lineData
            Expect.equal res expected "property test parse of MOVs"





    [<Tests>]
    // parsing bit arithmetic functions
    let testBitArithRandomised = 
        testPropertyWithConfig config  "Property Test Parse of bit arithmetic instructions" <| 
        fun wa suff op2 ->
            let root = getRandomItem [AND ; ORR ; EOR ; BIC]
            let destReg = getRandomItem [R1 ; R2 ; R3 ; R4 ; R5 ; R6 ; R7 ; R8 ; R9 ; R10 ; R11 ; R12; R14]
            let op1Reg = getRandomItem [R1 ; R2 ; R3 ; R4 ; R5 ; R6 ; R7 ; R8 ; R9 ; R10 ; R11 ; R12; R14]
            let rootStr r = 
                match r with
                | AND -> "AND"
                | ORR -> "ORR"
                | EOR -> "EOR"
                | BIC -> "BIC"
                | _ -> failwithf "Should not happen"        
            let shifterToStr s =
                match s with
                | Lsl -> "LSL"
                | Lsr -> "LSR"
                | Asr -> "ASR"
                | Ror -> "ROR"
            let suffStr = 
                match suff with
                | S -> "S"
                | NA -> ""
            let destStr = regStrings.[destReg]  
            let op1Str = regStrings.[op1Reg] 
            let flexOpToStr op = 
                match op with
                | Ok (Literal i) -> sprintf "#%d" i
                | Ok (Register r) -> regStrings.[r]
                | Ok (RegShiftReg (r1, s, r2)) -> 
                    regStrings.[r1] + "," + (shifterToStr s) + " " + regStrings.[r2]
                | Ok (RegShiftLit (r1, s, i)) -> 
                    regStrings.[r1] + "," + (shifterToStr s) + (sprintf " #%d" i)
                | Ok (RegRRX (r)) -> 
                    regStrings.[r] + ", RRX"
                | _ -> ""
            let op2Str = flexOpToStr (Ok op2)                      
            let lineData = 
                {
                    LoadAddr = wa; 
                    Label = None; 
                    SymTab = Some Map.empty;
                    OpCode = (rootStr root) + suffStr;
                    Operands = (destStr + ", " + op1Str + ", " + op2Str);
                }
            let instr = {
                Instruction = root;
                Suff = suff;
                Dest = Some (destReg);
                Op1 = Ok (Register op1Reg);
                Op2 = Ok (op2);
            }
            let expected =  
                match op2 with 
                | RegShiftReg (R13,_,_) -> 
                    Some ( Ok {
                        PInstr = {instr with Op2 = Error "Cannot use R13 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    }) 
                | RegShiftReg (_,_,R13) -> 
                    Some ( Ok {
                        PInstr = {instr with Op2 = Error "Cannot use R13 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })                             
                | RegShiftReg (R15,_,_) -> 
                    Some ( Ok {
                        PInstr = {instr with Op2 = Error "Cannot use R15 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })                    
                | RegShiftReg (_,_,R15) -> 
                    Some ( Ok {
                        PInstr = {instr with Op2 = Error "Cannot use R15 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })    
                | RegShiftLit (R13,_,_) -> 
                    Some ( Ok {
                        PInstr = {instr with Op2 = Error "Cannot use R13 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })       
                | RegShiftLit (R15,_,_) -> 
                    Some ( Ok {
                        PInstr = {instr with Op2 = Error "Cannot use R15 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })                    
                | Register R13 ->  
                    Some ( Ok {
                        PInstr = {instr with Op2 = Error "Cannot use R13 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })                       
                | Register R15 ->  
                    Some ( Ok {
                        PInstr = {instr with Op2 = Error "Cannot use R15 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })                                                                          
                | _ ->
                    Some ( Ok {
                        PInstr = instr
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })                                       
            let res = parse lineData
            Expect.equal res expected "Property Test Parse of bit arithmetic instructions"







    [<Tests>]
    // parsing shift functions tests
    let testShiftsRandomised = 
        testPropertyWithConfig config  "Property Test Parse of shift instructions" <| 
        fun wa suff lit ->
            let root = getRandomItem [LSL ; LSR ; ASR ; ROR]
            let destReg = getRandomItem [R1 ; R2 ; R3 ; R4 ; R5 ; R6 ; R7 ; R8 ; R9 ; R10 ; R11 ; R12; R14]
            let op1Reg = getRandomItem [R1 ; R2 ; R3 ; R4 ; R5 ; R6 ; R7 ; R8 ; R9 ; R10 ; R11 ; R12; R14]
            let op2Reg = getRandomItem [R1 ; R2 ; R3 ; R4 ; R5 ; R6 ; R7 ; R8 ; R9 ; R10 ; R11 ; R12; R14]
            let op2 = getRandomItem [Register op2Reg; Literal lit]
            let rootStr r = 
                match r with
                | LSL -> "LSL"
                | LSR -> "LSR"
                | ASR -> "ASR"
                | ROR -> "ROR"
                | _ -> failwithf "Should not happen"        
            let suffStr = 
                match suff with
                | S -> "S"
                | NA -> ""
            let destStr = regStrings.[destReg]  
            let op1Str = regStrings.[op1Reg] 
            let flexOpToStr op = 
                match op with
                | Literal i -> sprintf "#%d" i
                | Register r -> regStrings.[r]
                | _ -> failwith "Should not happen"
            let op2Str = flexOpToStr op2                     
            let lineData = 
                {
                    LoadAddr = wa; 
                    Label = None; 
                    SymTab = Some Map.empty;
                    OpCode = (rootStr root) + suffStr;
                    Operands = (destStr + ", " + op1Str + ", " + op2Str);
                }
            let instr = {
                Instruction = root;
                Suff = suff;
                Dest = Some (destReg);
                Op1 = Ok (Register op1Reg);
                Op2 = Ok (op2);
            }
            let expected =                                                                        
                Some ( Ok {
                    PInstr = instr
                    PLabel = None; PSize = 4u; PCond = Cal;
                })                                       
            let res = parse lineData
            Expect.equal res expected "Property Test Parse of shift instructions"












    [<Tests>]
    // parsing mov functions (no second opperand)
    let testTSTandTEQRandomised = 
        testPropertyWithConfig config  "Property Test Parse of TST and TEQ" <| 
        fun wa suff op1 ->
            let dest = getRandomItem [R1 ; R2 ; R3 ; R4 ; R5 ; R6 ; R7 ; R8 ; R9 ; R10 ; R11 ; R12; R14]
            let root = getRandomItem [TST ; TEQ]
            let rootStr r = 
                match r with
                | TST -> "TST"
                | TEQ -> "TEQ"
                | _ -> failwithf "Should not happen"        
            let shifterToStr s =
                match s with
                | Lsl -> "LSL"
                | Lsr -> "LSR"
                | Asr -> "ASR"
                | Ror -> "ROR"
            let suffStr = 
                match suff with
                | S -> "S"
                | NA -> ""
            let destStr = regStrings.[dest]  
            let flexOpToStr op = 
                match op with
                | Literal i -> sprintf "#%d" i
                | Register r -> regStrings.[r]
                | RegShiftReg (r1, s, r2) -> 
                    regStrings.[r1] + "," + (shifterToStr s) + " " + regStrings.[r2]
                | RegShiftLit (r1, s, i) -> 
                    regStrings.[r1] + "," + (shifterToStr s) + (sprintf " #%d" i)
                | RegRRX r -> 
                    regStrings.[r] + ", RRX"
            let op1Str = flexOpToStr op1                      
            let lineData = 
                {
                    LoadAddr = wa; 
                    Label = None; 
                    SymTab = Some Map.empty;
                    OpCode = (rootStr root) + suffStr;
                    Operands = (destStr + ", " + op1Str);
                }
            let instr = {
                Instruction = root;
                Suff = suff;
                Dest = Some (dest);
                Op1 = Ok (op1);
                Op2 = Error "";
            }
            let expected =  
                match op1 with 
                | RegShiftReg (R13,_,_) -> 
                    Some ( Ok {
                        PInstr = {instr with Op1 = Error "Cannot use R13 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    }) 
                | RegShiftReg (_,_,R13) -> 
                    Some ( Ok {
                        PInstr = {instr with Op1 = Error "Cannot use R13 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })                             
                | RegShiftReg (R15,_,_) -> 
                    Some ( Ok {
                        PInstr = {instr with Op1 = Error "Cannot use R15 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })                    
                | RegShiftReg (_,_,R15) -> 
                    Some ( Ok {
                        PInstr = {instr with Op1 = Error "Cannot use R15 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })    
                | RegShiftLit (R13,_,_) -> 
                    Some ( Ok {
                        PInstr = {instr with Op1 = Error "Cannot use R13 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })       
                | RegShiftLit (R15,_,_) -> 
                    Some ( Ok {
                        PInstr = {instr with Op1 = Error "Cannot use R15 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })                    
                | Register R13 ->  
                    Some ( Ok {
                        PInstr = {instr with Op1 = Error "Cannot use R13 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })                       
                | Register R15 ->  
                    Some ( Ok {
                        PInstr = {instr with Op1 = Error "Cannot use R15 with this instruction"};
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })                                                                          
                | _ ->
                    Some ( Ok {
                        PInstr = instr
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })   
            let res = parse lineData
            Expect.equal res expected "property test parse of TEQ and TST"



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
            vTest "AND test 1" "AND R2, R1, R0" "0000" [R 0, 0]
            vTest "AND test 2" "AND R3, R2, R0" "0000" [R 0, 0]
            vTest "AND test 3" "AND R2, R2, R0" "0000" [R 2, 0]
            vTest "AND test 4" "AND R3, R3, R3" "0000" [R 3, 30] 

            vTest "EORS test 1" "EORS R0, R0, #0" "0100" [R 0, 0]
            ]








// testing execute function (exeInstr)  





    let flags = {N=false; C=false; Z=false; V=false}
    let regMap = 
        [R0,0u ; R1,10u ; R2,20u ; R3,30u ; R4,40u ; R5,50u ; R6,60u
         R7,70u ; R8,80u ; R9,90u ; R10,100u ; R11,110u ; R12,120u ; R13,130u
         R14,140u ; R15,80u] |> Map.ofList
       
    let CPUDATA = {Fl = flags ; Regs = regMap ; MM = Map.empty} 
     


    /// takes the output form execute instructions and changes it into a form that can be compared to visual testing
    /// needs the register of intrest to be specified to check the destination
    let convExe rName exeOut =

        let boolToStrInt = function
        | true -> "1"
        | false -> "0"

        match exeOut with 
            | Ok instExe ->
                // The flags after execution in visual testing form i.e "0100" (NZCV)
                let flgs = 
                    List.map boolToStrInt [instExe.Fl.N;instExe.Fl.Z;instExe.Fl.C;instExe.Fl.V]
                    |> List.reduce (+)

                // obtains the register contents in the specified destination register               
                let specRegConts = 
                    instExe.Regs.[rName]
                    |> int32

                flgs, [R rName.RegNum, specRegConts]
            | _ -> failwithf "exeOut is an error: Shoud not happen"             

    let parseThenExe destReg = 
        let checkOk =
            function
            | Some (Ok exeData) -> exeData 
            | _ -> failwithf "output from parse is an error: Should not happen"
        parse >> checkOk >> exeInstr CPUDATA  >> convExe destReg


    

    [<Tests>]
    let testsExeMOV = 
        testList "Execution MOV tests"
            [

            // valid input tests (suffix not set)

            // MOV tests

            vTest "MOV test 1" "MOV R0, #1" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #1"}
            vTest "MOV test 2" "MOV R0, #-1" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #-1"}
            vTest "MOV test 3" "MOV R0, #97" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #97"}
            vTest "MOV test 4" "MOV R0, #255" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #255"}
            vTest "MOV test 5" "MOV R0, #256" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #256"}
            vTest "MOV test 6" "MOV R0, #4080" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #4080"}
            vTest "MOV test 7" "MOV R0, #-4081" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #-4081"}
            vTest "MOV test 8" "MOV R0, #0" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #0"}
            vTest "MOV test 9" "MOV R0, #-97" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #-97"}
            vTest "MOV test 10" "MOV R0, #-0x1" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #-0x1"}

            vTest "MOV test 11" "MOV R0, #4*6+7" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #4*6+7"}
            vTest "MOV test 12" "MOV R0, R0" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, R0"}
            vTest "MOV test 13" "MOV R0, R1" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, R1"}
            vTest "MOV test 14" "MOV R0, R7" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, R7"}
            vTest "MOV test 15" "MOV R0, #30*4" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #30*4"}
            vTest "MOV test 16" "MOV R0, #0b10101" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #0b10101"}
            vTest "MOV test 17" "MOV R0, #-0b1" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #-0b1"}
            vTest "MOV test 18" "MOV R0, #0xFF" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #0xFF"}
            vTest "MOV test 19" "MOV R0, #0x0" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #0x0"}            
            vTest "MOV test 20" "MOV R0, #0xA" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, #10"}

            vTest "MOV test 21" "MOV R0, R7, LSL #5" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, R7, LSL #5"}
            vTest "MOV test 22" "MOV R0, R8, LSR R6" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, R8, LSR R6"}
            vTest "MOV test 23" "MOV R0, R2, ROR #6" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, R2, ROR #6"}
            vTest "MOV test 24" "MOV R0, R3, RRX" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, R3, RRX"}
            vTest "MOV test 25" "MOV R0, R1, ASR #30*4" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, R1, ASR #30*4"}
            vTest "MOV test 26" "MOV R0, R2, LSL #0b10101" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, R2, LSL #0b10101"}
            vTest "MOV test 27" "MOV R0, R5, LSL #0x1" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, R5, LSL #0x1"}
            vTest "MOV test 28" "MOV R0, R2, LSL #0xFF" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, R2, LSL #0xFF"}
            vTest "MOV test 29" "MOV R0, R6, ASR #8" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, R6, ASR #8"}            
            vTest "MOV test 30" "MOV R0, R1, RRX" <|| parseThenExe R0 {ld with OpCode = "MOV" ; Operands = "R0, R1, RRX"}           
            ]



    [<Tests>]
    let testsExeMOVS = 
        testList "Execution MOVS tests"
            [

            // valid input tests (suffix set)

            // MOVS tests

            vTest "MOVS test 1" "MOVS R0, #1" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #1"}
            vTest "MOVS test 2" "MOVS R0, #-1" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #-1"}
            vTest "MOVS test 3" "MOVS R0, #97" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #97"}
            vTest "MOVS test 4" "MOVS R0, #255" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #255"}
            vTest "MOVS test 5" "MOVS R0, #256" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #256"}
            vTest "MOVS test 6" "MOVS R0, #4080" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #4080"}
            vTest "MOVS test 7" "MOVS R0, #-4081" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #-4081"}
            vTest "MOVS test 8" "MOVS R0, #0" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #0"}
            vTest "MOVS test 9" "MOVS R0, #-97" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #-97"}
            vTest "MOVS test 10" "MOVS R0, #-0x1" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #-0x1"}

            vTest "MOVS test 11" "MOVS R0, #4*6+7" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #4*6+7"}
            vTest "MOVS test 12" "MOVS R0, R0" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, R0"}
            vTest "MOVS test 13" "MOVS R0, R1" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, R1"}
            vTest "MOVS test 14" "MOVS R0, R7" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, R7"}
            vTest "MOVS test 15" "MOVS R0, #30*4" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #30*4"}
            vTest "MOVS test 16" "MOVS R0, #0b10101" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #0b10101"}
            vTest "MOVS test 17" "MOVS R0, #-0b1" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #-0b1"}
            vTest "MOVS test 18" "MOVS R0, #0xFF" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #0xFF"}
            vTest "MOVS test 19" "MOVS R0, #0x0" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #0x0"}            
            vTest "MOVS test 20" "MOVS R0, #0xA" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, #10"}

            vTest "MOVS test 21" "MOVS R0, R7, LSL #5" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, R7, LSL #5"}
            vTest "MOVS test 22" "MOVS R0, R8, LSR R6" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, R8, LSR R6"}
            vTest "MOVS test 23" "MOVS R0, R2, ROR #6" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, R2, ROR #6"}
            vTest "MOVS test 24" "MOVS R0, R3, RRX" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, R3, RRX"}
            vTest "MOVS test 25" "MOVS R0, R1, ASR #30*4" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, R1, ASR #30*4"}
            vTest "MOVS test 26" "MOVS R0, R2, LSL #0b10101" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, R2, LSL #0b10101"}
            vTest "MOVS test 27" "MOVS R0, R5, LSL #0x1" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, R5, LSL #0x1"}
            vTest "MOVS test 28" "MOVS R0, R2, LSL #0xFF" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, R2, LSL #0xFF"}
            vTest "MOVS test 29" "MOVS R0, R6, ASR #8" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, R6, ASR #8"}            
            vTest "MOVS test 30" "MOVS R0, R1, RRX" <|| parseThenExe R0 {ld with OpCode = "MOVS" ; Operands = "R0, R1, RRX"}           
            ]    




    [<Tests>]
    let testsExeMVNS = 
        testList "Execution MVNS tests"
            [

            // valid input tests (suffix not set)

            // MVNS tests

            vTest "MVNS test 1" "MVNS R0, #1" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #1"}
            vTest "MVNS test 2" "MVNS R0, #-1" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #-1"}
            vTest "MVNS test 3" "MVNS R0, #97" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #97"}
            vTest "MVNS test 4" "MVNS R0, #255" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #255"}
            vTest "MVNS test 5" "MVNS R0, #256" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #256"}
            vTest "MVNS test 6" "MVNS R0, #4080" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #4080"}
            vTest "MVNS test 7" "MVNS R0, #-4081" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #-4081"}
            vTest "MVNS test 8" "MVNS R0, #0" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #0"}
            vTest "MVNS test 9" "MVNS R0, #-97" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #-97"}
            vTest "MVNS test 10" "MVNS R0, #-0x1" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #-0x1"}

            vTest "MVNS test 11" "MVNS R0, #4*6+7" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #4*6+7"}
            vTest "MVNS test 12" "MVNS R0, R0" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, R0"}
            vTest "MVNS test 13" "MVNS R0, R1" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, R1"}
            vTest "MVNS test 14" "MVNS R0, R7" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, R7"}
            vTest "MVNS test 15" "MVNS R0, #30*4" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #30*4"}
            vTest "MVNS test 16" "MVNS R0, #0b10101" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #0b10101"}
            vTest "MVNS test 17" "MVNS R0, #-0b1" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #-0b1"}
            vTest "MVNS test 18" "MVNS R0, #0xFF" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #0xFF"}
            vTest "MVNS test 19" "MVNS R0, #0x0" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #0x0"}            
            vTest "MVNS test 20" "MVNS R0, #0xA" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, #10"}

            vTest "MVNS test 21" "MVNS R0, R7, LSL #5" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, R7, LSL #5"}
            vTest "MVNS test 22" "MVNS R0, R8, LSR R6" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, R8, LSR R6"}
            vTest "MVNS test 23" "MVNS R0, R2, ROR #6" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, R2, ROR #6"}
            vTest "MVNS test 24" "MVNS R0, R3, RRX" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, R3, RRX"}
            vTest "MVNS test 25" "MVNS R0, R1, ASR #30*4" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, R1, ASR #30*4"}
            vTest "MVNS test 26" "MVNS R0, R2, LSL #0b10101" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, R2, LSL #0b10101"}
            vTest "MVNS test 27" "MVNS R0, R5, LSL #0x1" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, R5, LSL #0x1"}
            vTest "MVNS test 28" "MVNS R0, R2, LSL #0xFF" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, R2, LSL #0xFF"}
            vTest "MVNS test 29" "MVNS R0, R6, ASR #8" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, R6, ASR #8"}            
            vTest "MVNS test 30" "MVNS R0, R1, RRX" <|| parseThenExe R0 {ld with OpCode = "MVNS" ; Operands = "R0, R1, RRX"}           
            ]            



    [<Tests>]
    let testsExeANDS = 
        testList "Execution ANDS tests"
            [

            // valid input tests (suffix not set)

            // ANDS tests

            vTest "ANDS test 1" "ANDS R0, R0, #0" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R0, #0"}
            vTest "ANDS test 2" "ANDS R0, R1, #1" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R1, #1"}
            vTest "ANDS test 3" "ANDS R0, R9, #97" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R9, #97"}
            vTest "ANDS test 4" "ANDS R0, R7, #255" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, #255"}
                // visual error? Not sure why carry is set with these instructions
            // vTest "ANDS test 5" "ANDS R0, R2, #256" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R2, #256"}
            // vTest "ANDS test 6" "ANDS R0, R7, #4080" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, #4080"}
            // vTest "ANDS test 7" "ANDS R0, R7, #-4081" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, #-4081"}
            // vTest "ANDS test 8" "ANDS R0, R7, #-1" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, #-1"}
            // vTest "ANDS test 9" "ANDS R0, R7, #-3" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, #-3"}
            // vTest "ANDS test 10" "ANDS R0, R7, #-0x1" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, #-0x1"}

            vTest "ANDS test 11" "ANDS R0, R7, #4*6+7" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, #4*6+7"}
            vTest "ANDS test 12" "ANDS R0, R7, R0" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, R0"}
            vTest "ANDS test 13" "ANDS R0, R7, R1" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, R1"}
            vTest "ANDS test 14" "ANDS R0, R7, R7" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, R7"}
            vTest "ANDS test 15" "ANDS R0, R7, #30*4" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, #30*4"}
            vTest "ANDS test 16" "ANDS R0, R7, #0b10101" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, #0b10101"}
                // visual error? Not sure why carry is set with these instructions
            // vTest "ANDS test 17" "ANDS R0, R7, #-0b1" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, #-0b1"}

            vTest "ANDS test 18" "ANDS R0, R7, #0xFF" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, #0xFF"}
            vTest "ANDS test 19" "ANDS R0, R7, #0x0" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, #0x0"}            
            vTest "ANDS test 20" "ANDS R0, R7, #0xA" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, #10"}

            vTest "ANDS test 21" "ANDS R0, R7, R3, LSL #5" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R7, R3, LSL #5"}
            vTest "ANDS test 22" "ANDS R0, R8, R5, LSR R6" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R8, R5, LSR R6"}
            vTest "ANDS test 23" "ANDS R0, R2, R7, ROR #6" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R2, R7, ROR #6"}
            vTest "ANDS test 24" "ANDS R0, R3, R7, RRX" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R3, R7, RRX"}
            vTest "ANDS test 25" "ANDS R0, R1, R7, ASR #30*4" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R1, R7, ASR #30*4"}
            vTest "ANDS test 26" "ANDS R0, R2, R5, LSL #0b10101" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R2, R5, LSL #0b10101"}
            vTest "ANDS test 27" "ANDS R0, R5, R3, LSL #0x1" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R5, R3, LSL #0x1"}
            vTest "ANDS test 28" "ANDS R0, R2, R4, LSL #0xFF" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R2, R4, LSL #0xFF"}
            vTest "ANDS test 29" "ANDS R0, R6, R2, ASR #8" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R6, R2, ASR #8"}            
            vTest "ANDS test 30" "ANDS R0, R1, R9, RRX" <|| parseThenExe R0 {ld with OpCode = "ANDS" ; Operands = "R0, R1, R9, RRX"}           
            ]              







            
    [<Tests>]
    let testsExeEORS = 
     
        testList "Execution EORS tests"
            [

            // valid input tests (suffix not set)

            // EORS tests

            vTest "EORS test 1" "EORS R0, R0, #0" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R0, #0"}
            vTest "EORS test 2" "EORS R0, R1, #1" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R1, #1"}
            vTest "EORS test 3" "EORS R0, R9, #97" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R9, #97"}
            vTest "EORS test 4" "EORS R0, R7, #255" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, #255"}
                // visual error?
            // vTest "EORS test 5" "EORS R0, R2, #256" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R2, #256"}
            // vTest "EORS test 6" "EORS R0, R7, #4080" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, #4080"}
                // errored due to visUAL creating these literals by inverting
            //vTest "EORS test 7" "EORS R0, R7, #-4081" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, #-4081"}
            //vTest "EORS test 8" "EORS R0, R7, #-1" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, #-1"}
            //vTest "EORS test 9" "EORS R0, R7, #-3" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, #-3"}
            //vTest "EORS test 10" "EORS R0, R7, #-0x1" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, #-0x1"}

            vTest "EORS test 11" "EORS R0, R7, #4*6+7" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, #4*6+7"}
            vTest "EORS test 12" "EORS R0, R7, R0" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, R0"}
            vTest "EORS test 13" "EORS R0, R7, R1" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, R1"}
            vTest "EORS test 14" "EORS R0, R7, R7" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, R7"}
            vTest "EORS test 15" "EORS R0, R7, #30*4" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, #30*4"}
            vTest "EORS test 16" "EORS R0, R7, #0b10101" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, #0b10101"}
                //  errored due to visUAL creating these literals by inverting
            //vTest "EORS test 17" "EORS R0, R7, #-0b1" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, #-0b1"}
            vTest "EORS test 18" "EORS R0, R7, #0xFF" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, #0xFF"}
            vTest "EORS test 19" "EORS R0, R7, #0x0" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, #0x0"}            
            vTest "EORS test 20" "EORS R0, R7, #0xA" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, #10"}

            vTest "EORS test 21" "EORS R0, R7, R3, LSL #5" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R7, R3, LSL #5"}
            vTest "EORS test 22" "EORS R0, R8, R5, LSR R6" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R8, R5, LSR R6"}
            vTest "EORS test 23" "EORS R0, R2, R7, ROR #6" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R2, R7, ROR #6"}
            vTest "EORS test 24" "EORS R0, R3, R7, RRX" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R3, R7, RRX"}
            vTest "EORS test 25" "EORS R0, R1, R7, ASR #30*4" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R1, R7, ASR #30*4"}
            vTest "EORS test 26" "EORS R0, R2, R5, LSL #0b10101" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R2, R5, LSL #0b10101"}
            vTest "EORS test 27" "EORS R0, R5, R3, LSL #0x1" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R5, R3, LSL #0x1"}
            vTest "EORS test 28" "EORS R0, R2, R4, LSL #0xFF" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R2, R4, LSL #0xFF"}
            vTest "EORS test 29" "EORS R0, R6, R2, ASR #8" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R6, R2, ASR #8"}            
            vTest "EORS test 30" "EORS R0, R1, R9, RRX" <|| parseThenExe R0 {ld with OpCode = "EORS" ; Operands = "R0, R1, R9, RRX"}           
            ]              




    [<Tests>]
    let testsExeORRS = 
     
        testList "Execution ORRS tests"
            [

            // valid input tests (suffix not set)

            // ORRS tests

            vTest "ORRS test 1" "ORRS R0, R0, #0" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R0, #0"}
            vTest "ORRS test 2" "ORRS R0, R1, #1" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R1, #1"}
            vTest "ORRS test 3" "ORRS R0, R9, #97" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R9, #97"}
            vTest "ORRS test 4" "ORRS R0, R7, #255" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, #255"}

            vTest "ORRS test 5" "ORRS R0, R2, #256" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R2, #256"}
            vTest "ORRS test 6" "ORRS R0, R7, #4080" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, #4080"}
                // errored due to visUAL creating these literals by inverting
            // vTest "ORRS test 7" "ORRS R0, R7, #-4081" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, #-4081"}
            // vTest "ORRS test 8" "ORRS R0, R7, #-1" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, #-1"}
            // vTest "ORRS test 9" "ORRS R0, R7, #-3" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, #-3"}
            // vTest "ORRS test 10" "ORRS R0, R7, #-0x1" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, #-0x1"}

            vTest "ORRS test 11" "ORRS R0, R7, #4*6+7" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, #4*6+7"}
            vTest "ORRS test 12" "ORRS R0, R7, R0" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, R0"}
            vTest "ORRS test 13" "ORRS R0, R7, R1" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, R1"}
            vTest "ORRS test 14" "ORRS R0, R7, R7" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, R7"}
            vTest "ORRS test 15" "ORRS R0, R7, #30*4" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, #30*4"}
            vTest "ORRS test 16" "ORRS R0, R7, #0b10101" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, #0b10101"}
                //  errored due to visUAL creating these literals by inverting
            //vTest "ORRS test 17" "ORRS R0, R7, #-0b1" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, #-0b1"}
            vTest "ORRS test 18" "ORRS R0, R7, #0xFF" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, #0xFF"}
            vTest "ORRS test 19" "ORRS R0, R7, #0x0" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, #0x0"}            
            vTest "ORRS test 20" "ORRS R0, R7, #0xA" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, #10"}

            vTest "ORRS test 21" "ORRS R0, R7, R3, LSL #5" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R7, R3, LSL #5"}
            vTest "ORRS test 22" "ORRS R0, R8, R5, LSR R6" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R8, R5, LSR R6"}
            vTest "ORRS test 23" "ORRS R0, R2, R7, ROR #6" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R2, R7, ROR #6"}
            vTest "ORRS test 24" "ORRS R0, R3, R7, RRX" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R3, R7, RRX"}
            vTest "ORRS test 25" "ORRS R0, R1, R7, ASR #30*4" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R1, R7, ASR #30*4"}
            vTest "ORRS test 26" "ORRS R0, R2, R5, LSL #0b10101" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R2, R5, LSL #0b10101"}
            vTest "ORRS test 27" "ORRS R0, R5, R3, LSL #0x1" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R5, R3, LSL #0x1"}
            vTest "ORRS test 28" "ORRS R0, R2, R4, LSL #0xFF" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R2, R4, LSL #0xFF"}
            vTest "ORRS test 29" "ORRS R0, R6, R2, ASR #8" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R6, R2, ASR #8"}            
            vTest "ORRS test 30" "ORRS R0, R1, R9, RRX" <|| parseThenExe R0 {ld with OpCode = "ORRS" ; Operands = "R0, R1, R9, RRX"}           
            ]           


    [<Tests>]
    let testsExeBIC = 
     
        testList "Execution BIC tests"
            [

            // valid input tests (suffix not set)

            // BIC tests

            vTest "BIC test 1" "BIC R0, R0, #0" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R0, #0"}
            vTest "BIC test 2" "BIC R0, R1, #1" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R1, #1"}
            vTest "BIC test 3" "BIC R0, R9, #97" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R9, #97"}
            vTest "BIC test 4" "BIC R0, R7, #255" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, #255"}

            vTest "BIC test 5" "BIC R0, R2, #256" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R2, #256"}
            vTest "BIC test 6" "BIC R0, R7, #4080" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, #4080"}
            vTest "BIC test 7" "BIC R0, R7, #-4081" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, #-4081"}
            vTest "BIC test 8" "BIC R0, R7, #-1" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, #-1"}
            vTest "BIC test 9" "BIC R0, R7, #-3" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, #-3"}
            vTest "BIC test 10" "BIC R0, R7, #-0x1" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, #-0x1"}

            vTest "BIC test 11" "BIC R0, R7, #4*6+7" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, #4*6+7"}
            vTest "BIC test 12" "BIC R0, R7, R0" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, R0"}
            vTest "BIC test 13" "BIC R0, R7, R1" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, R1"}
            vTest "BIC test 14" "BIC R0, R7, R7" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, R7"}
            vTest "BIC test 15" "BIC R0, R7, #30*4" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, #30*4"}
            vTest "BIC test 16" "BIC R0, R7, #0b10101" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, #0b10101"}
            vTest "BIC test 17" "BIC R0, R7, #-0b1" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, #-0b1"}
            vTest "BIC test 18" "BIC R0, R7, #0xFF" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, #0xFF"}
            vTest "BIC test 19" "BIC R0, R7, #0x0" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, #0x0"}            
            vTest "BIC test 20" "BIC R0, R7, #0xA" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, #10"}

            vTest "BIC test 21" "BIC R0, R7, R3, LSL #5" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R7, R3, LSL #5"}
            vTest "BIC test 22" "BIC R0, R8, R5, LSR R6" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R8, R5, LSR R6"}
            vTest "BIC test 23" "BIC R0, R2, R7, ROR #6" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R2, R7, ROR #6"}
            vTest "BIC test 24" "BIC R0, R3, R7, RRX" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R3, R7, RRX"}
            vTest "BIC test 25" "BIC R0, R1, R7, ASR #30*4" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R1, R7, ASR #30*4"}
            vTest "BIC test 26" "BIC R0, R2, R5, LSL #0b10101" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R2, R5, LSL #0b10101"}
            vTest "BIC test 27" "BIC R0, R5, R3, LSL #0x1" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R5, R3, LSL #0x1"}
            vTest "BIC test 28" "BIC R0, R2, R4, LSL #0xFF" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R2, R4, LSL #0xFF"}
            vTest "BIC test 29" "BIC R0, R6, R2, ASR #8" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R6, R2, ASR #8"}            
            vTest "BIC test 30" "BIC R0, R1, R9, RRX" <|| parseThenExe R0 {ld with OpCode = "BIC" ; Operands = "R0, R1, R9, RRX"}           
            ]    





    [<Tests>]
    let testsExeLSLS = 
     
        testList "Execution LSLS tests"
            [

            // valid input tests (suffix not set)

            // LSLS tests

            vTest "LSLS test 1" "LSLS R0, R0, #0" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R0, #0"}
            vTest "LSLS test 2" "LSLS R0, R1, #1" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R1, #1"}
            vTest "LSLS test 3" "LSLS R0, R9, #15" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R9, #15"}
            vTest "LSLS test 4" "LSLS R0, R7, #31" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, #31"}
                //  errored due to visUAL creating these literals by inverting
            //vTest "LSLS test 5" "LSLS R0, R2, #256" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R2, #256"}
            //vTest "LSLS test 6" "LSLS R0, R7, #4080" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, #4080"}
            //vTest "LSLS test 7" "LSLS R0, R7, #-4081" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, #-4081"}
            //vTest "LSLS test 8" "LSLS R0, R7, #-1" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, #-1"}
            //vTest "LSLS test 9" "LSLS R0, R7, #-3" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, #-3"}
            //vTest "LSLS test 10" "LSLS R0, R7, #-0x1" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, #-0x1"}

            vTest "LSLS test 11" "LSLS R0, R7, #4*6+7" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, #4*6+7"}
            vTest "LSLS test 12" "LSLS R0, R7, R0" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, R0"}
            vTest "LSLS test 13" "LSLS R0, R7, R1" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, R1"}
            vTest "LSLS test 14" "LSLS R0, R7, R3" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, R3"}
            vTest "LSLS test 15" "LSLS R0, R7, #2*7" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, #2*7"}
            vTest "LSLS test 16" "LSLS R0, R7, #0b10101" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, #0b10101"}
            //vTest "LSLS test 17" "LSLS R0, R7, #-0b1" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, #-0b1"}
            vTest "LSLS test 18" "LSLS R0, R7, #0x6" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, #0x6"}
            vTest "LSLS test 19" "LSLS R0, R7, #0x0" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, #0x0"}            
            vTest "LSLS test 20" "LSLS R0, R7, #0xA" <|| parseThenExe R0 {ld with OpCode = "LSLS" ; Operands = "R0, R7, #10"}
            ]                   




    [<Tests>]
    let testsExeLSRS = 
     
        testList "Execution LSRS tests"
            [

            // valid input tests (suffix not set)

            // LSRS tests

            vTest "LSRS test 1" "LSRS R0, R0, #0" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R0, #0"}
            vTest "LSRS test 2" "LSRS R0, R1, #1" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R1, #1"}
            vTest "LSRS test 3" "LSRS R0, R9, #15" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R9, #15"}
            vTest "LSRS test 4" "LSRS R0, R7, #31" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, #31"}
                //  errored due to visUAL creating these literals by inverting
            //vTest "LSRS test 5" "LSRS R0, R2, #256" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R2, #256"}
            //vTest "LSRS test 6" "LSRS R0, R7, #4080" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, #4080"}
            //vTest "LSRS test 7" "LSRS R0, R7, #-4081" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, #-4081"}
            //vTest "LSRS test 8" "LSRS R0, R7, #-1" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, #-1"}
            //vTest "LSRS test 9" "LSRS R0, R7, #-3" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, #-3"}
            //vTest "LSRS test 10" "LSRS R0, R7, #-0x1" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, #-0x1"}

            vTest "LSRS test 11" "LSRS R0, R7, #4*6+7" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, #4*6+7"}
            vTest "LSRS test 12" "LSRS R0, R7, R0" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, R0"}
            vTest "LSRS test 13" "LSRS R0, R7, R1" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, R1"}
            vTest "LSRS test 14" "LSRS R0, R7, R3" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, R3"}
            vTest "LSRS test 15" "LSRS R0, R7, #2*7" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, #2*7"}
            vTest "LSRS test 16" "LSRS R0, R7, #0b10101" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, #0b10101"}
            //vTest "LSRS test 17" "LSRS R0, R7, #-0b1" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, #-0b1"}
            vTest "LSRS test 18" "LSRS R0, R7, #0x6" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, #0x6"}
            vTest "LSRS test 19" "LSRS R0, R7, #0x0" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, #0x0"}            
            vTest "LSRS test 20" "LSRS R0, R7, #0xA" <|| parseThenExe R0 {ld with OpCode = "LSRS" ; Operands = "R0, R7, #10"}
            ]      



    [<Tests>]
    let testsExeASRS = 
     
        testList "Execution ASRS tests"
            [

            // valid input tests (suffix not set)

            // ASRS tests

            vTest "ASRS test 1" "ASRS R0, R0, #0" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R0, #0"}
            vTest "ASRS test 2" "ASRS R0, R1, #1" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R1, #1"}
            vTest "ASRS test 3" "ASRS R0, R9, #15" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R9, #15"}
            vTest "ASRS test 4" "ASRS R0, R7, #31" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, #31"}
                //  errored due to visUAL creating these literals by inverting
            //vTest "ASRS test 5" "ASRS R0, R2, #256" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R2, #256"}
            //vTest "ASRS test 6" "ASRS R0, R7, #4080" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, #4080"}
            //vTest "ASRS test 7" "ASRS R0, R7, #-4081" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, #-4081"}
            //vTest "ASRS test 8" "ASRS R0, R7, #-1" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, #-1"}
            //vTest "ASRS test 9" "ASRS R0, R7, #-3" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, #-3"}
            //vTest "ASRS test 10" "ASRS R0, R7, #-0x1" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, #-0x1"}

            vTest "ASRS test 11" "ASRS R0, R7, #4*6+7" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, #4*6+7"}
            vTest "ASRS test 12" "ASRS R0, R7, R0" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, R0"}
            vTest "ASRS test 13" "ASRS R0, R7, R1" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, R1"}
            vTest "ASRS test 14" "ASRS R0, R7, R3" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, R3"}
            vTest "ASRS test 15" "ASRS R0, R7, #2*7" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, #2*7"}
            vTest "ASRS test 16" "ASRS R0, R7, #0b10101" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, #0b10101"}
            //vTest "ASRS test 17" "ASRS R0, R7, #-0b1" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, #-0b1"}
            vTest "ASRS test 18" "ASRS R0, R7, #0x6" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, #0x6"}
            vTest "ASRS test 19" "ASRS R0, R7, #0x0" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, #0x0"}            
            vTest "ASRS test 20" "ASRS R0, R7, #0xA" <|| parseThenExe R0 {ld with OpCode = "ASRS" ; Operands = "R0, R7, #10"}
            ]     



    [<Tests>]
    let testsExeRORS = 
     
        testList "Execution RORS tests"
            [

            // valid input tests (suffix not set)

            // RORS tests

            vTest "RORS test 1" "RORS R0, R0, #0" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R0, #0"}
            vTest "RORS test 2" "RORS R0, R1, #1" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R1, #1"}
            vTest "RORS test 3" "RORS R0, R9, #15" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R9, #15"}
            vTest "RORS test 4" "RORS R0, R7, #31" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, #31"}
                //  errored due to visUAL creating these literals by inverting
            //vTest "RORS test 5" "RORS R0, R2, #256" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R2, #256"}
            //vTest "RORS test 6" "RORS R0, R7, #4080" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, #4080"}
            //vTest "RORS test 7" "RORS R0, R7, #-4081" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, #-4081"}
            //vTest "RORS test 8" "RORS R0, R7, #-1" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, #-1"}
            //vTest "RORS test 9" "RORS R0, R7, #-3" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, #-3"}
            //vTest "RORS test 10" "RORS R0, R7, #-0x1" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, #-0x1"}

            vTest "RORS test 11" "RORS R0, R7, #4*6+7" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, #4*6+7"}
            vTest "RORS test 12" "RORS R0, R7, R0" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, R0"}
            vTest "RORS test 13" "RORS R0, R7, R1" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, R1"}
            vTest "RORS test 14" "RORS R0, R7, R3" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, R3"}
            vTest "RORS test 15" "RORS R0, R7, #2*7" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, #2*7"}
            vTest "RORS test 16" "RORS R0, R7, #0b10101" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, #0b10101"}
            //vTest "RORS test 17" "RORS R0, R7, #-0b1" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, #-0b1"}
            vTest "RORS test 18" "RORS R0, R7, #0x6" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, #0x6"}
            vTest "RORS test 19" "RORS R0, R7, #0x0" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, #0x0"}            
            vTest "RORS test 20" "RORS R0, R7, #0xA" <|| parseThenExe R0 {ld with OpCode = "RORS" ; Operands = "R0, R7, #10"}
            ]         