module TopLevelTests

    open Expecto
    open CommonData
    open CommonLex
    open Arithmetic
    open Mem
    open MultMem
    open Emulator.TopLevel

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

    /// takes a function f, and a list of two parameters
    /// it applies (f inp param1 param2) where twoParams = (param1, param2)
    /// creates an expecto testlist with unit tests testing each case
    let makeUnitTestListWithTwoParams f twoParams name inOutLst=
        let makeTest inp outp =
            let testName = (sprintf "%s: %A" name inp)
            testCase testName <| fun () ->
                Expect.equal (f inp <|| twoParams) outp testName
        List.map (fun (i, o) -> makeTest i o) inOutLst
        |> testList (sprintf "%s Test List" name) 

    /// takes a function f, and a list of two parameters
    /// it applies (f inp param1 param2) where twoParams = (param1, param2)
    /// creates an expecto testlist with unit tests testing each case
    let makeUnitTestListWithThreeParams f threeParams name inOutLst=
        let makeTest inp outp =
            let testName = (sprintf "%s: %A" name inp)
            testCase testName <| fun () ->
                Expect.equal (f inp <||| threeParams <| true) outp testName
        List.map (fun (i, o) -> makeTest i o) inOutLst
        |> testList (sprintf "%s Test List" name) 

    /// tests parsing of individual lines 
    [<Tests>]
    let testParseLine = 
        makeUnitTestList (parseLine (Map.empty) (WA 0u)) "Unit Test parseLine" [
            // test valid instructions
            "ADD R0, R0, #5", 
            Ok {PInstr = IARITH (ArithI {InstrType = Some ADD; SuffixSet = false;
                             Target = R0; Op1 = R0; Op2 = Literal 5u;});
                PLabel = None; PSize = 4u; PCond = Cal;}
            "add R0, R0, #5", 
            Ok {PInstr = IARITH (ArithI {InstrType = Some ADD; SuffixSet = false;
                             Target = R0; Op1 = R0; Op2 = Literal 5u;});
                PLabel = None; PSize = 4u; PCond = Cal;}
            "ADD r0, r0, #5", 
            Ok {PInstr = IARITH (ArithI {InstrType = Some ADD; SuffixSet = false;
                             Target = R0; Op1 = R0; Op2 = Literal 5u;});
                PLabel = None; PSize = 4u; PCond = Cal;}
            "add r0, r0, #5", 
            Ok {PInstr = IARITH (ArithI {InstrType = Some ADD; SuffixSet = false;
                             Target = R0; Op1 = R0; Op2 = Literal 5u;});
                PLabel = None; PSize = 4u; PCond = Cal;}
            "ADR R0, 4", 
                Ok {PInstr = IMEM (AdrO (Ok {InstructionType = ADRm;
                                                 DestReg = R0;
                                                 SecondOp = 4u;}));
                    PLabel = None;PSize = 4u;PCond = Cal;}

            "LDM R0, {R1,R2,R3}", 
            Ok {PInstr = IMULTMEM (MemI {InsType = Some(LDM); Direction = Some(FD);
                                Target = R0; WriteBack = false; RegList = [R1;R2;R3]});
                PLabel = None; PCond = Cal; PSize = 4u;}

            // test labels
            "someLabel ADDS R1, R2, #93", 
            Ok {PInstr = IARITH (ArithI {InstrType = Some ADD; SuffixSet = true;
                             Target = R1; Op1 = R2; Op2 = Literal 93u;});
                PLabel = Some ("someLabel", 0u); PSize = 4u; PCond = Cal;}

            "l4belw1thnum5 STM R0, {R1,R2,R3}", 
            Ok {PInstr = IMULTMEM (MemI {InsType = Some(STM); Direction = Some(EA);
                                Target = R0; WriteBack = false; RegList = [R1;R2;R3]});
                PLabel = Some ("l4belw1thnum5", 0u); PCond = Cal; PSize = 4u;}

            // test conditions
            "testlab STMEQ R0, {R1,R2,R3}", 
            Ok {PInstr = IMULTMEM (MemI {InsType = Some(STM); Direction = Some(EA);
                                Target = R0; WriteBack = false; RegList = [R1;R2;R3]});
                PLabel = Some ("testlab", 0u); PCond = Ceq; PSize = 4u;}

            "ADDSNE R1, R2, #93", 
            Ok {PInstr = IARITH (ArithI {InstrType = Some ADD; SuffixSet = true;
                             Target = R1; Op1 = R2; Op2 = Literal 93u;});
                PLabel = None; PSize = 4u; PCond = Cne;}
            
            "", Ok {PInstr = BLANKLINE; PLabel = None; PSize = 0u; PCond = Cal;}

            "ANDS		R0, R7, R3, LSL #5",
            Ok {PInstr = IBITARITH ({Instruction = BitArithmetic.AND;
                              Suff = BitArithmetic.S;
                              Dest = Some R0;
                              Op1 = Ok (BitArithmetic.Register R7);
                              Op2 = Ok (BitArithmetic.RegShiftLit (R3,BitArithmetic.Lsl,5u));})
                PLabel = None; PSize = 4u; PCond = Cal;}

            "ANDS R0, R7, R3, LSL #5",
            Ok {PInstr = IBITARITH ({Instruction = BitArithmetic.AND;
                              Suff = BitArithmetic.S;
                              Dest = Some R0;
                              Op1 = Ok (BitArithmetic.Register R7);
                              Op2 = Ok (BitArithmetic.RegShiftLit (R3,BitArithmetic.Lsl,5u));})
                PLabel = None; PSize = 4u; PCond = Cal;}

            "ANDS r0, r7, r3, LsL #5",
            Ok {PInstr = IBITARITH ({Instruction = BitArithmetic.AND;
                              Suff = BitArithmetic.S;
                              Dest = Some R0;
                              Op1 = Ok (BitArithmetic.Register R7);
                              Op2 = Ok (BitArithmetic.RegShiftLit (R3,BitArithmetic.Lsl,5u));})
                PLabel = None; PSize = 4u; PCond = Cal;}

            // test invalid instructions
            "NOTANOPCODE R1, R2, #93", 
            Error (ERRTOPLEVEL "Instruction not implemented: NOTANOPCODE R1, R2, #93")
            "blah", Error (ERRTOPLEVEL "Invalid instruction: blah")
        ]

    // execution of individual parsed lines for every supported instruction
    [<Tests>]
    let testExecParsedLine = 
        let removeResult x = 
            match x with
            | Ok y -> y
            | Error _ -> failwithf "Should never happen."
        let cpuData = 
            match initDataPath None None None with
            | Ok x -> x
            | Error _ -> failwithf "Should never happen."
        let symtab = ["test", 0u] |> Map.ofList
        let getParsed f ld = 
            match f ld with
            | Some x -> x
            | None -> failwithf "Should never happen."
        makeUnitTestListWithThreeParams (removeResult >> execParsedLine) (cpuData, symtab, 0u) "Unit Test execParsedLine" [
            // test valid lines
            (parseLine (symtab) (WA 0u) "ADD R0, R0, #1"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 1u)}, symtab)
            (parseLine (symtab) (WA 0u) "SUB R0, R0, #1"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 0u-1u)}, symtab)
            (parseLine (symtab) (WA 0u) "ADC R0, R0, #0xFF"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 0xFFu)}, symtab)
            (parseLine (symtab) (WA 0u) "SBC R0, R0, #0xFF"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 0u-1u-0xFFu)}, symtab)
            (parseLine (symtab) (WA 0u) "RSB R0, R0, #&FF"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 0xFFu)}, symtab)
            (parseLine (symtab) (WA 0u) "RSC R0, R0, #0b11"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 0b11u-1u)}, symtab)
            (parseLine (symtab) (WA 0u) "CMP R0, #0b0000"), 
                Ok ({cpuData with Fl = {N=false;Z=true;V=false;C=false;}}, symtab)
            (parseLine (symtab) (WA 0u) "CMN R0, #0b0000"), 
                Ok ({cpuData with Fl = {N=false;Z=true;V=false;C=false;}}, symtab)

            // test BitArithmetic instructions
            (parseLine (symtab) (WA 0u) "MOV R0, #&DE"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 0xDEu)}, symtab)
            (parseLine (symtab) (WA 0u) "MVN R0, #&DE"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, ~~~0xDEu)}, symtab)
            (parseLine (symtab) (WA 0u) "AND R0, R0, #0xFF"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 0u)}, symtab)
            (parseLine (symtab) (WA 0u) "ORR R0, R0, #0xFA"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 0xFAu)}, symtab)
            (parseLine (symtab) (WA 0u) "EOR R0, R0, #0b1111"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 0b1111u)}, symtab)
            (parseLine (symtab) (WA 0u) "BIC R0, R0, #0b1111"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 0u)}, symtab)
            (parseLine (symtab) (WA 0u) "LSL R0, R0, #3"), 
                Ok (cpuData, symtab)
            (parseLine (symtab) (WA 0u) "LSR R0, R0, #0xAE"), 
                Ok (cpuData, symtab)
            (parseLine (symtab) (WA 0u) "ASR R0, R0, #0xFF"), 
                Ok (cpuData, symtab)
            (parseLine (symtab) (WA 0u) "ROR R0, R0, #0b1101"), 
                Ok (cpuData, symtab)
            (parseLine (symtab) (WA 0u) "RRX R0, R0"), 
                Ok (cpuData, symtab)
            (parseLine (symtab) (WA 0u) "TST R0, #0b1111"), 
                Ok ({cpuData with Fl = {cpuData.Fl with Z=true}}, symtab)
            (parseLine (symtab) (WA 0u) "TEQ R0, #0"), 
                Ok ({cpuData with Fl = {cpuData.Fl with Z=true}}, symtab)

            // test Mem instructions
            (parseLine (symtab) (WA 0u) "LDR R0, [R1]"), 
                Error (ERRLINE (ERRIMEM "execLDR-interpretingRecord: Error accesing memory location", 0u))
            (parseLine (symtab) (WA 0u) "STR R0, [R1]"), 
                Ok ({cpuData with MM = cpuData.MM.Add (WA 0u, DataLoc 0u)}, symtab)
            (parseLine (symtab) (WA 0u) "ADR R0, 4"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 4u)}, symtab)
            (parseLine (symtab) (WA 0u) "test DCD 1"), 
                Ok ({cpuData with MM = cpuData.MM.Add (WA (minDataMemAddress+4u), DataLoc 1u)}, symtab.Add ("test", (minDataMemAddress+4u)))
            (parseLine (symtab) (WA 0u) "test EQU 44"), 
                Ok (cpuData, symtab.Add ("test", 44u))
            (parseLine (symtab) (WA 0u) "test FILL 4"), 
                Ok ({cpuData with MM = cpuData.MM.Add (WA (minDataMemAddress+4u), DataLoc 0u)}, symtab.Add ("test", (minDataMemAddress+4u)))

            // test MultMem instructions
            (parseLine (symtab) (WA 0u) "STM R0, {R1}"), 
                Ok ({cpuData with MM = cpuData.MM.Add (WA 0u, DataLoc 0u)}, symtab)
            (parseLine (symtab) (WA 0u) "LDM R0, {R1}"), 
                Error (ERRLINE (ERRIMULTMEM "Invalid memory address.", 0u))

            (parseLine (symtab) (WA 0u) "test ADD R0, R0, #1"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 1u)}, symtab.Add ("test", 0u))
            (parseLine (symtab) (WA 0u) "ADDEQ R0, R0, #4"), 
                Ok (cpuData, symtab)
         ]

         
    /// tests parsing and execution of multiple lines
    [<Tests>]
    let testParseThenExecLines = 
        let cpuData = 
            match initDataPath None None None with
            | Ok x -> x
            | Error _ -> failwithf "Should never happen."
        let symtab = ["test", 0x100u] |> Map.ofList
        let getParsed f ld = 
            match f ld with
            | Some x -> x
            | None -> failwithf "Should never happen."
        makeUnitTestListWithTwoParams parseThenExecLines (cpuData, symtab) "Unit Test parseThenExecLines" [
            // // test single valid lines
            ["ADD R0, R0, #1"], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R0 1u
                            |> Map.add R15 8u
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 1u;})))
                    }, symtab)
            ["ADD R0, R0, R0, LSL #5"], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R0 0u
                            |> Map.add R15 8u
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                        SuffixSet = false;
                                        Target = R0;
                                        Op1 = R0;
                                        Op2 = RegisterShift (R0,LSL,5);})))
                    }, symtab)
            // test valid line with blank lines
            ["; some comment here"; "ADD R0, R0, #1"; ""], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R0 1u
                            |> Map.add R15 8u
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 1u;})))
                    }, symtab)

            ["ADDEQ R0, R0, #1"], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R0 0u
                            |> Map.add R15 8u
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 1u;})))
                    }, symtab)
            // test line starting with tab
            ["\tADDEQ R0, R0, #1"], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R0 0u
                            |> Map.add R15 8u
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 1u;})))
                    }, symtab)
            // test line starting with tab and label
            ["test\tADDEQ R0, R0, #1"], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R0 0u
                            |> Map.add R15 8u
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 1u;})))
                    }, symtab.Add ("test", 0u))
            // test line starting with spaces
            ["     ADDEQ R0, R0, #1"], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R0 0u
                            |> Map.add R15 8u
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 1u;})))
                    }, symtab)

            ["ADD R0, R0, #1"; "END"; "ADD R0,R0,#1";], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R0 1u
                            |> Map.add R15 12u
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 1u;})))
                            |> Map.add (WA 4u) 
                                (Code (IMULTMEM (EndI END)))
                            |> Map.add (WA 8u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 1u;})))
                    }, symtab)   

            ["ADR R0, testL2"; "testL DCD 135";"testL2 DCD 137"], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R0 (minDataMemAddress+8u)
                            |> Map.add R15 16u
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IMEM (AdrO (Ok {InstructionType = ADRm;
                                      DestReg = R0;
                                      SecondOp = (minDataMemAddress+8u);}))))
                            |> Map.add (WA 4u) 
                                (Code (IMEM (LabelO (Ok {InstructionType = DCD;
                                        Name = Some "testL";
                                        EquDcdFill = Vl ["135"];}))))
                            |> Map.add (WA 8u) 
                                (Code (IMEM (LabelO (Ok {InstructionType = DCD;
                                        Name = Some "testL2";
                                        EquDcdFill = Vl ["137"];}))))
                            |> Map.add (WA (minDataMemAddress+4u)) (DataLoc 135u)
                            |> Map.add (WA (minDataMemAddress+8u)) (DataLoc 137u)
                    }, symtab
                        |> Map.add "testL"  (minDataMemAddress+4u)
                        |> Map.add "testL2" (minDataMemAddress+8u))      
      
            ["ADD R0, R0, #1"; "ENDEQ"; "ADD R0,R0,#1";], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R0 2u
                            |> Map.add R15 16u
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 1u;})))
                            |> Map.add (WA 4u) 
                                (Code (IMULTMEM (EndI END)))
                            |> Map.add (WA 8u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 1u;})))
                    }, symtab)

            ["ADD R0, R0, #3"; "start ADD R3,R3,#1"; "SUBS R0,R0,#1"; "BNE start"], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R3 3u
                            |> Map.add R15 20u
                        Fl = {N = false; C = true; Z = true; V = false;}
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 3u;})))
                            |> Map.add (WA 4u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R3;
                                            Op1 = R3;
                                            Op2 = Literal 1u;})))
                            |> Map.add (WA 8u) 
                                (Code (IARITH (ArithI {InstrType = Some SUB;
                                            SuffixSet = true;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 1u;})))
                            |> Map.add (WA 12u) 
                                (Code (IMULTMEM (BranchI {BranchAddr = 12u; LinkAddr=None})))
                    }, symtab.Add ("start", 4u))

            ["ADD R0, R0, #3"; "start ADD R3,R3,#1"; "SUBS R0,R0,#1"; "MOVNE R15, #start+8"], 
            Ok ({cpuData with 
                    Regs = cpuData.Regs
                        |> Map.add R3 3u
                        |> Map.add R15 20u
                    Fl = {N = false; C = true; Z = true; V = false;}
                    MM = cpuData.MM
                        |> Map.add (WA 0u) 
                            (Code (IARITH (ArithI {InstrType = Some ADD;
                                        SuffixSet = false;
                                        Target = R0;
                                        Op1 = R0;
                                        Op2 = Literal 3u;})))
                        |> Map.add (WA 4u) 
                            (Code (IARITH (ArithI {InstrType = Some ADD;
                                        SuffixSet = false;
                                        Target = R3;
                                        Op1 = R3;
                                        Op2 = Literal 1u;})))
                        |> Map.add (WA 8u) 
                            (Code (IARITH (ArithI {InstrType = Some SUB;
                                        SuffixSet = true;
                                        Target = R0;
                                        Op1 = R0;
                                        Op2 = Literal 1u;})))
                        |> Map.add (WA 12u) 
                            (Code (IBITARITH ({Instruction = BitArithmetic.MOV;
                                        Dest = Some R15;
                                        Suff = BitArithmetic.NA;
                                        Op1 = Ok(BitArithmetic.Literal 12u);
                                        Op2 = Error ""
                                        })))
                }, symtab.Add ("start", 4u))

            ["MVNNE R15, #12"], 
            Ok ({cpuData with 
                    Regs = cpuData.Regs
                        |> Map.add R15 (~~~12u)
                    MM = cpuData.MM
                        |> Map.add (WA 0u) 
                            (Code (IBITARITH ({Instruction = BitArithmetic.MVN;
                                        Dest = Some R15;
                                        Suff = BitArithmetic.NA;
                                        Op1 = Ok(BitArithmetic.Literal 12u);
                                        Op2 = Error ""
                                        })))
                }, symtab)

            // test branch with blank lines
            [""; "ADD R0, R0, #3"; "; some comment"; "start ADD R3,R3,#1"; ""; "SUBS R0,R0,#1"; "BNE start"], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R3 3u
                            |> Map.add R15 20u
                        Fl = {N = false; C = true; Z = true; V = false;}
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 3u;})))
                            |> Map.add (WA 4u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R3;
                                            Op1 = R3;
                                            Op2 = Literal 1u;})))
                            |> Map.add (WA 8u) 
                                (Code (IARITH (ArithI {InstrType = Some SUB;
                                            SuffixSet = true;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 1u;})))
                            |> Map.add (WA 12u) 
                                (Code (IMULTMEM (BranchI {BranchAddr = 12u; LinkAddr=None})))
                    }, symtab.Add ("start", 4u))

            ["ADD R0, R0, #test3"; "test3 SUB R0, R0, #3"], 
            Ok ({cpuData with 
                    Regs = cpuData.Regs
                        |> Map.add R0 1u
                        |> Map.add R15 12u
                    MM = cpuData.MM
                        |> Map.add (WA 0u) 
                            (Code (IARITH (ArithI {InstrType = Some ADD;
                                        SuffixSet = false;
                                        Target = R0;
                                        Op1 = R0;
                                        Op2 = Literal 4u;})))
                        |> Map.add (WA 4u) 
                            (Code (IARITH (ArithI {InstrType = Some SUB;
                                        SuffixSet = false;
                                        Target = R0;
                                        Op1 = R0;
                                        Op2 = Literal 3u;})))
                }, symtab.Add ("test3", 4u))
            ["ADDS R0, R0, #test1"; "test1 LSL R0, R0, #3"], 
            Ok ({cpuData with 
                    Regs = cpuData.Regs
                        |> Map.add R0 32u
                        |> Map.add R15 12u
                    MM = cpuData.MM
                        |> Map.add (WA 0u) 
                            (Code (IARITH (ArithI {InstrType = Some ADD;
                                        SuffixSet = true;
                                        Target = R0;
                                        Op1 = R0;
                                        Op2 = Literal 4u;})))
                        |> Map.add (WA 4u) 
                            (Code (IBITARITH {Instruction = BitArithmetic.LSL;
                                        Suff = BitArithmetic.NA;
                                        Dest = Some R0;
                                        Op1 = Ok (BitArithmetic.Register R0);
                                        Op2 = Ok (BitArithmetic.Literal 3u);}))
                }, symtab.Add ("test1", 4u))

            ["ADDS R0, R0, #test1"; "test1 LSL R0, R0, #3"; "STR R0, [R0]"; "LDR R4, [R0]"], 
            Ok ({cpuData with 
                    Regs = cpuData.Regs
                        |> Map.add R0 32u
                        |> Map.add R4 32u
                        |> Map.add R15 20u
                    MM = cpuData.MM
                        |> Map.add (WA 0u) 
                            (Code (IARITH (ArithI {InstrType = Some ADD;
                                        SuffixSet = true;
                                        Target = R0;
                                        Op1 = R0;
                                        Op2 = Literal 4u;})))
                        |> Map.add (WA 4u) 
                            (Code (IBITARITH {Instruction = BitArithmetic.LSL;
                                        Suff = BitArithmetic.NA;
                                        Dest = Some R0;
                                        Op1 = Ok (BitArithmetic.Register R0);
                                        Op2 = Ok (BitArithmetic.Literal 3u);}))
                        |> Map.add (WA 8u) 
                            (Code (IMEM (MemO (Ok {InstructionType = STR;
                                      DestSourceReg = R0;
                                      AddressReg = R0;
                                      BytesNotWords = false;
                                      IncrementValue = 0;
                                      PreOrPostIndRb = Neither;
                                      ExtraAddressReg = None;
                                      ShiftExtraRegBy = None;}))))
                        |> Map.add (WA 12u) 
                            (Code (IMEM (MemO (Ok {InstructionType = LDR;
                                       DestSourceReg = R4;
                                       AddressReg = R0;
                                       BytesNotWords = false;
                                       IncrementValue = 0;
                                       PreOrPostIndRb = Neither;
                                       ExtraAddressReg = None;
                                       ShiftExtraRegBy = None;}))))
                        |> Map.add (WA 32u) (DataLoc 32u)
                }, symtab.Add ("test1", 4u))

            ["MOV R0, #0xAA"; "MOV R1, #0b111011"; "MOV R2, #0xA9"; "STM R0, {R1,R2}"], 
            Ok ({cpuData with 
                    Regs = cpuData.Regs
                        |> Map.add R0 170u
                        |> Map.add R1 59u
                        |> Map.add R2 169u
                        |> Map.add R15 20u
                    MM = cpuData.MM
                        |> Map.add (WA 0u) 
                            (Code (IBITARITH {Instruction = BitArithmetic.MOV;
                                 Suff = BitArithmetic.NA;
                                 Dest = Some R0;
                                 Op1 = Ok (BitArithmetic.Literal 170u);
                                 Op2 = Error "";}))
                        |> Map.add (WA 4u) 
                            (Code (IBITARITH {Instruction = BitArithmetic.MOV;
                                 Suff = BitArithmetic.NA;
                                 Dest = Some R1;
                                 Op1 = Ok (BitArithmetic.Literal 59u);
                                 Op2 = Error "";}))
                        |> Map.add (WA 8u) 
                            (Code (IBITARITH {Instruction = BitArithmetic.MOV;
                                 Suff = BitArithmetic.NA;
                                 Dest = Some R2;
                                 Op1 = Ok (BitArithmetic.Literal 169u);
                                 Op2 = Error "";}))
                        |> Map.add (WA 12u) 
                            (Code (IMULTMEM (MemI {InsType = Some STM;
                                       Direction = Some EA;
                                       Target = R0;
                                       WriteBack = false;
                                       RegList = [R1; R2];})))
                        |> Map.add (WA 170u) (DataLoc 59u)
                        |> Map.add (WA 174u) (DataLoc 169u)
                }, symtab)

            ["ADD R0, R0, #0xFF"; "ADD R1, R1, #0xA9"; "ADD R2, R2, #0xD3";
                "STM R0!, {R1,R2}"], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R0 263u
                            |> Map.add R1 0xA9u
                            |> Map.add R2 0xD3u
                            |> Map.add R15 20u
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 0xFFu;})))
                            |> Map.add (WA 4u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R1;
                                            Op1 = R1;
                                            Op2 = Literal 0xA9u;})))
                            |> Map.add (WA 8u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R2;
                                            Op1 = R2;
                                            Op2 = Literal 0xD3u;})))
                            |> Map.add (WA 12u) 
                                (Code (IMULTMEM (MemI {InsType = Some STM;
                                            Direction = Some EA;
                                            Target = R0;
                                            WriteBack = true;
                                            RegList = [R1; R2];})))
                            |> Map.add (WA 255u)  (DataLoc 0xA9u)
                            |> Map.add (WA 259u)  (DataLoc 211u)
                    }, symtab)

            // test EQU
            
            ["label EQU 37"], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R15 8u}, symtab.Add ("label", 37u))
            ["label EQU 39"; "ADD R0,R0, #0"], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R15 8u
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 0u;})))
                    }, symtab.Add ("label",39u))
            ["ADD R0,R0, #0"; "label EQU 39";], 
                Ok ({cpuData with 
                        Regs = cpuData.Regs
                            |> Map.add R15 8u
                        MM = cpuData.MM
                            |> Map.add (WA 0u) 
                                (Code (IARITH (ArithI {InstrType = Some ADD;
                                            SuffixSet = false;
                                            Target = R0;
                                            Op1 = R0;
                                            Op2 = Literal 0u;})))
                    }, symtab.Add ("label",39u))
            
            ["infinite ADD R0, R0, #1"; "B infinite"],
                Error (ERRTOPLEVEL "Infinite loop detected. Branched more than 100,000 times.")

            // test invalid single lines
            ["LDM R0, R0, {}"], 
                Error (ERRLINE (ERRIMULTMEM "Incorrectly formatted operands.", 0u))
            // test multiple invalid lines 
            ["LDM R0, R0, {}"; "ADD R16, R0, #1"], 
                Error (ERRLINE (ERRIMULTMEM "Incorrectly formatted operands.", 0u))
            ["LDM R0, {R4}"; "ADD R16, R0, #1"], 
                Error (ERRLINE (ERRIARITH "Destination is not a valid register",1u))
            // test errors on blank lines
            ["LDM R0, {R4}"; ""; "ADD R16, R0, #1"], 
                Error (ERRLINE (ERRIARITH "Destination is not a valid register",2u))
            [""; "LDM R0, {R4}"; ""; "ADD R16, R0, #1"], 
                Error (ERRLINE (ERRIARITH "Destination is not a valid register",3u))
            [""; "ADD R0, R0, #3"; "someLabel"; "start ADD R3,R3,#1"; ""; "SUBS R0,R0,#1"; "BNE start"], 
                Error (ERRLINE (ERRTOPLEVEL "Invalid instruction: someLabel",2u))
         ]