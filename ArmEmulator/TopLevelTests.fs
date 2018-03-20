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
                Expect.equal (f inp <||| threeParams) outp testName
        List.map (fun (i, o) -> makeTest i o) inOutLst
        |> testList (sprintf "%s Test List" name) 

    [<Tests>]
    let testParseLine = 
        makeUnitTestList (parseLine None (WA 0u)) "Unit Test parseLine" [
            // test valid instructions
            "ADD R0, R0, #5", 
            Ok {PInstr = IARITH (ArithI {InstrType = Some ADD; SuffixSet = false;
                             Target = R0; Op1 = R0; Op2 = Literal 5u;});
                PLabel = None; PSize = 4u; PCond = Cal;}
            "ADR R0, 4", 
            Ok {PInstr = IMEM (AdrO (Error "parseAdrIns: ls.SymTab = None"));
                PLabel = None; PSize = 4u; PCond = Cal;}
                
            // TODO: add all instructions here

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

            // test invalid instructions
            "NOTANOPCODE R1, R2, #93", 
            Error (ERRTOPLEVEL "Instruction not implemented: NOTANOPCODE R1, R2, #93")
            "", Error (ERRTOPLEVEL "Invalid instruction: ")
            "blah", Error (ERRTOPLEVEL "Invalid instruction: blah")
        ]

    // tests the function execParsedLine with unit tests
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
        let someSymTab = Some (symtab)
        let getParsed f ld = 
            match f ld with
            | Some x -> x
            | None -> failwithf "Should never happen."
        makeUnitTestListWithThreeParams (removeResult >> execParsedLine) (cpuData, symtab, 0u) "Unit Test execParsedLine" [
            // test valid lines
            (parseLine (someSymTab) (WA 0u) "STM R0, {R1}"), 
                Ok ({cpuData with MM = cpuData.MM.Add (WA 0u, DataLoc 0u)}, symtab)
            (parseLine (someSymTab) (WA 0u) "ADD R0, R0, #1"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 1u)}, symtab)
            (parseLine (someSymTab) (WA 0u) "test ADD R0, R0, #1"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 1u)}, symtab.Add ("test", 0u))
            (parseLine (someSymTab) (WA 0u) "ADR R0, 4"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 4u)}, symtab)
            (parseLine (someSymTab) (WA 0u) "ADDEQ R0, R0, #4"), 
                Ok (cpuData, symtab)
            (parseLine (someSymTab) (WA 0u) "test DCD 1"), 
                Ok ({cpuData with MM = cpuData.MM.Add (WA 0x104u, DataLoc 1u)}, symtab.Add ("test", 1u))
         ]

         
    /// tests the function parseThenExecLines with unit tests
    let testParseThenExecLines = 
        let cpuData = 
            match initDataPath None None None with
            | Ok x -> x
            | Error _ -> failwithf "Should never happen."
        let symtab = ["test", 0x100u] |> Map.ofList
        let someSymTab = Some (symtab)
        let getParsed f ld = 
            match f ld with
            | Some x -> x
            | None -> failwithf "Should never happen."
        makeUnitTestListWithTwoParams parseThenExecLines (cpuData, someSymTab) "Unit Test execParsedLines" [
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
            ["infinite ADD R0, R0, #1"; "B infinite"],
                Error (ERRTOPLEVEL "Infinite loop detected. Branched more than 100,000 times.")

            // test invalid single lines
            ["LDM R0, R0, {}"], 
                Error (ERRLINE (ERRIMULTMEM "Incorrectly formatted operands.", 0u))
            // test multiple invalid lines - only returns the first error (TODO: change)
            ["LDM R0, R0, {}"; "ADD R16, R0, #1"], 
                Error (ERRLINE (ERRIMULTMEM "Incorrectly formatted operands.", 0u))
         ]