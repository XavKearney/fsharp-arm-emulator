module TopLevelTests

    open Expecto
    open CommonData
    open CommonLex
    open Arithmetic
    open BitArithmetic
    open MultMem
    open TopLevel

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

    [<Tests>]
    let testParseLine = 
        makeUnitTestList (parseLine None (WA 0u)) "Unit Test parseLine" [
            // test valid instructions
            "ADD R0, R0, #5", 
            Ok {PInstr = IARITH (ArithI {InstrType = Some ADD; SuffixSet = false;
                             Target = R0; Op1 = R0; Op2 = Literal 5u;});
                PLabel = None; PSize = 4u; PCond = Cal;}
                
            // TODO: add memory + bitarith instructions here

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
            Error (ERRTOPLEVEL "Instruction not implemented: \"NOTANOPCODE R1, R2, #93\"")
            "", Error (ERRTOPLEVEL "Invalid instruction: \"\"")
            "blah", Error (ERRTOPLEVEL "Invalid instruction: \"blah\"")
        ]

    /// tests the function execParsedLine with unit tests
    let testExecParsedLine = 
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
        makeUnitTestListWithTwoParams execParsedLine (cpuData, symtab) "Unit Test execParsedLine" [
            // test valid lines
            (parseLine (someSymTab) (WA 0u) "STM R0, {R1}"), 
                Ok ({cpuData with MM = cpuData.MM.Add (WA 0u, DataLoc 0u)}, symtab)
            (parseLine (someSymTab) (WA 0u) "ADD R0, R0, #1"), 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 1u)}, symtab)
            // test invalid lines
            (parseLine (someSymTab) (WA 0u) "ADDM R0, R0, #1"), 
                Error (ERRTOPLEVEL "Instruction not implemented: \"ADDM R0, R0, #1\"")
            (parseLine (someSymTab) (WA 0u) "ADD R16, R0, #1"), 
                Error (ERRIARITH "Destination is not a valid register")
            (parseLine (someSymTab) (WA 0u) "LDM R0, R0, {}"), 
                Error (ERRIMULTMEM "Incorrectly formatted operands.")
         ]

         
    /// tests the function execParsedLines with unit tests
    let testExecParsedLines = 
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
        makeUnitTestListWithTwoParams execParsedLines (cpuData, symtab) "Unit Test execParsedLines" [
            // test single valid lines
            [parseLine (someSymTab) (WA 0u) "STM R0, {R1}"], 
                Ok ({cpuData with MM = cpuData.MM.Add (WA 0u, DataLoc 0u)}, symtab)
            [parseLine (someSymTab) (WA 0u) "ADD R0, R0, #1"], 
                Ok ({cpuData with Regs = cpuData.Regs.Add (R0, 1u)}, symtab)
            // test multiple valid lines
            [parseLine (someSymTab) (WA 0u) "STM R0, {R1}";
                parseLine (someSymTab) (WA 0u) "ADD R0, R0, #1"], 
            Ok ({cpuData with 
                    Regs = cpuData.Regs.Add (R0, 1u);MM = cpuData.MM.Add (WA 0u, DataLoc 0u)}, symtab)
            [parseLine (someSymTab) (WA 0u) "ADD R0, R0, #5";
                parseLine (someSymTab) (WA 0u) "SUB R0, R0, #3"], 
            Ok ({cpuData with 
                    Regs = cpuData.Regs.Add (R0, 2u);}, symtab)
            // test invalid single lines
            [parseLine (someSymTab) (WA 0u) "LDM R0, R0, {}"], 
                Error (ERRIMULTMEM "Incorrectly formatted operands.")
            // test multiple invalid lines - only returns the first error (TODO: change)
            [parseLine (someSymTab) (WA 0u) "LDM R0, R0, {}";
                parseLine (someSymTab) (WA 0u) "ADD R16, R0, #1"], 
                Error (ERRIMULTMEM "Incorrectly formatted operands.")
         ]
