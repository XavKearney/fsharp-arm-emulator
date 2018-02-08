module MultMemTests
    open CommonData
    open CommonLex
    open MultMem
    open Expecto

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

    [<Tests>]
    let testParseOps = 
        makeUnitTestList parseOps "parseOps Unit" 
            [
                ("R7, {R3,R9,R1}", Ok (R7, false, [R3;R9;R1]));
                ("R0!, {R2,R12,R1,R3}", Ok (R0, true, [R2;R12;R1;R3]));
                ("R, {R3,R9,R1}", Error "Target register not found.");
                ("R7,, {R3,R9,R1}", Error "Incorrectly formatted operands.");
                ("R7, {R3,R9,R1", Error "Incorrectly formatted operands.");
                ("R7 {R3,R9,R1}", Error "Target register not found.");
            ]

    let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    [<Tests>]
    let testParse =
        let makeLineData wa opcode suffix target wb rLst = 
            let opCodeStr = 
                match opcode with
                | LDM -> "LDM"
                | STM -> "STM"
            // NB: This doesn't test suffix aliases or ""
            let suffixStr = 
                match suffix with
                | FD -> "FD"
                | FA -> "FA"
                | ED -> "ED"
                | EA -> "EA"
            let reglstStr =  String.concat "," (List.map (fun r-> regStrings.[r]) rLst)
            if wb then regStrings.[target] + "!," + "{" + reglstStr + "}"
            else regStrings.[target] + "," + "{" + reglstStr + "}"
            |> fun operandStr ->
            {
                LoadAddr = wa; 
                Label = None; 
                SymTab = None;
                OpCode = opCodeStr + suffixStr;
                Operands = operandStr;
            }

        testPropertyWithConfig config "Property Test Parse" <| 
        fun wa opcode suffix target wb rLst ->
            let ls = makeLineData wa opcode suffix target wb rLst
            let expected = 
                match opcode, target, wb, rLst with
                | _, _, _, [] -> 
                    Some (Error "Invalid list of registers.")
                | _, t, _, _ when t = R15 -> 
                    Some(Error "Target register cannot be PC (R15).")
                | _, _, _, rlst when List.contains R13 rlst ->
                    Some(Error "Register list cannot contain SP (R13).")
                | STM, _, _, rlst when List.contains R15 rlst ->
                    Some(Error "Register list cannot contain PC (R15) for STM instructions.")
                | LDM, _, _, rlst when List.contains R14 rlst && List.contains R15 rlst ->
                    Some(Error "Register list cannot contain PC(R15) if it contains LR for LDM.")
                | _, t, wb, rlst when wb && List.contains t rlst ->
                    Some(Error "Register list cannot contain target reg if writeback is enabled.")
                | _ -> Some (Ok {
                        PInstr =  { InsType = Some(opcode); Direction = Some(suffix);
                                    Target = target; WriteBack = wb; RegList = rLst} ;
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })
            let res = parse ls
            Expect.equal res expected "parse1"
    
