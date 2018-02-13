module MultMemTests
    open CommonData
    open CommonLex
    open MultMem
    open Expecto
    open FsCheck
    open VisualTest

    /// choose an item from list at random
    let chooseFromList lst = 
        Gen.elements lst
        |> Gen.sample 0 1
        |> List.head

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
        let makeLineData wa opcode suffixStr target wb rLst = 
            let opCodeStr = 
                match opcode with
                | LDM -> "LDM"
                | STM -> "STM"
            // handle suffixes with aliases

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
        fun wa opcode target wb rLst ->
            // choose a random suffix string, including aliases
            let suffixStr = chooseFromList ["";"FD";"FA";"ED";"EA";"IA";"DB"]
            // get the correct direction from the suffix string
            let direction = 
                match opcode, suffixStr with
                | LDM, ("" | "FD" | "IA") -> FD
                | LDM, ("EA" | "DB") -> EA
                | STM, ("" | "EA" | "IA") -> EA
                | STM, ("FD" | "DB") -> FD
                | _, "FA" -> FA
                | _, "ED" -> ED
                | _ -> failwithf "Should never happen"
            // make the correct input data from random params
            let ls = makeLineData wa opcode suffixStr target wb rLst
            // determine correct output based on params
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
                | _ -> Some (
                        Ok {
                            PInstr =  { 
                                        InsType = Some(opcode); 
                                        Direction = Some(direction);
                                        Target = target; 
                                        WriteBack = wb; 
                                        RegList = rLst
                                    };
                            PLabel = None; PSize = 4u; PCond = Cal;
                    })
            let res = parse ls
            Expect.equal res expected "test parse"
    
    [<Tests>]
    let testExecMultMem = 
        let makeParsed opcode direction target wb rLst =
            {
                PInstr = {
                            InsType = Some(opcode); 
                            Direction = Some(direction);
                            Target = target; 
                            WriteBack = wb; 
                            RegList = rLst
                };
                PLabel = None; 
                PSize = 4u; 
                PCond = Cal;
            }

        let regs =    
            Gen.choose (0, 0xFFFF) |> Gen.sample 0 16
            |> List.mapi (fun i x -> (inverseRegNums.[i], uint32 x))
            |> Map.ofList
        let makeMem startAddr dirOp initialN len = 
            Gen.choose (0, 0xFFFF) |> Gen.sample 0 len
            |> List.mapi (fun i n -> 
                (WA (dirOp startAddr (((uint32 i)+initialN)) )), DataLoc (uint32 n))
            |> Map.ofList
        testPropertyWithConfig config "Property Test ExecMultMem" <| 
        fun opcode direction target wb rLst flags->
            let parsed = makeParsed opcode direction target wb rLst
            let dirOp, initialN, reverse =
                match opcode, direction with
                | LDM, FD -> (+), 0u, true
                | LDM, FA -> (-), 0u, false
                | LDM, EA -> (-), 1u, true
                | LDM, ED -> (+), 1u, false
                | STM, FD -> (-), 1u, true
                | STM, FA -> (+), 1u, false
                | STM, EA -> (+), 0u, false
                | STM, ED -> (-), 0u, true
            let mem = makeMem regs.[target] dirOp initialN rLst.Length
            let cpuData = {
                Fl = flags;
                Regs = regs;
                MM = mem;
            }
            let res = execMultMem parsed cpuData
            let expected =
                match opcode with
                | STM -> 
                    let regData = 
                        rLst
                        |> List.map (fun r-> regs.[r])
                    let newMem = 
                        mem 
                        |> Map.toList
                        |> List.mapi (fun i (a, _) -> (a, DataLoc regData.[i]))
                        |> if reverse then List.rev else id
                        |> Map.ofList
                    Ok {
                        cpuData with 
                            MM = newMem;
                            Regs = if wb then 
                                    cpuData.Regs.Add (target, dirOp regs.[target] (uint32 rLst.Length + initialN))
                                    else cpuData.Regs;
                    }
                | LDM ->
                    let memData = 
                        mem
                        |> Map.toList
                        |> List.map (
                            function
                            | (_, DataLoc x) -> x
                            | _ -> failwithf "Should never happen")
                    let rec setReg (curRegs: Map<RName,uint32>) regList n inc =
                        let next = if inc then n + 1 else n - 1
                        match regList with
                        | r :: rest -> setReg (curRegs.Add (r, memData.[n])) rest next inc
                        | [] -> curRegs
                    let orderedRLst = if reverse then List.rev rLst else rLst
                    let newRegs = 
                        match initialN, reverse with
                        | 0u, false | 1u, true -> setReg regs orderedRLst (memData.Length-1) false
                        | _ -> setReg regs orderedRLst 0 true
                        
                    Ok {
                        cpuData with 
                            Regs = if wb then 
                                    newRegs.Add (target, dirOp regs.[target] (uint32 rLst.Length + initialN))
                                   else newRegs
                    }
            match opcode with
            | LDM -> Expect.equal res expected "test exec"
            | _ -> ()
            