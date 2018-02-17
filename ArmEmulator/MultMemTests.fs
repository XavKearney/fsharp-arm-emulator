module MultMemTests
    open CommonData
    open CommonLex
    open MultMem
    open Expecto
    open FsCheck
    open VisualTest.Visual
    open VisualTest.VTest
    open VisualTest.VCommon

    let genRandomUint32List (min,max) count =
        let rnd = System.Random()
        List.init count (fun _ -> rnd.Next (min, max))
        |> List.map uint32

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
    let testParseOpsUnit = 
        makeUnitTestList parseOps "parseOps Unit" 
            [
                ("R7, {R3,R9,R1}", Ok (R7, false, [R3;R9;R1]));
                ("R0!, {R2,R12,R1,R3}", Ok (R0, true, [R2;R12;R1;R3]));
                ("R7, {R1-R3}", Ok (R7, false, [R1;R2;R3]));
                ("R7, {R3-R1}", Error "Invalid register list range.");
                ("R4, {}", Error "Invalid list of registers.");
                ("R4, {E3}", Error "Invalid list of registers.");
                ("R4 {R1,R2}", Error "Target register not found.");
                ("R, {R3,R9,R1}", Error "Target register not found.");
                ("R7,, {R3,R9,R1}", Error "Incorrectly formatted operands.");
                ("R7, {R3,R9,R1", Error "Incorrectly formatted operands.");
                ("R7, R3,R9,R1", Error "Incorrectly formatted operands.");
                ("R7, R3,R9,R1}", Error "Incorrectly formatted operands.");
                ("R7 {R3,R9,R1}", Error "Target register not found.");
            ]

    [<Tests>]
    let testParseUnit = 
        let ls = { LoadAddr = WA 0u; Label = None; SymTab = None;
                OpCode = ""; Operands = ""; }
        makeUnitTestList parse "parse Unit" 
            [
                ({ls with OpCode = "STM"; Operands = "R15, {R1,R3}";}, 
                    Some(Error "Target register cannot be PC (R15)."));
                ({ls with OpCode = "LDM"; Operands = "R15, {R1,R3}";}, 
                    Some(Error "Target register cannot be PC (R15)."));
                ({ls with OpCode = "LDM"; Operands = "R7, {R1,R13}";}, 
                    Some(Error "Register list cannot contain SP (R13)."));
                ({ls with OpCode = "STM"; Operands = "R4, {R15,R3}";}, 
                    Some(Error "Register list cannot contain PC (R15) for STM instructions."));
                ({ls with OpCode = "LDM"; Operands = "R4, {R15,R14,R5}";}, 
                    Some(Error "Register list cannot contain PC(R15) if it contains LR for LDM."));
                ({ls with OpCode = "LDM"; Operands = "R4!, {R4,R7,R9}";}, 
                    Some(Error "Register list cannot contain target reg if writeback is enabled."));
                ({ls with OpCode = "STM"; Operands = "R9!, {R4,R1,R9}";}, 
                    Some(Error "Register list cannot contain target reg if writeback is enabled."));
                ({ls with OpCode = "STMEF"; Operands = "R15, {R1,R3}";}, 
                    None);
                ({ls with OpCode = "ADD"; Operands = "R15, R15, #5";}, 
                    None);
            ]
    let config = { FsCheckConfig.defaultConfig with maxTest = 1 }
    [<Tests>]
    let testParse =
        let makeLineData wa opcode suffixStr target wb rLst = 
            let opCodeStr = 
                match opcode with
                | LDM -> "LDM"
                | STM -> "STM"

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
        let makeInstrString opcode direction target wb rLst =
            let opCodeStr = 
                match opcode with
                | LDM -> "LDM"
                | STM -> "STM"
            let suffixStr, dirOp, initialN = 
                match opcode, direction with
                // VisUAL doesn't support "" as a suffix
                | LDM, FD -> chooseFromList ["FD"; "IA"], (+), 0u
                | LDM, FA -> "FA", (-), 0u
                | LDM, EA -> chooseFromList ["EA"; "DB"], (-), 1u
                | LDM, ED -> "ED", (+), 1u
                | STM, FD -> chooseFromList ["FD"; "DB"], (-), 1u
                | STM, FA -> "FA", (+), 1u
                // VisUAL doesn't support "" as a suffix
                | STM, EA -> chooseFromList ["EA"; "IA"], (+), 0u
                | STM, ED -> "ED", (-), 0u
            let reglstStr =  String.concat "," (List.map (fun r-> regStrings.[r]) rLst)

            // create instruction string
            if wb then regStrings.[target] + "!," + "{" + reglstStr + "}"
            else regStrings.[target] + "," + "{" + reglstStr + "}"
            |> sprintf "%s%s %s" opCodeStr suffixStr
            // return all necessary params
            |> fun s -> (s, dirOp, initialN, suffixStr)

        testPropertyWithConfig config "Property Test ExecMultMem" <| 
        fun opcode direction (target: RName) wb (rLst: RName list) flags->
            // generate random values for registers R0-R11, set R12-R14 to 0
            let regVals = 
                genRandomUint32List (-0x7FFFFFFF, 0xFFFFFFFF) 12
                |> fun lst -> List.concat [lst; [0u; 0u; 0u;]]
            // create the instruction as a string, and get other necessary params
            let instrString, dirOp, initialN, suffixStr = 
                makeInstrString opcode direction target wb rLst
            let valid =  
                match opcode, target, wb, rLst with
                // the following are ARM restrictions
                // NB: not all restrictions listed, as some VisUAL restrictions overlap
                | _, _, _, [] -> false
                | _, t, _, _ when t = R15 -> false
                | _, _, _, rlst when List.contains R13 rlst -> false
                | STM, _, _, rlst when List.contains R15 rlst -> false
                | LDM, _, _, rlst when List.contains R14 rlst && List.contains R15 rlst -> false
                // the following are VisUAL restrictions
                // VisUAL doesn't allow target register within reg list
                | _, _, _, rlst when List.contains target rlst -> false
                // VisUAL memory addresses are > 0x1000 (extra 0x30 to allow for descending instructions)
                | _, t, _, _ when regVals.[t.RegNum] < 0x1030u -> false
                // VisUAL requires mem addresses to be divisible by 4
                | _, t, _, _ when (dirOp regVals.[t.RegNum] (initialN*4u)) % 4u <> 0u -> false
                // Can't test if R15 is in rlst because it will branch
                | _, _, _, rlst when List.contains R15 rlst -> false
                // need to be able to read the registers in the list
                // VTest only allows reading up to 12
                | _, _, _, rlst when rlst.Length > 12 -> false
                | _ -> true
            
            match valid with
            | false -> ()
            | true ->
                // get the address stored in the target register
                let targetAddr = regVals.[target.RegNum]
                // calculate the writeback address 
                let wbAddr = 
                    match opcode, direction with
                    | STM, (ED | FD) -> targetAddr - (4u* uint32 rLst.Length)
                    | STM, (EA | FA) -> targetAddr + (4u* uint32 rLst.Length)
                    // LDM instructions can use the same
                    | _ -> targetAddr 
                // generate random register values
                let memVals = genRandomUint32List (-0x7FFFFFFF, 0x7FFFFFFF) rLst.Length
                // put random register values in a map with correct addresses
                let memMap =
                    memVals
                    |> List.mapi (fun i x -> 
                        (WA (dirOp targetAddr ((uint32 i + initialN)*4u))), (DataLoc x))
                    |> Map.ofList;
                // create a register map in the VTest format
                let visMemMap =
                    memVals
                    |> List.mapi (fun i x -> 
                        ((dirOp targetAddr ((uint32 i + initialN)*4u))), x)
                    |> Map.ofList;
                // determine the suffix to load with when checking memory
                // aliases of EA/FD are not the same for LDM
                let loadSuffix = 
                    match opcode, direction with
                    | STM, EA -> "EA"
                    | STM, FD -> "FD"
                    | _ -> suffixStr
                // set the test parameters, used by VTest
                let testParas = {
                        defaultParas with
                            InitRegs = regVals;
                            //initialise memory locations 
                            InitMem = visMemMap;
                            //Read memBase..memBase+12 into R1-R12, starting at wbAddr
                            MemReadBase = wbAddr;
                            // use the loadSuffix as the direction for the LDM instruction
                            MemReadDirection = loadSuffix;
                    }
                // create the parsed MultMemInstr
                let parsed = {
                        PInstr =  { 
                                    InsType = Some(opcode); 
                                    Direction = Some(direction);
                                    Target = target; 
                                    WriteBack = wb; 
                                    RegList = rLst
                                };
                        PLabel = None; PSize = 4u; PCond = Cal;
                    }
                // run VisUAL with the instruction and parameters above
                let _, outExp, memExp = RunVisualWithFlagsOut testParas instrString

                // get the values of the registers from VisUAL in ascending order
                let regsExp = 
                    outExp.Regs
                    |> List.sortBy (fun (r, _) -> r)
                    |> List.map (fun (_, i) -> uint32 i)
                // get the relevant memory contents from VisUAL
                let memCheck = 
                    match opcode, direction with
                    | STM, (ED | FD) | LDM, (FD | ED) -> 
                        memExp |> List.rev |> List.tail |> List.take rLst.Length
                    | STM, (EA | FA) | LDM, (EA | FA)->
                        memExp |> List.take rLst.Length |> List.rev
               
               // create a DataPath with the correct initial register and mem contents
                let cpuData = {
                    Fl = flags;
                    Regs = List.mapi (fun i x -> (inverseRegNums.[i], x)) regVals |> Map.ofList;
                    MM = memMap;
                }
                // run execMultMem to produce the resulting DataPath after instruction execution
                let resCpu = execMultMem parsed cpuData
                match resCpu with
                | Ok cpu ->
                    // get the register contents after instruction execution
                    let regsActual =
                        cpu.Regs
                        |> Map.toList
                        |> List.map (fun (_, x) -> x)
                    // get memory contents
                    let memActual =
                        cpu.MM
                        |> Map.toList
                        |> List.map 
                            (function
                            | _, DataLoc x -> x
                            | _ -> failwithf "Should never happen")
                    // check that the registers in VisUAL = registers after execution
                    Expect.equal regsActual regsExp.[..regsExp.Length - 2]  "test mem regs"
                    // check that the memory in VisUAL = memory after execution
                    Expect.equal memActual memCheck "test mem memory"
                | Error _ -> ()
