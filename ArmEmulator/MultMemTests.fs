module MultMemTests
    open CommonData
    open CommonLex
    open MultMem
    open Expecto
    open FsCheck
    open VisualTest.Visual
    open VisualTest.VTest
    open VisualTest.VCommon



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
                //TODO get memReadOffset correct, and get resulting memory list in a checkable form
                // some resulting memActual lists need to be reversed, or have initialN corrected
                match opcode, direction with
                // VisUAL doesn't support ""
                | LDM, FD -> chooseFromList ["FD"; "IA"], (+), 0u
                | LDM, FA -> "FA", (-), 0u
                | LDM, EA -> chooseFromList ["EA"; "DB"], (-), 1u
                | LDM, ED -> "ED", (+), 1u
                | STM, FD -> chooseFromList ["FD"; "DB"], (-), 1u
                | STM, FA -> "FA", (+), 1u
                // VisUAL doesn't support ""
                | STM, EA -> chooseFromList ["EA"; "IA"], (+), 0u
                | STM, ED -> "ED", (-), 0u
            let reglstStr =  String.concat "," (List.map (fun r-> regStrings.[r]) rLst)

            if wb then regStrings.[target] + "!," + "{" + reglstStr + "}"
            else regStrings.[target] + "," + "{" + reglstStr + "}"
            |> sprintf "%s%s %s" opCodeStr suffixStr
            |> fun s -> (s, dirOp, initialN, suffixStr)
            

        testPropertyWithConfig config "Property Test ExecMultMem" <| 
        fun opcode direction (target: RName) wb (rLst: RName list) flags->
            let regVals = 
                Gen.choose (0, 0xFFFF) |> Gen.sample 0 12 |> List.map uint32
                |> fun lst -> List.concat [lst; [0u; 0u; 0u;]]
            
            let instrString, dirOp, initialN, suffixStr = 
                makeInstrString opcode direction target wb rLst
            let valid =  
                match opcode, target, wb, rLst with
                | _, _, _, [] -> false
                | _, t, _, _ when t = R15 -> false
                | _, _, _, rlst when List.contains R13 rlst -> false
                | STM, _, _, rlst when List.contains R15 rlst -> false
                // visual complains about the below
                | _, _, _, rlst when List.contains target rlst -> false
                | LDM, _, _, rlst when List.contains R14 rlst && List.contains R15 rlst -> false
                | _, t, wb, rlst when wb && List.contains t rlst -> false
                // VisUAL memory addresses are > 0x1000
                | _, t, _, _ when regVals.[t.RegNum] < 0x1000u -> false
                // VisUAL requires mem addresses to be divisible by 4
                | _, t, _, _ when (dirOp regVals.[t.RegNum] (initialN*4u)) % 4u <> 0u -> false
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
                let wbAddr = 
                    match opcode, direction with
                    | STM, ED -> targetAddr - (4u* uint32 rLst.Length) //works
                    | STM, FA -> targetAddr + (4u* uint32 rLst.Length)
                    | STM, EA -> targetAddr + (4u* uint32 rLst.Length)
                    | STM, FD -> targetAddr - (4u* uint32 rLst.Length) 
                    | _ -> targetAddr 
                // generate random register values
                let memVals = Gen.choose (0, 0xFFFF) |> Gen.sample 0 rLst.Length |> List.map uint32
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
                            //Read memBase..memBase+13 
                            MemReadBase = wbAddr;// regVals.[target.RegNum] ;//+ (initialN*4u);
                            // STMFA requires + initialN
                            // STMFD doesn't require + initialN
                            // STMED doesn't require + initialN
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
                let flagsExp, outExp, memExp = RunVisualWithFlagsOut testParas instrString
                printfn "%A" memExp
                // get the values of the registers in ascending order
                let regsExp = 
                    outExp.Regs
                    |> List.sortBy (fun (r, x) -> r)
                    |> List.map (fun (_, i) -> uint32 i)
                let memRec = 
                    match opcode, direction with
                    | STM, ED  -> List.rev memExp |> List.tail |> List.take rLst.Length
                    | STM, EA  -> List.take rLst.Length memExp |> List.rev
                    | STM, FD -> List.rev memExp |> List.take (rLst.Length+1) |> List.tail
                    | STM, FA ->  memExp.[..rLst.Length - 1] |> List.rev
                    | LDM,  FD -> List.rev memExp |> List.tail |> List.take rLst.Length
                    | LDM,  ED -> List.rev memExp |> List.tail |> List.take rLst.Length
                    | LDM, (EA | FA)->  memExp.[..rLst.Length - 1] |> List.rev
               
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
                    printfn "%A" cpu.MM
                    printfn "%s" instrString
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
                    Expect.equal memActual memRec  "test mem memory"
                | Error _ -> ()

// EA needs re-indexing
// FD is fine
// FA needs re-indexing
// ED is fine