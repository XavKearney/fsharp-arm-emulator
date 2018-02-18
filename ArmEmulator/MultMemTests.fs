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

    /// unit tests for parseOps function
    /// takes a string, corresponding to ls.Operands
    /// returns either an error or
    /// target register, writeback (bool) and a register list
    [<Tests>]
    let testParseOpsUnit = 
        makeUnitTestList parseOps "parseOps Unit" 
            [
                // test valid input
                "R7, {R3,R9,R1}", Ok (R7, false, [R3;R9;R1])
                "R0!, {R2,R12,R1,R3}", Ok (R0, true, [R2;R12;R1;R3])
                "R0!, {LR,PC,SP}", Ok (R0, true, [R14; R15; R13])
                "R7, {R1-R3}", Ok (R7, false, [R1;R2;R3])
                "R7, {R10-LR}", Ok (R7, false, [R10; R11; R12; R13; R14])
                "R7, {SP-LR}", Ok (R7, false, [R13; R14])
                // test invalid input
                "R7, {R3-R1}", Error "Invalid register list range."
                "R7, {R0-R17}", Error "Invalid register list range."
                "R7, {R0R4}", Error "Invalid list of registers."
                "R4, {}", Error "Invalid list of registers."
                "R4, {E3}", Error "Invalid list of registers."
                "R4 {R1,R2}", Error "Target register not found."
                "R, {R3,R9,R1}", Error "Target register not found."
                "R7,, {R3,R9,R1}", Error "Incorrectly formatted operands."
                "R7, {R3,R9,R1", Error "Incorrectly formatted operands."
                "R7, R3,R9,R1", Error "Incorrectly formatted operands."
                "R7, R3,R9,R1}", Error "Incorrectly formatted operands."
                "R7 {R3,R9,R1}", Error "Target register not found."
            ]
    [<Tests>]
    let testMakeBranchInstrUnit = 
        let testSymTab = [("testLab", 37u); ("otherLab", 94u)] |> Map.ofList
        makeUnitTestList makeBranchInstr "makeBranchInstr Unit" 
            [
                // test valid input without symbol table
                ("", "someLabel", (WA 0u), None), 
                    Ok (BranchI {Label = "someLabel"; BranchAddr = None; Link = None;})
                ("L", "anotherLABEL", (WA 0u), None),
                    Ok (BranchI {Label = "anotherLABEL"; BranchAddr = None; Link = Some(WA 4u);})
                ("L", " aLabel ", (WA 0u), None), 
                    Ok (BranchI {Label = "aLabel"; BranchAddr = None; Link = Some(WA 4u);})
                // test valid input with symbol table (2nd pass)
                ("L", "testLab", (WA 0u), Some(testSymTab)), 
                    Ok (BranchI {Label = "testLab"; BranchAddr = Some(37u); Link = Some(WA 4u);})
                ("", "otherLab", (WA 0u), Some(testSymTab)), 
                    Ok (BranchI {Label = "otherLab"; BranchAddr = Some(94u); Link = None;})
                // test invalid input
                ("", "unknownLab", (WA 0u), Some(testSymTab)), Error "Branch label not found."
                ("K", "yetAnotherLabel", (WA 0u), None), Error "Invalid branch instruction."
                ("L", "label with spaces", (WA 0u), None), Error "Invalid branch instruction."
                ("", "", (WA 0u), None), Error "Invalid branch instruction."
                ("L", "", (WA 0u), None), Error "Invalid branch instruction."
                ("", " ", (WA 0u), None), Error "Invalid branch instruction."
            ]
    // unit tests for parse function
    // with input corresponding to LDM/STM instructions
    [<Tests>]
    let testParseUnitMultMem = 
        let ls = { LoadAddr = WA 0u; Label = None; SymTab = None;
                OpCode = ""; Operands = ""; }
        makeUnitTestList parse "parse Unit-MultMem" 
            [
                // test ARM restrictions are caught correctly
                {ls with OpCode = "STM"; Operands = "R15, {R1,R3}";}, 
                    Some(Error "Target register cannot be PC (R15).")
                {ls with OpCode = "LDM"; Operands = "R15, {R1,R3}";}, 
                    Some(Error "Target register cannot be PC (R15).")
                {ls with OpCode = "LDM"; Operands = "R7, {R1,R13}";}, 
                    Some(Error "Register list cannot contain SP (R13).")
                {ls with OpCode = "STM"; Operands = "R4, {R15,R3}";}, 
                    Some(Error "Register list cannot contain PC (R15) for STM instructions.")
                {ls with OpCode = "LDM"; Operands = "R4, {R15,R14,R5}";}, 
                    Some(Error "Register list cannot contain PC(R15) if it contains LR for LDM.")
                {ls with OpCode = "LDM"; Operands = "R4!, {R4,R7,R9}";}, 
                    Some(Error "Register list cannot contain target reg if writeback is enabled.")
                {ls with OpCode = "STM"; Operands = "R9!, {R4,R1,R9}";}, 
                    Some(Error "Register list cannot contain target reg if writeback is enabled.")
                // test invalid and unsupported opcodes return None
                {ls with OpCode = "STMEF"; Operands = "R15, {R1,R3}";}, 
                    None
                {ls with OpCode = "ADD"; Operands = "R15, R15, #5";}, 
                    None
            ]
    // unit tests for parse function
    // with input corresponding to B/BL/END instructions
    [<Tests>]
    let testParseUnitBranch = 
        let testSymTab = [("testLab", 37u); ("otherLab", 94u)] |> Map.ofList
        // define test LineData default
        let ls = { LoadAddr = WA 0u; Label = None; SymTab = None;
                OpCode = ""; Operands = ""; }
        // also define test LineData default with a SymbolTable
        let lsSymTab = {ls with SymTab = Some testSymTab}
        // default result
        let res = {PInstr = BranchI {Label = ""; BranchAddr = None; Link = None;}; 
                    PLabel = None; PSize = 4u; PCond = Cal;}
        makeUnitTestList parse "parse Unit-Branch" 
            [
                // test valid input
                {ls with OpCode = "B"; Operands = "testLabel";}, 
                    Some(Ok {res with PInstr = BranchI {Label = "testLabel"; BranchAddr = None; Link = None;}})
                {ls with OpCode = "BL"; Operands = "testLabel";}, 
                    Some(Ok {res with PInstr = BranchI {Label = "testLabel"; BranchAddr = None; Link = Some (WA 4u);}})
                {lsSymTab with OpCode = "B"; Operands = "testLab";}, 
                    Some(Ok {res with PInstr = BranchI {Label = "testLab"; BranchAddr = Some 37u; Link = None;}})
                {lsSymTab with OpCode = "BL"; Operands = "otherLab";}, 
                    Some(Ok {res with PInstr = BranchI {Label = "otherLab"; BranchAddr = Some 94u; Link = Some (WA 4u);}})
                {ls with OpCode = "BL"; Operands = "   Testwhitespace    ";}, 
                    Some(Ok {res with PInstr = BranchI {Label = "Testwhitespace"; BranchAddr = None; Link = Some (WA 4u);}})
                // test invalid input
                {ls with OpCode = "BL"; Operands = "multiple words";}, 
                    Some(Error "Invalid branch instruction.")
                {lsSymTab with OpCode = "B"; Operands = "wrongLabel";}, 
                    Some(Error "Branch label not found.")
                {lsSymTab with OpCode = "BL"; Operands = "wrongLabel";}, 
                    Some(Error "Branch label not found.")
                {ls with OpCode = "BLX"; Operands = "testLabel";}, 
                    None
            ]
    let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

    /// property-based testing of parse function
    /// for randomly generated branch instructions
    [<Tests>]
    let testExecBranchInsr = 
        testPropertyWithConfig config "Property Test execBranchInstr" <| 
        fun flags memMap label branchAddr linkAddr->
            // generate random values for registers R0-R15
            let regVals = genRandomUint32List (-0x7FFFFFFF, 0xFFFFFFFF) 16
            // create randomised cpuData
            let cpuData = {
                Fl = flags;
                Regs = List.mapi (fun i x -> (inverseRegNums.[i], x)) regVals |> Map.ofList;
                MM = memMap;
            }
            // create randomised parsed branch instruction
            let parsed = {
                PInstr = {Label = label; BranchAddr = branchAddr; Link = linkAddr;}; 
                    PLabel = None; PSize = 4u; PCond = Cal;}
            let result = execBranchInstr parsed cpuData
            // determine the correct result given randomised data
            let expected =
                match branchAddr, linkAddr with
                | Some b, None ->
                    Ok {cpuData with Regs = cpuData.Regs.Add (R15, b)}
                | Some b, Some (WA l) ->
                    {cpuData with Regs = cpuData.Regs.Add (R15, b)}
                    |> fun cpu -> Ok {cpu with Regs = cpuData.Regs.Add (R14, l)}
                | _ -> Error "Invalid branch instruction."
            Expect.equal result expected "cpuData"

    /// property-based testing of parse function
    /// for randomly generated LDM/STM instructions
    [<Tests>]
    let testParseMultMem =
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

        testPropertyWithConfig config "Property Test Parse-MultMem" <| 
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
                // check ARM restrictions
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
                // if not restricted, must return a parsed instruction
                | _ -> Some (
                        Ok {
                            PInstr = MemI { 
                                        InsType = Some(opcode); 
                                        Direction = Some(direction);
                                        Target = target; 
                                        WriteBack = wb; 
                                        RegList = rLst
                                    };
                            PLabel = None; PSize = 4u; PCond = Cal;
                    })
            let res = parse ls
            Expect.equal res expected "cpuData"
    
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
        fun opcode direction (target: RName) wb (rLst: RName list) (flags: CommonData.Flags)->
            // generate random values for registers R0-R11, set R12-R14 to 0
            let regVals = 
                genRandomUint32List (-0x7FFFFFFF, 0xFFFFFFFF) 12
                |> fun lst -> List.concat [lst; [0u; 0u; 0u;]]
            // create the instruction as a string, and get other necessary params
            let instrString, dirOp, initialN, suffixStr = 
                makeInstrString opcode direction target wb rLst
            let valid =  
                match opcode, target, wb, rLst, flags with
                // the following are ARM restrictions
                // NB: not all restrictions listed, as some VisUAL restrictions overlap
                | _, _, _, [], _ -> false
                | _, t, _, _, _ when t = R15 -> false
                | _, _, _, rlst, _ when List.contains R13 rlst -> false
                | STM, _, _, rlst, _ when List.contains R15 rlst -> false
                | LDM, _, _, rlst, _ when List.contains R14 rlst && List.contains R15 rlst -> false
                // the following are VisUAL restrictions
                // VisUAL doesn't allow target register within reg list
                | _, _, _, rlst, _ when List.contains target rlst -> false
                // VisUAL memory addresses are > 0x1000 (extra 0x30 to allow for descending instructions)
                | _, t, _, _, _ when regVals.[t.RegNum] < 0x1030u -> false
                // VisUAL requires mem addresses to be divisible by 4
                | _, t, _, _, _ when (dirOp regVals.[t.RegNum] (initialN*4u)) % 4u <> 0u -> false
                // Can't test if R15 is in rlst because it will branch
                | _, _, _, rlst, _ when List.contains R15 rlst -> false
                // need to be able to read the registers in the list
                // VTest only allows reading up to 12
                | _, _, _, rlst, _ when rlst.Length > 12 -> false
                | _, _, _, _, f when f.N && f.Z -> false
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
                            //initialise flags
                            InitFlags = {FN=flags.N;FZ=flags.Z; FC=flags.C;FV=flags.V}
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
                let flagsExp, outExp, memExp = RunVisualWithFlagsOut testParas instrString
                // convert between Flags types
                let flagsActual = {
                    N = flagsExp.FN;
                    Z = flagsExp.FZ;
                    C = flagsExp.FC;
                    V = flagsExp.FV;
                }

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
                    Expect.equal regsActual regsExp.[..regsExp.Length - 2]  "Registers"
                    // check that the memory in VisUAL = memory after execution
                    Expect.equal memActual memCheck "Memory"
                    // check that the flags in VisUAL = flags after execution
                    Expect.equal flags flagsActual "Flags"
                | Error _ -> ()
