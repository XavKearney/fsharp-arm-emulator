/// handles LDM/STM instructions
/// author: Xav Kearney
module MultMem
    
    open CommonData
    open CommonLex
    open System.Text.RegularExpressions

    type MultMemInstrType = LDM | STM
    type MultMemDirection = FD | FA | ED | EA 
    
    /// multiple memory access instruction type
    type MultMemInstr = 
        {
            // load or store
            InsType: MultMemInstrType option;
            // stack direction
            Direction: MultMemDirection option;
            // target register: source/destination
            Target: RName;
            // optional writeback suffix '!'
            // writes the address of the final mem location back to target reg
            WriteBack: bool;
            // list of registers to store/load to/from
            RegList: RName list;
        }

    /// parse error (dummy, but will do)
    type ErrInstr = string

    /// Defines the spec for LDM/STM instructions
    /// which access multiple memory locations
    /// and store on stack
    let multMemSpec = {
        InstrC = MEM
        Roots = ["LDM";"STM"]
        // for LDM: "", "IA" and "FD" are synonyms
        // for LDM: "EA" and "DB" are synonyms
        // for STM: "", "EA" and "IA" are synonyms
        // for STM: "FD" and "DB" are synonyms
        Suffixes = [""; "EA"; "ED"; "IA"; "FA"; "FD"; "DB"]
    }

    /// map of all possible opcodes recognised
    let multMemOpCodes = opCodeExpand multMemSpec

    /// ----------- ACTIVE PATTERNS ---------------

    /// takes a string and ensures it starts and ends with
    /// a given prefix and suffix
    /// if so, returns the string inside
    let (|GetInside|) (prefix:string) (suffix: string) (str:string) =
        if (str.StartsWith(prefix) && str.EndsWith(suffix))
        then Some (str.[1..String.length str - 2])
        else None

    /// matches a string with regex pattern
    /// returns a list of the matches
    let (|Matches|_|) (pat:string) (inp:string) =
        let m = Regex.Matches(inp, pat) in
        if m.Count > 0
        then Some ([ for g in m -> g.Value ])
        else None

    /// matches a string with regex pattern
    /// returns list of the matched groups (excluding the whole match)
    let (|MatchGroups|_|) (pat:string) (inp:string) =
        let m = Regex.Matches(inp, pat) in
        if m.Count > 0
        then 
            [ for x in m -> x.Groups ]
            |> List.collect (fun x -> [for y in x -> y.Value])
            |> List.tail // remove the whole matched string
            |> Some 
        else None
    
    /// ----------- END ACTIVE PATTERNS ---------------

    /// take a string of all operands and parse
    /// into target reg, writeback and list of reg
    /// to load/store
    /// returns error if anything is incorrect
    let parseOps (ops: string) =
        // split the operands by ',' e.g. "R10!, {R1,R2,R3}"
        let sLst = ops.Split(',') |> Array.map (fun s-> s.Trim())
        // get the target register as a string, e.g. "R10!"
        let targetStr = sLst.[0]
        // recombine the list of registers
        let reglstStr = String.concat "," sLst.[1..]
        // check for writeback suffix '!'
        let wb = targetStr.EndsWith('!')
        // get the target register's RName from string without '!' suffix
        let target = regNames.TryFind (targetStr.Trim('!'))

        let matchRegLst wb targ =
            // if string starts and ends with curly braces, get string inside
            match (|GetInside|) "{" "}" reglstStr with
            | Some(regStr) ->
                // check for a register range (e.g. {R0-R5}), returns start and end registers
                match (|MatchGroups|_|) @"^([A-Z]{1,2}\d{1,2})(?:-)([A-Z]{1,2}\d{1,2})$" regStr with
                // if active pattern matches, get start and end registers
                | Some (rStart :: [rEnd]) ->
                    // convert start-end to range
                    [regNames.[rStart].RegNum..regNames.[rEnd].RegNum]
                    |> fun x -> 
                        if List.isEmpty x then Error "Invalid register list range." else Ok x
                    |> Result.map (List.map inverseRegNums.TryFind)
                | _ ->
                    match (|Matches|_|) @"([A-Z]{1,2}\d{1,2})+" regStr with
                    | Some (regNameLst) ->
                        List.map regNames.TryFind regNameLst
                        |> Ok
                    | _ -> Error "Invalid list of registers."
                |> Result.bind (
                    fun lst -> if List.contains None lst then 
                                Error "Invalid list of registers."
                                else Ok(targ, wb, List.choose id lst)
                )
            | None -> Error ("Incorrectly formatted operands.")
            
        match target with
        | Some t -> matchRegLst wb t
        | None -> Error ("Target register not found.")

    /// check that the result after parsing MultMemInstr conforms to ARM spec
    let checkValid ins =
        match ins.InsType, ins.Target, ins.RegList, ins.WriteBack with
        | _, t, _, _ when t = R15 -> 
            Error "Target register cannot be PC (R15)."
        | _, _, rlst, _ when List.contains R13 rlst -> 
            Error "Register list cannot contain SP (R13)."
        | Some(STM), _, rlst, _ when List.contains R15 rlst ->
            Error "Register list cannot contain PC (R15) for STM instructions."
        | Some(LDM), _, rlst, _ when List.contains R14 rlst && List.contains R15 rlst ->
            Error "Register list cannot contain PC(R15) if it contains LR for LDM."
        | _, t, rlst, wb when wb && List.contains t rlst ->
            Error "Register list cannot contain target reg if writeback is enabled."
        | _ -> Ok (ins)

    /// take opcode root, suffix and string of operands
    /// if the operands parse, return instruction
    /// if not, return an error
    let makeMultMemInstr root suffix operands =
        parseOps operands
        |> Result.bind (fun (target, wb, regLst) ->
            // create template result with empty InsType & Direction
            let defaultIns = {
                InsType = None; Direction = None;
                Target = target; WriteBack = wb; RegList = regLst}
            match root, suffix with
            // LDM instructions, including synonyms
            | "LDM", ("" | "FD" | "IA") ->
                checkValid { defaultIns with InsType = Some(LDM); Direction = Some(FD); } 
            | "LDM", ("EA" | "DB") ->
                checkValid { defaultIns with InsType = Some(LDM); Direction = Some(EA); } 
            | "LDM", "FA" ->
                checkValid { defaultIns with InsType = Some(LDM); Direction = Some(FA); } 
            | "LDM", "ED" ->
                checkValid { defaultIns with InsType = Some(LDM); Direction = Some(ED); } 
            // STM instructions, including synonyms
            | "STM", ("" | "EA" | "IA") ->
                checkValid { defaultIns with InsType = Some(STM); Direction = Some(EA); } 
            | "STM", ("FD" | "DB") ->
                checkValid { defaultIns with InsType = Some(STM); Direction = Some(FD); } 
            | "STM", "FA" ->
                checkValid { defaultIns with InsType = Some(STM); Direction = Some(FA); } 
            | "STM", "ED" ->
                checkValid { defaultIns with InsType = Some(STM); Direction = Some(ED); } 
            // error if unsupported instruction
            | _ -> Error ("Opcode not supported.")
        )


    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse ls =
        let parse' (instrC, (root,suffix,pCond)) =
            let (WA la) = ls.LoadAddr
            match instrC with
            | MEM -> 
                match makeMultMemInstr root suffix ls.Operands with
                | Ok pinstr -> Ok {
                        PInstr = pinstr;
                        PLabel = ls.Label |> Option.map (fun lab -> lab, la); 
                        PSize = 4u; 
                        PCond = pCond;
                    }
                | Error s -> Error s
            | _ -> Error ("Instruction class not supported.")
        Map.tryFind ls.OpCode multMemOpCodes
        |> Option.map parse'


    /// Execute a MultMem Instruction
    /// given cpuData, transform based on instruction
    /// returns a Result with new cpuData
    /// or errors if instruction is incorrect
    let execMultMem parsed cpuData =
        let exec mode dir targ wb rlst =
            let sortedRLst = List.sortByDescending (fun (x:RName) -> x.RegNum) rlst
            let dirOp, initialN, orderedRLst =
                match mode, dir with
                | LDM, Some(FD) -> (+), 0u, List.rev sortedRLst
                | LDM, Some(FA) -> (-), 0u, sortedRLst
                | LDM, Some(EA) -> (-), 1u, sortedRLst
                | LDM, Some(ED) -> (+), 1u, List.rev sortedRLst
                | STM, Some(FD) -> (-), 1u, sortedRLst
                | STM, Some(FA) -> (+), 1u, List.rev sortedRLst
                | STM, Some(EA) -> (+), 0u, List.rev sortedRLst
                | STM, Some(ED) -> (-), 0u, sortedRLst
                | _, None -> failwithf "Should never happen."

            let rec exec' regs addr cpu =
                let newAddr = dirOp addr 4u
                match mode, regs with
                // if list of registers empty, return with correct writeback addr
                | LDM, [] -> 
                    match dir with
                    | Some(FD | FA) -> Ok (cpu, addr)
                    | Some(EA) -> Ok (cpu, addr + 4u)
                    | Some(ED) -> Ok (cpu, addr - 4u)
                    | None -> failwithf "Should never happen"
                | STM, [] -> 
                    match dir with
                    | Some(FD) -> Ok (cpu, addr + 4u)
                    | Some(FA) -> Ok (cpu, addr - 4u)
                    | Some(EA | ED) -> Ok (cpu, addr)
                    | None -> failwithf "Should never happen"
                
                // otherwise, load/store with next register
                | LDM, reg :: rest -> 
                    match cpu.MM.[WA addr] with
                    | DataLoc data -> 
                        { cpu with Regs = cpu.Regs.Add (reg, data); }
                        |> fun newCpu -> exec' rest newAddr newCpu
                    | _ -> Error "Invalid memory address (code)."
                | STM, reg :: rest -> 
                    cpu.Regs.[reg]
                    |> fun data -> { cpu with MM = cpu.MM.Add (WA addr, DataLoc data); }
                    |> fun newCpu -> exec' rest newAddr newCpu

            match wb with
            | true ->
                exec' orderedRLst (dirOp cpuData.Regs.[targ] (initialN*4u)) cpuData
                |> function
                    | Ok(cpu, addr) -> Ok ({cpu with Regs = cpu.Regs.Add (targ, addr)};)
                    | Error s -> Error s
            | false -> 
                exec' orderedRLst (dirOp cpuData.Regs.[targ] (initialN*4u)) cpuData
                |> function
                    | Ok(cpu, _) -> Ok(cpu)
                    | Error s -> Error s

        let instr = parsed.PInstr
        let dir, targ, wb, rlst = instr.Direction, instr.Target, instr.WriteBack, instr.RegList
        match checkValid instr with
        | Ok _ ->
            match instr.InsType with
            | Some(LDM) -> exec LDM dir targ wb rlst
            | Some(STM) -> exec STM dir targ wb rlst
            | None -> failwithf "No instruction type given."
        | Error s -> Error s
        

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = parse
