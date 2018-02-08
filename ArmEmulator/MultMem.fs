/// handles LDM/STM instructions
/// author: Xav Kearney
module MultMem
    
    open CommonLex
    open CommonData

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
    let opCodes = opCodeExpand multMemSpec

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
            match reglstStr.StartsWith('{') && reglstStr.EndsWith('}') with
            | false -> Error ("Incorrectly formatted operands.")
            | true ->
                let reglst = 
                    reglstStr.[1..String.length reglstStr - 2].Split(",")
                    |> Array.toList
                    |> List.map regNames.TryFind
                match List.contains None reglst with
                // if any registers not found, return error
                // NB: doesn't check for duplicate registers
                | true -> Error ("Invalid list of registers.")
                | false -> // if all registers found, transform Some x -> x and return
                    List.choose id reglst
                    |> fun rlst -> Ok (targ, wb, rlst)

        match target with
        | Some t -> matchRegLst wb t
        | None -> Error ("Target register not found.")

    /// take opcode root, suffix and string of operands
    /// if the operands parse, return instruction
    /// if not, return an error
    let makeMultMemInstr root suffix operands =
        // check that the result after parsing conforms to ARM spec
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

        match parseOps operands with
        | Ok (target, wb, regLst) ->
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
        | Error s -> Error s


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
        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = parse

    let execMultMem parsed cpuData =
        let exec mode dir targ wb rlst =
            let dirOp, initialN, orderedRegList =
                match mode, dir with
                | LDM, Some(FD) -> (+), 0u, List.rev rlst
                | LDM, Some(FA) -> (-), 0u, rlst
                | LDM, Some(EA) -> (-), 1u, List.rev rlst
                | LDM, Some(ED) -> (+), 1u, rlst
                | STM, Some(FD) -> (-), 1u, List.rev rlst
                | STM, Some(FA) -> (+), 1u, rlst
                | STM, Some(EA) -> (+), 0u, rlst
                | STM, Some(ED) -> (-), 0u, List.rev rlst
            let rec exec' regs addr cpu n =
                let newAddr = dirOp addr 4u*n
                match mode, regs with
                | _, [] -> cpu, addr // done
                | LDM, reg :: rest -> 
                    match cpu.MM.[WA newAddr] with
                    | DataLoc data -> 
                        { cpu with Regs = cpu.Regs.Add (reg, data); }
                        |> fun newCpu -> exec' rest newAddr newCpu (n+1u)
                    | _ -> failwithf "Invalid memory address." //TODO: make this an error?
                | STM, reg :: rest -> 
                    cpu.Regs.[reg]
                    |> fun data -> { cpu with MM = cpu.MM.Add (WA newAddr, DataLoc data); }
                    |> fun newCpu -> exec' rest newAddr newCpu (n+1u)

            exec' orderedRegList (cpuData.Regs.[targ]) cpuData initialN
        let instr = parsed.PInstr
        let dir, targ, wb, rlst = instr.Direction, instr.Target, instr.WriteBack, instr.RegList
        match instr.InsType with
        | Some(LDM) -> exec LDM dir targ wb rlst
        | Some(STM) -> exec STM dir targ wb rlst
        | None -> failwithf "No instruction type given."