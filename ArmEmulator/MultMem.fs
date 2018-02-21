/// handles LDM/STM instructions
/// author: Xav Kearney
module MultMem
    
    open CommonData
    open CommonLex
    open System.Text.RegularExpressions
    open ParseExpr

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
    
    /// branch instruction type
    type BranchInstr = 
        {   
            // address to branch to
            // optional because it can require 2nd pass
            BranchAddr: uint32 option;
            // if optional link "L" suffix set
            // this stores the address of the next instruction
            LinkAddr: WAddr option;
        }

    type EndInstr = END

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

    /// --------- MISC INSTRUCTIONS -----------
    let branchSpec = {
        InstrC = MISC
        Roots = ["B"]
        Suffixes = [""; "L";]
    }

    let endSpec = {
        InstrC = MISC
        Roots = ["END"]
        Suffixes = [""]
    }

    /// return type to allow parse to return multiple instruction types
    type ReturnInstr = | MemI of MultMemInstr | BranchI of BranchInstr

    /// maps of all possible opcodes recognised
    let multMemOpCodes = opCodeExpand multMemSpec
    let branchOpCodes = opCodeExpand branchSpec
    let endOpCodes = opCodeExpand endSpec

    /// take a string of all LDM/STM operands and parse
    /// into target reg, writeback and list of reg
    /// to load/store
    /// returns error if anything is incorrect
    let parseMemOps (ops: string) =
        // split the operands by ',' e.g. "R10!, {R1,R2,R3}"
        let sLst = ops.Split(',') |> Array.map (fun s-> s.Trim()) |> Array.toList
        // get the target register as a string, e.g. "R10!"
        let targetStr = sLst.Head
        // check for writeback suffix '!'
        let wb = targetStr.EndsWith('!')
        // get the target register's RName from string without '!' suffix
        let target = regNames.TryFind (targetStr.Trim('!'))
        // recombine the list of registers
        let reglstStr = String.concat "," sLst.Tail

        let matchRegLst =
            // if string starts and ends with curly braces, get string inside
            match reglstStr with
            | GetInside "{" "}" regStr ->
                match regStr with
                // check for a register range (e.g. {R0-R5}), returns start and end registers
                // if active pattern matches, get start and end registers
                | MatchGroups @"^([A-Z0-9]{2,3})(?:-)([A-Z0-9]{2,3})$" (rStart :: [rEnd]) ->
                    // check the start and end registers are valid
                    List.map regNames.TryFind [rStart; rEnd]
                    |> fun lst -> 
                        match List.contains None lst with
                        | true -> Error "Invalid register list range." 
                        | false ->  Ok (List.choose id lst)
                    // get the range as integers
                    |> Result.bind (
                        function
                        | rNameStart :: [rNameEnd] -> Ok [rNameStart.RegNum..rNameEnd.RegNum]
                        | _ -> Error "Invalid register list range.")
                    // if list is empty, rNameStart must be a higher reg than rNameEnd
                    // so throw an error
                    |> Result.bind (fun lst -> 
                        match List.isEmpty lst with
                        | true -> Error "Invalid register list range." 
                        | false ->  Ok (List.map inverseRegNums.TryFind lst))
                // if not a range, check for register list (e.g. {R1,R3,R5})
                | Matches @"([A-Z0-9]{2,3})+" (regNameLst)  ->
                    List.map regNames.TryFind regNameLst
                    |> Ok
                | _ -> Error "Invalid list of registers."
                // check the final list is valid and return
            | _ -> Error ("Incorrectly formatted operands.")
            |> Result.bind (
                fun lst -> 
                match List.contains None lst with
                | true -> Error "Invalid list of registers."
                | false -> Ok(List.choose id lst)
            )
            
        match target with
        | Some t -> 
            matchRegLst
            |> Result.map (fun regLst -> (t, wb, regLst))
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
        | _ -> Ok (MemI ins)

    /// take opcode root, suffix and string of operands
    /// if the operands parse, return instruction
    /// if not, return an error
    let makeMultMemInstr root suffix operands =
        parseMemOps operands
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
            | _ -> Error "Opcode not supported."
        )


    /// takes a suffix and operands as strings
    /// determines whether the branch corresponds to a link ("L")
    /// and gets the addres corresponding to the expression in the operands
    /// will return None for all fields if symbol table not given (1st pass)
    let makeBranchInstr (suffix, operands:string, WA loadaddr, symTab:Map<string,uint32> option) =
        match symTab with
        // symbol table given (2nd pass), so can process expression
        | Some syms ->
            let branchAddr = evalExpr syms operands
            let linkAddr = 
                match suffix with
                // link address is address of next instruction (current address + 4)
                | "L" -> Some(WA (loadaddr + 4u))
                | _ -> None
            // if valid, and symtable not none, try find address of label
            branchAddr
            // if valid, return branch instruction
            |> Result.map (fun bAddr ->
                BranchI {
                    BranchAddr = Some (bAddr);
                    LinkAddr = linkAddr;
                }
            )
        // no symbol table given, must be first pass
        | None -> BranchI { BranchAddr = None; LinkAddr = None;} |> Ok

    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse ls =
        let parse' (instrC, (root,suffix,pCond)) =
            let (WA la) = ls.LoadAddr
            // generate correct instruction type based on Instruction Class
            match instrC with
            | MEM -> makeMultMemInstr root suffix ls.Operands
            | MISC -> 
                match root with
                | "B" -> makeBranchInstr (suffix, ls.Operands, ls.LoadAddr, ls.SymTab)
                | _ -> Error "Invalid instruction root."
            | _ -> Error "Instruction class not supported."
            // if valid instruction, wrap in Parse type
            |> Result.map (
                fun instr ->  
                    {
                        PInstr = instr;
                        PLabel = ls.Label |> Option.map (fun lab -> lab, la); 
                        PSize = 4u; 
                        PCond = pCond;
                    })
        // check each of the opcode maps to see if any match
        [multMemOpCodes; branchOpCodes; endOpCodes]
        |> List.choose (Map.tryFind ls.OpCode)
        |> function 
            // should only be a single result, if so, parse it
            | [instr] -> Some(parse' instr)
            | _ -> None


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
                    match cpu.MM.TryFind (WA addr) with
                    | Some(DataLoc d) -> 
                        { cpu with Regs = cpu.Regs.Add (reg, d); }
                        |> fun newCpu -> exec' rest newAddr newCpu
                    | _ -> Error "Invalid memory address."
                | STM, reg :: rest -> 
                    cpu.Regs.[reg]
                    |> fun d -> { cpu with MM = cpu.MM.Add (WA addr, DataLoc d); }
                    |> exec' rest newAddr
            
            // execute the instruction
            exec' orderedRLst (dirOp cpuData.Regs.[targ] (initialN*4u)) cpuData
            |> function
                | Ok(cpu, addr) ->
                    // check for writeback
                    match wb with
                    | true -> Ok ({cpu with Regs = cpu.Regs.Add (targ, addr)};)
                    | false -> Ok cpu
                | Error s -> Error s

        let instr = parsed.PInstr
        let dir, targ, wb, rlst = instr.Direction, instr.Target, instr.WriteBack, instr.RegList
        checkValid instr
        |> Result.bind (fun _ ->
            match instr.InsType with
            | Some(LDM) -> exec LDM dir targ wb rlst
            | Some(STM) -> exec STM dir targ wb rlst
            | None -> failwithf "No instruction type given.")

    /// executes a parsed branch instruction
    /// given cpuData        
    let execBranchInstr parsed cpuData =
        let instr = parsed.PInstr
        match instr.BranchAddr, instr.LinkAddr with
        | Some bAddr, None ->
            // set PC to branch address
            Ok {cpuData with Regs = cpuData.Regs.Add (R15, bAddr)}
        | Some bAddr, Some (WA lAddr) ->
            // set PC to branch address
            let branchedCpu = {cpuData with Regs = cpuData.Regs.Add (R15, bAddr)}
            // and set LR to link address
            Ok {branchedCpu with Regs = cpuData.Regs.Add (R14, lAddr)}
        | _ -> Error "Invalid branch instruction."
            

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = parse


