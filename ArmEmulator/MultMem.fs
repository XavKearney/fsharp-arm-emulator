/// handles LDM/STM instructions
/// author: Xav Kearney
module MultMem
    
    open CommonLex
    open CommonData

    /// sample specification for set of instructions

    // change these types as required

    type multIns = LDM | STM
    type multDirection = FD | FA | ED | EA 
    
    /// multiple memory access instruction type
    type multMemInstr = 
        {
            // load or store
            InsType: multIns option;
            // stack direction
            Direction: multDirection option;
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
        let sLst = ops.Split(',') |> Array.map (fun s-> s.Trim())
        let targetStr = sLst.[0]
        let reglstStr = String.concat "," sLst.[1..]
        let wb = targetStr.[String.length targetStr - 1] = '!'
        let target = 
            regNames.TryFind 
            <| targetStr.[..String.length targetStr - 1 - System.Convert.ToInt32(wb)]
        let matchRegLst wb targ =
            match reglstStr.[0] = '{' && reglstStr.[String.length reglstStr - 1] = '}' with
            | true ->
                let reglst = 
                    reglstStr.[1..String.length reglstStr - 2].Split(",")
                    |> Array.toList
                    |> List.map regNames.TryFind
                if List.contains None reglst
                then Error ("Invalid list of registers.")
                else 
                List.map (fun r -> 
                            match r with
                            | Some x -> x
                            | _ -> failwithf "Should never happen.") reglst
                |> fun rlst -> Ok (targ, wb, rlst)
            | false -> Error ("Incorrect brackets around list.")
        match target with
        | Some t -> matchRegLst wb t
        | None -> Error ("Target register not found.")



    let makeMemIns root suf operands =
        // try to parse operands from string
        match parseOps operands with
        | Ok (target, wb, reglst) ->
            // create template result
            let defaultIns = {
                InsType = None; Direction = None;
                Target = target; WriteBack = wb; RegList = reglst}
            match root, suf with
            // LDM instructions, including synonyms
            | "LDM", ("" | "FD" | "IA") ->
                Ok { defaultIns with InsType = Some(LDM); Direction = Some(FD); } 
            | "LDM", ("EA" | "DB") ->
                Ok { defaultIns with InsType = Some(LDM); Direction = Some(EA); } 
            | "LDM", "FA" ->
                Ok { defaultIns with InsType = Some(LDM); Direction = Some(FA); } 
            | "LDM", "ED" ->
                Ok { defaultIns with InsType = Some(LDM); Direction = Some(ED)  ; } 
            // STM instructions, including synonyms
            | "STM", ("" | "EA" | "IA") ->
                Ok { defaultIns with InsType = Some(STM); Direction = Some(EA); } 
            | "STM", ("FD" | "DB") ->
                Ok { defaultIns with InsType = Some(STM); Direction = Some(FD); } 
            | "STM", "FA" ->
                Ok { defaultIns with InsType = Some(STM); Direction = Some(FA); } 
            | "STM", "ED" ->
                Ok { defaultIns with InsType = Some(STM); Direction = Some(ED); } 
            // unsupported instruction
            | _ -> Error ("Opcode not supported.")
        | Error s -> Error s


    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse ls: Result<Parse<multMemInstr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =
            let (WA la) = ls.LoadAddr
            match instrC with
            | MEM -> 
                match makeMemIns root suffix ls.Operands with
                | Ok pinstr -> Ok {
                        // make the memory instruction
                        PInstr = pinstr;
                        // TODO: check this is correct
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