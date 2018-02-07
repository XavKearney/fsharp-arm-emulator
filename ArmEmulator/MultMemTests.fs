module MultMemTests
    open CommonData
    open CommonLex
    open MultMem
    open Expecto

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

        testProperty "Test Parse" <| fun wa opcode suffix target wb rLst ->
            let ls = makeLineData wa opcode suffix target wb rLst
            let expected = 
                match rLst with
                | [] -> Some (Error "Invalid list of registers.")
                | _ -> Some (Ok {
                        PInstr =  { InsType = Some(opcode); Direction = Some(suffix);
                                    Target = target; WriteBack = wb; RegList = rLst} ;
                        PLabel = None; PSize = 4u; PCond = Cal;
                    })
            let res = parse ls
            Expect.equal res expected "parse1"