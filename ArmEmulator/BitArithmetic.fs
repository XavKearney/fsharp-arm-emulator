// lex/parse and execute a given subset of instructions
// subset is some data processing instructions
module BitArithmetic



    open CommonLex
    open CommonData
    open ParseExpr




// Types //





    /// instructions
    type InstRoots =  MOV | MVN | AND | ORR | EOR | BIC 
                    | LSL | LSR | ASR | ROR | RRX 
                    | TST | TEQ 

    type Shifter = Lsl | Lsr | Asr | Ror

    type Suffix = S | NA

    /// parse error
    type ErrInstr = string

    /// Flexible opperator
    /// either a number or a register with an optional shift
    type FlexOp = 
        | Literal of uint32
        | Register of RName
        | RegShiftReg of RName*Shifter*RName
        | RegShiftLit of RName*Shifter*uint32
        | RegRRX of RName

    /// Infromation needed for instruction execution
    /// Part of the return from parse
    type InstDecomp = { Instruction: InstRoots
                        Suff: Suffix
                        Dest: RName Option
                        Op1: Result<FlexOp,string>
                        Op2: Result<FlexOp,string>
                        }





// Maps





    /// map of all possible opcodes recognised
    let opCodes = 
        /// sample specification for set of instructions
        let dPSpec = {
            InstrC = BITARITH
            Roots = ["MOV" ; "MVN" ; "AND" ; "ORR" ; "EOR" ; "BIC" ;
                    "LSL" ; "LSR" ; "ASR" ; "ROR" ; "RRX" ; "TST" ; "TEQ"]

            Suffixes = [""; "S"]
        }
        opCodeExpand dPSpec

    /// map used to convert strings into instruction values 
    let instrNames = 
        Map.ofList [ 
            "MOV",MOV ; "MVN",MVN ; "AND",AND ; "ORR",ORR ; "EOR",EOR ; "BIC",BIC ;
            "LSL",LSL ; "LSR",LSR ; "ASR",ASR ; "ROR",ROR ; "RRX",RRX ; "TST",TST ; "TEQ",TEQ ]

    /// map to convert string suffix into DU suffix
    let suffMap = 
        Map.ofList ["S",S ; "",NA]

    /// Map of allowed shifts
    let allowedShifts = 
        Map.ofList ["LSL",Lsl ; "ASR",Asr ; "LSR",Lsr ; "ROR",Ror ]






// check litteral 





    /// checks if an integer can be created by rotating an 8 bit number in a 32 bit word 
    let allowedLiterals num =
        let valid =
            [0..2..30] 
            |> List.allPairs [(uint32 num)] 
            |> List.map (fun (n,r) -> (n >>> r) ||| (n <<< (32-r)))
            |> List.collect (fun n -> [n < 256u])
            |> List.contains true
        match num with
        | x when ((x <= 2147483647) && (x >= -2147483648)) ->
            match valid with 
            | true -> Ok (uint32 num)
            | false -> Error "Litteral can't be created by rotating an 8 bit number in a 32 bit word "
        | _ -> Error "Litteral is out of range"

    /// converts string to some litteral or none
    /// string number must start with #
    let toLit (str : string) (symTab : SymbolTable) =  
        match str.Length=0 with 
        | false -> 
            match str.[0] with
            | '#' -> 
                match evalExpr symTab str.[1..] with
                | Ok num when (int num) >= 0 -> allowedLiterals (int num)
                | Ok num -> 
                    match allowedLiterals (~~~ (int num)) with
                    | Ok _ -> Ok num
                    | _ -> Error "This litteral is not allowed"
                | _ -> Error "Invalid litteral or expression"
            | _ -> Error "No # preceeding expression/litteral"
        | true -> Error "No string passed to toLit function"





// parse instruction





    /// returns the instruction line parsed into its seprate components given 
    /// the root, operands and suffix 
    let parseInstr (root : string) (operands : string) (suffix : string) (symTab : SymbolTable) =
 
        let ops = 
            let splitOps = 
                operands.Split(',') 
                |> Array.map (fun str -> str.Trim())
            match Array.contains "" splitOps with 
            | true -> [|"Input is not valid: There are two ',' back to back"|]
            | false -> splitOps            

        /// converts string to some valid register or none
        let toReg str = Map.tryFind str regNames

        /// converts string that could be a literal or register to a valid literal or register
        let toLitReg regOrLit =
            match toLit regOrLit symTab, toReg regOrLit with
            | Ok lit, None -> Ok (Literal lit)
            | Error _, Some reg -> Ok (Register reg)
            | _ -> Error "Opperand is not valid literal or register" 

        /// converts string to some shift with shift value or none
        let toShift (opp : string) =
            let shiftSplit = opp.Split(' ')
                            |> Array.map (fun str -> str.Trim())
                            |> Array.filter (fun str -> str <> "")
            match shiftSplit.Length with
            | 2 -> 
                match Map.tryFind shiftSplit.[0] allowedShifts,toLitReg shiftSplit.[1] with 
                | Some shift,Ok regOrLit -> Ok (shift,regOrLit)
                | _ -> Error "Invalid shift or register/litteral"
            | _ -> Error "Invalid number of shift opperands"                 

        /// converts a string array to a flexible opperator
        let toFlexOp (strArr : string array) = 
            match strArr with
            | [|regOrLit|] -> toLitReg regOrLit            
            | [|reg ; shift|] -> 
                match toReg reg, Map.tryFind shift instrNames with
                | Some targetReg, Some shift when shift = RRX -> Ok (RegRRX targetReg)
                | Some targetReg, _ ->
                    match toShift shift with 
                    | Ok (shift,regOrLit) -> 
                        match regOrLit with
                        | Literal lit -> Ok (RegShiftLit (targetReg,shift,lit))
                        | Register reg -> Ok (RegShiftReg (targetReg,shift,reg))
                        | _ -> Error "Invalid literal or register following shift instruction"
                    | _ -> Error "Invalid shift instruction"
                | _ -> Error "Invalid target register for shift opperator"
            | _ -> Error "Operands not in a form that can be converted to a flexible opperator"

        let checkReg reg =
            match reg with
            | Some R13 -> None 
            | Some R15 -> None
            | _ -> reg    

        // check this can be done without a when
        /// checks that destenation register or operand register is valid
        let checkValid flexOp = 
            match flexOp with
            | Ok (Register R13) -> Error "Cannot use R13 with this instruction"            
            | Ok (Register R15) -> Error "Cannot use R15 with this instruction"
            | Ok (RegShiftLit (R13,_,_)) -> Error "Cannot use R13 with this instruction"
            | Ok (RegShiftLit (R15,_,_)) -> Error "Cannot use R15 with this instruction" 
            | Ok (RegShiftReg (R13,_,_)) -> Error "Cannot use R13 with this instruction"
            | Ok (RegShiftReg (_,_,R13)) -> Error "Cannot use R13 with this instruction"            
            | Ok (RegShiftReg (R15,_,_)) -> Error "Cannot use R15 with this instruction"
            | Ok (RegShiftReg (_,_,R15)) -> Error "Cannot use R15 with this instruction"
            | _ -> flexOp                      

        let baseInstr = { Instruction = instrNames.[root]
                          Suff = suffMap.[suffix]
                          Dest = None
                          Op1 = Error ""
                          Op2 = Error ""
                        }
            
        match instrNames.[root] with 
        | MOV | MVN when (ops.Length = 2) || (ops.Length = 3) ->
            Ok {baseInstr with Dest = toReg ops.[0]
                                      Op1 = toFlexOp ops.[1..]}

        | TST | TEQ when (ops.Length = 2) || (ops.Length = 3) ->
            Ok {baseInstr with Dest = checkReg (toReg ops.[0]) 
                                      Op1 = checkValid (toFlexOp ops.[1..])}                                  

        | AND | ORR | EOR | BIC when (ops.Length = 3) || (ops.Length = 4) ->
            Ok {baseInstr with Dest = checkReg (toReg ops.[0])
                                      Op1 = checkValid (toFlexOp [|ops.[1]|]) 
                                      Op2 = checkValid (toFlexOp ops.[2..])}

        | LSL | LSR | ASR | ROR when ops.Length = 3 -> 
            Ok {baseInstr with Dest = checkReg (toReg ops.[0])
                                      Op1 = checkValid (toFlexOp [|ops.[1]|])
                                      Op2 = checkValid (toFlexOp ops.[2..])}

        | RRX when ops.Length = 2 -> 
            Ok {baseInstr with Dest = checkReg (toReg ops.[0])
                                      Op1 = toFlexOp [|ops.[1]|]}

        | _ -> Error "Not valid input operands; wrong number of opperands"

    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse (ls: LineData) =
        let parse' (_, (root,suffix,pCond)) =
            let pLab = 
                match ls.Label,ls.LoadAddr with
                | Some lab, WA addr -> Some (lab,addr)
                | _ -> None 
            match ls.SymTab with 
            Some symTable ->     
                match parseInstr root ls.Operands suffix symTable with 
                | Ok pInst -> Ok { PInstr=pInst; PLabel = pLab; PSize = 4u; PCond = pCond }
                | _ -> Error "Parse error"
            | _ -> Error "No symbol tabel"

        // checks Opcode is processed my this module
        // Calls parse' if Opcode is recognised
        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse





// executing instructions





    let intToBool num = 
        match num with
        | 1u -> true
        | 0u -> false
        | _ -> failwithf "Invalid integer being converted to bool"


    // functions that do the bit manipulation

    /// applies the given shift to the first argument, returning the 
    /// required result and the carry the shift produces.
    /// i.e returns (evaluated shift number, carry)
    let doShift n shifter shiftVal =
        let sVal = (int32 shiftVal) % 32  
        match shifter with 
        | Lsl -> n <<< sVal , (((n <<< sVal - 1) >>> 31) |> intToBool)
        | Lsr -> n >>> sVal , (((n >>> (sVal - 1)) &&& 1u) |> intToBool)
        | Asr -> uint32 (int32 n >>> sVal) , (((uint32 (int32 n >>> (sVal - 1))) &&& 1u) |> intToBool)
        | Ror -> (n >>> sVal) ||| (n <<<(32- sVal)) , ((((n >>> (sVal - 1)) ||| (n <<<(31 - sVal))) &&& 1u) |> intToBool)

    let doRRX n carry = 
        let newCarry = (n &&& 1u) |> intToBool        
        (n >>> 1) + (carry <<< 31),newCarry 


    /// updates N Z C flags
    let updateFlags result flags carry = {flags with N = (int32 result) < 0 ; C = carry; Z = (result = 0u)}


    /// evaluates flexible operator
    /// returns (evaluatedOp as a uint32 , carry) 
    let flexEval cpuData op =

        let carry = cpuData.Fl.C 

        match op with
        | Literal lit -> lit,carry
        | Register reg -> cpuData.Regs.[reg],carry
        
        | RegShiftLit (regTarget,shift,lit) ->
            doShift cpuData.Regs.[regTarget] shift lit

        | RegShiftReg (regTarget,shift,reg) ->
            doShift cpuData.Regs.[regTarget] shift cpuData.Regs.[reg]      

        | RegRRX reg -> 
            doRRX cpuData.Regs.[reg] (System.Convert.ToUInt32(carry))


    /// executes the instruction
    let exeInstr cpuData parseOut symTable  =

        match parseOut with
        | Some (Ok exeInfo) ->

            let suffix = exeInfo.PInstr.Suff
            let flags = cpuData.Fl
            let updateRegs dest lit = Map.add dest lit cpuData.Regs

            match exeInfo.PInstr.Instruction, exeInfo.PInstr.Dest, exeInfo.PInstr.Op1, exeInfo.PInstr.Op2 with

            | MOV, Some dest, Ok op1, Error ""  ->
                match flexEval cpuData op1, suffix with 
                | (lit,_), NA  -> Ok {cpuData with Regs = updateRegs dest lit}
                | (lit,carry), S -> Ok {cpuData with Regs = updateRegs dest lit 
                                                            Fl = updateFlags lit flags carry}

            | MVN, Some dest, Ok op1, Error "" ->
                match flexEval cpuData op1, suffix with 
                | (lit,_), NA  -> Ok {cpuData with Regs =updateRegs dest (~~~ lit)}
                | (lit,carry), S -> Ok {cpuData with Regs = updateRegs dest (~~~ lit)
                                                            Fl = updateFlags (~~~ lit) flags carry}       
                                                            
            | AND, Some dest, Ok (Register reg), Ok op2 ->
                match flexEval cpuData op2, suffix with
                | (lit,_), NA -> Ok {cpuData with Regs = updateRegs dest (cpuData.Regs.[reg] &&& lit)}
                | (lit,carry), S -> Ok {cpuData with Regs = updateRegs dest (cpuData.Regs.[reg] &&& lit)
                                                            Fl = updateFlags (cpuData.Regs.[reg] &&& lit) flags carry}

            | ORR, Some dest, Ok (Register reg), Ok op2 ->
                match flexEval cpuData op2, suffix with
                | (lit,_), NA -> Ok {cpuData with Regs = updateRegs dest (cpuData.Regs.[reg] ||| lit)}
                | (lit,carry), S -> Ok {cpuData with Regs = updateRegs dest (cpuData.Regs.[reg] ||| lit)
                                                            Fl = updateFlags (cpuData.Regs.[reg] ||| lit) flags carry}            

            | EOR, Some dest, Ok (Register reg), Ok op2 ->
                match flexEval cpuData op2, suffix with
                | (lit,_), NA -> Ok {cpuData with Regs = updateRegs dest (cpuData.Regs.[reg] ^^^ lit)}
                | (lit,carry), S -> Ok {cpuData with Regs = updateRegs dest (cpuData.Regs.[reg] ^^^ lit)
                                                            Fl = updateFlags (cpuData.Regs.[reg] ^^^ lit) flags carry} 

            | BIC, Some dest, Ok (Register reg), Ok op2 ->
                match flexEval cpuData op2, suffix with
                | (lit,_), NA -> Ok {cpuData with Regs = updateRegs dest (cpuData.Regs.[reg] &&& (~~~  lit))}
                | (lit,carry), S -> Ok {cpuData with Regs = updateRegs dest (cpuData.Regs.[reg] &&& (~~~  lit))
                                                            Fl = updateFlags (cpuData.Regs.[reg] &&& (~~~  lit)) flags carry} 

            | LSL, Some dest, Ok (Register reg), Ok op2 ->
                match flexEval cpuData op2, suffix with
                | (lit,_), NA -> 
                    let (result,_) = doShift cpuData.Regs.[reg] Lsl lit
                    Ok {cpuData with Regs = updateRegs dest result}
                | (lit,_), S ->
                    let (result,carry) = doShift cpuData.Regs.[reg] Lsl lit                        
                    Ok {cpuData with Regs = updateRegs dest result
                                            Fl = updateFlags result flags carry} 

            | LSR, Some dest, Ok (Register reg), Ok op2 ->
                match flexEval cpuData op2, suffix with
                | (lit,_), NA -> 
                    let (result,_) = doShift cpuData.Regs.[reg] Lsr lit
                    Ok {cpuData with Regs = updateRegs dest result}
                | (lit,_), S ->
                    let (result,carry) = doShift cpuData.Regs.[reg] Lsr lit                        
                    Ok {cpuData with Regs = updateRegs dest result
                                            Fl = updateFlags result flags carry}                                                 
                                                                  
            | ASR, Some dest, Ok (Register reg), Ok op2 ->
                match flexEval cpuData op2, suffix with
                | (lit,_), NA -> 
                    let (result,_) = doShift cpuData.Regs.[reg] Asr lit
                    Ok {cpuData with Regs = updateRegs dest result}
                | (lit,_), S ->
                    let (result,carry) = doShift cpuData.Regs.[reg] Asr lit                        
                    Ok {cpuData with Regs = updateRegs dest result
                                            Fl = updateFlags result flags carry}     

            | ROR, Some dest, Ok (Register reg), Ok op2 ->
                match flexEval cpuData op2, suffix with
                | (lit,_), NA -> 
                    let (result,_) = doShift cpuData.Regs.[reg] Ror lit
                    Ok {cpuData with Regs = updateRegs dest result}
                | (lit,_), S ->
                    let (result,carry) = doShift cpuData.Regs.[reg] Ror lit                        
                    Ok {cpuData with Regs = updateRegs dest result
                                            Fl = updateFlags result flags carry}  

            | RRX, Some dest, Ok op1, Error "" -> 
                match flexEval cpuData op1, suffix with
                | (lit,_), NA -> 
                    let (result,_) = doRRX lit (System.Convert.ToUInt32(flags.C))
                    Ok {cpuData with Regs = updateRegs dest result}                                   
                | (lit,_), S ->
                    let (result,carry) = doRRX lit (System.Convert.ToUInt32(flags.C))                        
                    Ok {cpuData with Regs = updateRegs dest result
                                            Fl = updateFlags result flags carry} 

            | TST, Some dest, Ok op1, Error ""  ->
                match flexEval cpuData op1 with 
                | lit,carry -> Ok {cpuData with Fl = updateFlags (cpuData.Regs.[dest] &&& lit) flags carry}

            | TEQ, Some dest, Ok op1, Error "" ->
                match flexEval cpuData op1 with 
                | lit,carry -> Ok {cpuData with Fl = updateFlags (cpuData.Regs.[dest] ^^^ lit) flags carry}  

            | _ -> Error "Execution error"

        | Some _ -> Error "Return from parse is an error"
        | _ -> Error "Return from parse is none" 
