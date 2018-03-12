// lex/parse and execute a given subset of instructions
// subset is some data processing instructions
module BitArithmetic



    open CommonLex
    open CommonData
    open ParseExpr
    open System.Text.RegularExpressions
    open FsCheck
    open Expecto





// Types //





    /// instructions
    type InstRoots =  MOV | MVN | AND | ORR | EOR | BIC 
                    | LSL | LSR | ASR | ROR | RRX 
                    | TST | TEQ 

    type Shifter = Lsl | Lsr | Asr | Ror

    type Suffix = S | NA

    /// parse error
    type ErrInstr = string

    type LitOrReg = Nm of uint32 | Rg of RName

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
                match evalExpr symTab str with
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
            | _, Some reg -> Ok (Register reg)
            | _ -> Error "Opperand is not literal or register" 

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
                | _ -> Error "Invalid target register"
            | _ -> Error "Operands not in a form that can be converted to a flexible opperator"          

        let baseInstr = { Instruction = instrNames.[root]
                          Suff = suffMap.[suffix]
                          Dest = None
                          Op1 = Error ""
                          Op2 = Error ""
                        }
            
        match instrNames.[root] with 
        | MOV | MVN | TST | TEQ when (ops.Length = 2) || (ops.Length = 3)
            -> Ok {baseInstr with Dest = toReg ops.[0] ; Op1 = toFlexOp ops.[1..]}

        | AND | ORR | EOR | BIC when (ops.Length = 3) || (ops.Length = 4) 
            -> Ok {baseInstr with Dest = toReg ops.[0] ; Op1 = toFlexOp [|ops.[1]|] ; Op2 = toFlexOp ops.[2..]}

        | LSL | LSR | ASR | ROR when ops.Length = 3
            -> Ok {baseInstr with Dest = toReg ops.[0] ; Op1 = toFlexOp [|ops.[1]|] ; Op2 = toFlexOp ops.[2..]}

        | RRX when ops.Length = 2
            -> Ok {baseInstr with Dest = toReg ops.[0] ; Op1 = toFlexOp [|ops.[1]|]}

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



    // functions that carry out the bit manipulation
    let AndTst a b = a &&& b
    let Orr a b = a ||| b
    let EorTeq a b = a ^^^ b
    let Bic a b = a &&& (~~~ b)
    let Lsl n shiftVal = n <<< int32 shiftVal
    let Lsr n shiftVal = n >>> int32 shiftVal
    let Ror n rotateVal = (n>>> int32 rotateVal) ||| (n<<<(32- int32 rotateVal))
    let Asr n shiftVal = uint32 (int32 n >>> int32 shiftVal)
    let Rrx n carry = (n >>> 1) + (carry <<< 31)
    let selectMSB n = uint32 n >>> 31
    let selectLSB n = uint32 n &&& 1u    

    /// checks the carry after a shift opperation 
    /// Not used for RRX
    let shiftCarry n shiftVal shift msbOrLsb = 
        let penVal = shift n (shiftVal- 1u)
        msbOrLsb penVal


    /// evaluates flexible operator 
    /// if suffix is "S" then update C flag
    let flexEval cpuData (suffix : string) (flexOp : FlexOp) =  
        let regContent r =  Map.tryFind r (cpuData.Regs)
        let carryNum = System.Convert.ToUInt32(cpuData.Fl.C)
        let litRegNum litOrReg = 
            match litOrReg with
            | Nm num -> Some num
            | Rg reg -> regContent reg
        match flexOp with
        | Num n -> Some n,carryNum
        | RegShiftOp (r,None) -> regContent r,carryNum
        | RegShiftOp (r,Some (instroot,None)) when instroot = RRX -> 
            match regContent r,suffix with
            | Some regCont,"" -> Some (Rrx regCont carryNum),carryNum
            | Some regCont,"S" -> Some (Rrx regCont carryNum),selectLSB regCont  
            | _ -> None,carryNum
        | RegShiftOp (r,Some (instroot,Some litOrReg)) ->
            match regContent r with
            | Some regCont -> 
                match instroot with
                | LSL -> 
                    match litRegNum litOrReg,suffix with           
                    | Some num,"" -> Some (Lsl regCont num),carryNum
                    | Some num,"S" -> Some (Lsl regCont num),(shiftCarry regCont num Lsl selectMSB)
                    | _ -> None,carryNum
                | LSR ->
                    match litRegNum litOrReg,suffix with           
                    | Some num,"" -> Some (Lsr regCont num),carryNum
                    | Some num,"S" -> Some (Lsr regCont num),(shiftCarry regCont num Lsr selectLSB)
                    | _ -> None,carryNum
                | ASR ->
                    match litRegNum litOrReg,suffix with           
                    | Some num,"" -> Some (Asr regCont num),carryNum
                    | Some num,"S" -> Some (Asr regCont num),(shiftCarry regCont num Asr selectLSB)  
                    | _ -> None,carryNum
                | ROR ->
                    match litRegNum litOrReg,suffix with           
                    | Some num,"" -> Some (Ror regCont num),carryNum 
                    | Some num,"S" -> Some (Ror regCont num),(shiftCarry regCont num Ror selectLSB)  
                    | _ -> None,carryNum                                   
                | _ -> None,carryNum
            | _ -> None,carryNum
        | _ -> None,carryNum

    /// decides if instruction should be executed based on condidtion
    /// if instruction should be executed return is true, otherwise its false     
    let exeCond flags cond =
        let z = flags.Z
        let n = flags.N
        let v = flags.V
        let c = flags.C
        let exeDecide flag exeCondition =
            match flag = exeCondition with
            | true -> true
            | false -> false
        match cond with 
        | Ceq -> exeDecide z true                // execute if Z=1
        | Cne -> exeDecide z false               // execute if Z=0      
        | Cmi -> exeDecide n true                // execute if N=1
        | Cpl -> exeDecide n false               // execute if N=0
        | Cvs -> exeDecide v true                // execute if V = 1
        | Cvc -> exeDecide v false               // execute if V = 0            
        | Chs -> exeDecide c true                // execute if C = 1
        | Clo -> exeDecide c false               // execute if C = 0            
        | Cge -> exeDecide n v                   // execute if N = V            
        | Clt -> exeDecide n (not v)             // execute if N != V            
        | Chi ->                                 // execute if C = 1 and Z = 0
            match c with
            | true -> exeDecide z false
            | false -> true
        | Cls ->                                 // execute if C = 0 or Z = 1
            match  c with
            | true -> exeDecide z true
            | false -> false
        | Cgt ->                                 // execute if Z = 0 and N = V 
            match z with
            | true -> false
            | false -> exeDecide n v
        | Cle ->                                 // execute if Z = 1 and N != V"
            match z with
            | true -> exeDecide n (not v)
            | false -> false
        | Cnv -> false                           // never execute
        | Cal -> true                            // always execute

    /// updates N Z C flags
    let updateNZC flgs result carry =
        let isNeg = int32 result < 0 
        let isZero = result = 0u
        let convCarry = 
            match carry with
            | 0u -> false
            | 1u -> true
        match isNeg,isZero with 
        | true,false -> Ok {flgs with N = true ; Z=false ; C=convCarry}
        | false,true -> Ok {flgs with N = false ; Z=true ; C=convCarry}
        | false,false -> Ok {flgs with N = false ; Z=false ; C=convCarry}
        | _ -> Error "can't be both negative and zero"


    /// executes the instruction
    let exeInstr (cpuData : DataPath<'a>) parseOut symTable =

        match parseOut with
        | Some (Ok exeInfo) ->

            let suffix = exeInfo.PInstr.Suff
            let regContent r =  Map.tryFind r cpuData.Regs

            match exeCond cpuData.Fl exeInfo.PCond with
            | true -> 
                match exeInfo.PInstr.Instruction, exeInfo.PInstr.Dest, exeInfo.PInstr.Op1, exeInfo.PInstr.Op2 with
                | MOV, Some dest, Ok op1, Error ""  ->
                    match flexEval cpuData suffix op1, suffix with 
                    | Some (lit,_), NA  -> Ok {cpuData with Regs = Map.add dest lit cpuData.Regs}
                    | Some (lit,carry), S -> Ok {cpuData with Regs = Map.add dest lit cpuData.Regs ; Fl = updateFlags flags carry}
                    | _ -> Error "..."
                | MVN, Some dest, Ok op1, Error "" ->
                    match flexEval cpuData suffix op1, suffix with 
                    | Some (lit,_), NA  -> Ok {cpuData with Regs = Map.add dest (~~~ lit) cpuData.Regs}
                    | Some (lit,carry), S -> Ok {cpuData with Regs = Map.add dest (~~~ lit) cpuData.Regs ; Fl = updateFlags flags carry}
                    | _ -> Error "..."                 
                | AND, Some dest, Ok (Register reg), Ok op2 ->
                    match flexEval cpuData suffix op2, suffix with
                    | Some (lit,_), NA -> Ok {cpuData with Regs = Map.add dest (cpuData.Regs.[reg] &&& lit) cpuData.Regs}
                    | Some (lit,carry), S -> Ok {cpuData with Regs = Map.add dest (cpuData.Regs.[reg] &&& lit) cpuData.Regs ; Fl = updateFlags flags carry}
                    | _ -> Error "..."
                | ORR, Some dest, Ok (Register reg), Ok op2 ->
                    match flexEval cpuData suffix op2, suffix with
                    | Some (lit,_), NA -> Ok {cpuData with Regs = Map.add dest (cpuData.Regs.[reg] ||| lit) cpuData.Regs}
                    | Some (lit,carry), S -> Ok {cpuData with Regs = Map.add dest (cpuData.Regs.[reg] ||| lit) cpuData.Regs ; Fl = updateFlags flags carry}
                    | _ -> Error "..."           
                | EOR, Some dest, Ok (Register reg), Ok op2 ->
                    match flexEval cpuData suffix op2, suffix with
                    | Some (lit,_), NA -> Ok {cpuData with Regs = Map.add dest (cpuData.Regs.[reg] ^^^ lit) cpuData.Regs}
                    | Some (lit,carry), S -> Ok {cpuData with Regs = Map.add dest (cpuData.Regs.[reg] ^^^ lit) cpuData.Regs ; Fl = updateFlags flags carry}
                    | _ -> Error "..."                    
                | BIC, Some dest, Ok (Register reg), Ok op2 ->
                    match flexEval cpuData suffix op2, suffix with
                    | Some (lit,_), NA -> Ok {cpuData with Regs = Map.add dest (cpuData.Regs.[reg] &&& (~~~  lit)) cpuData.Regs}
                    | Some (lit,carry), S -> Ok {cpuData with Regs = Map.add dest (cpuData.Regs.[reg] &&& (~~~ lit)) cpuData.Regs ; Fl = updateFlags flags carry}
                    | _ -> Error "..." 
                | LSL, Some dest, Ok (Register reg), Ok op2 ->
                    match flexEval cpuData suffix op2, suffix with
                    | Some (lit,_), NA -> Ok {cpuData with Regs = Map.add dest (cpuData.Regs.[reg] <<<  lit) cpuData.Regs}
                    | Some (lit,_), S -> Ok {cpuData with Regs = Map.add dest (cpuData.Regs.[reg] <<< lit) cpuData.Regs ; Fl = updateFlags flags (carry)}
                    | _ -> Error "..."                 
                | LSR ->
                | ASR ->
                | ROR ->
                | RRX ->
                | TST ->
                | TEQ ->

            | false -> Ok cpuData
        | _ -> Error "Return from parse is None or error"

        
