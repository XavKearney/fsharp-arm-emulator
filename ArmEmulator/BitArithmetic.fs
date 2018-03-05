// lex/parse and execute a given subset of instructions
// subset is some data processing instructions
module BitArithmetic



    open CommonLex
    open CommonData
    open System.Text.RegularExpressions





// Types //





    /// instructions
    type InstRoots =  MOV | MVN | AND | ORR | EOR | BIC | LSL | LSR | ASR
                    | ROR | RRX | TST | TEQ 

    /// parse error
    type ErrInstr = string

    type LitOrReg = Nm of uint32 | Rg of RName

    /// Flexible opperator, can either be a number or a register with an optional shift
    type FlexOp = 
        | Num of uint32
        | RegShiftOp of RName*(InstRoots*LitOrReg Option) Option

    type RegOrFlexOp =
        | Reg of RName option
        | Flex of FlexOp option

    /// Infromation needed for instruction execution
    /// Part of the return from parse
    type InstDecomp = { instruction: InstRoots
                        suff: string
                        opA: RName Option
                        opB: RegOrFlexOp Option
                        opC: RegOrFlexOp Option
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

    /// Map of allowed shifts
    let allowedShifts = 
        Map.ofList ["LSL",LSL ; "ASR",ASR ; "LSR",LSR ; "ROR",ROR ; "RRX",RRX]





// check litteral 
// need to include expressions i.e * + - ()





    let (|FirstMatch|_|) pattern input =
        let m = Regex.Match(input,pattern)
        match m.Success with
        | true -> Some (m.Value)
        | false -> None

    let (|CheckLit|_|) input = 
        match input with
        | FirstMatch @"^(-?0[xX][a-fA-F0-9]+)$" x -> Some (int x)
        | FirstMatch @"^(-?&[a-fA-F0-9]+)$" x -> Some (int ("0x"+x.[1..])) 
        | FirstMatch @"^(-?0[bB][0-1]+)$" x -> Some (int x)
        | FirstMatch @"^(-?[0-9]+)$" x -> Some (int x)
        | _ -> None

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
            | true -> Some (uint32 num)
            | false -> None
        | _ -> None

    /// converts string to some litteral or none
    /// string number must start with #
    let toLit (str : string) =  
        match str.Length=0 with 
        | false -> 
            match str.[0] with
            | '#' -> 
                match str.[1..] with
                | CheckLit n -> 
                    match n >= 0 with
                    | true -> allowedLiterals n
                    | false ->
                        match allowedLiterals (-n) with
                        | Some _ -> Some (uint32 n)
                        | _ -> None
                | _ -> None
            | _ -> None
        | true -> None





// parse instruction





    /// returns the instruction line parsed into its seprate components given 
    /// the root, operands and suffix 
    let parseInstr (root : string) (operands : string) (suffix : string) =
 
        let ops = 
            let splitOps = 
                operands.Split(',') 
                |> Array.map (fun str -> str.Trim())
            match Array.contains "" splitOps with 
            | true -> [|"Not valid input"|]
            | false -> splitOps            

        /// converts string to some valid register or none
        let toReg str = Map.tryFind str regNames

        /// converts string to some shift with shift value or none
        /// if RRX then shift value is none
        let toShift (opp : string) =
            let shiftSplit = opp.Split(' ')
                            |> Array.map (fun str -> str.Trim())
                            |> Array.filter (fun str -> str <> "")
            match shiftSplit.Length with
            | 1 -> 
                let cShift = Map.tryFind shiftSplit.[0] allowedShifts
                match cShift with
                | Some shift when shift=RRX -> Some (shift,None)
                | _ -> None 
            | 2 ->
                let cShift = Map.tryFind shiftSplit.[0] allowedShifts
                let cReg = Map.tryFind shiftSplit.[1] regNames
                let cLit = toLit shiftSplit.[1]
                match cShift,cReg,cLit with 
                | Some shift,_,_ when shift = RRX -> None
                | Some shift, Some r, None -> Some (shift,Some (Rg r))
                | Some shift,None,Some lit -> Some (shift,Some (Nm lit))
                | _ -> None
            | _ -> None

        /// converts string to a valid literal or register or is none
        let toLitReg str =                                  
            let checkLit = toLit str
            let checkReg = toReg str
            match checkLit,checkReg with
            | Some lit, None -> Some (Nm lit)
            | None, Some reg -> Some (Rg reg)
            | _ -> None

        /// converts array of strings to a flexible opperator (literal or register with optional shift)
        let toFlexOp (strArr : string array) = 
            match strArr with
            | [|reg ; shift|] -> 
                match toReg reg, toShift shift with
                | Some r, Some s -> Some (RegShiftOp (r,Some s))
                | _ -> None
            | [|regOrLit|] ->
                match toLitReg regOrLit with
                | Some (Nm lit) -> Some (Num lit)
                | Some (Rg r) -> Some (RegShiftOp (r,None))
                | _ -> None
            | _ -> None

        let baseInstr = { instruction = instrNames.[root]
                          suff = suffix
                          opA = None
                          opB = None
                          opC = None
                        }
            
        match instrNames.[root] with 
        | MOV | MVN | TST | TEQ when (ops.Length = 2) || (ops.Length = 3)
            -> Ok {baseInstr with opA = toReg ops.[0] ; opB = Some (Flex (toFlexOp ops.[1..]))}

        | AND | ORR | EOR | BIC when (ops.Length = 3) || (ops.Length = 4) 
            -> Ok {baseInstr with opA = toReg ops.[0] ; opB = Some (Reg (toReg ops.[1])) ; opC = Some (Flex (toFlexOp ops.[2..]))}

        | LSL | LSR | ASR | ROR when ops.Length = 3
            -> Ok {baseInstr with opA = toReg ops.[0] ; opB = Some (Reg (toReg ops.[1])) ; opC = Some (Reg (toReg ops.[2]))}

        | RRX when ops.Length = 2
            -> Ok {baseInstr with opA = toReg ops.[0] ; opB = Some(Reg (toReg ops.[1]))}

        | _ -> Error "Not valid input"


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
            match parseInstr root ls.Operands suffix with 
            | Ok pInst -> Ok { PInstr=pInst; PLabel = pLab; PSize = 4u; PCond = pCond }
            | _ -> Error "Parse error"
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
    let Lsr n shiftVal = uint32 n >>> int32 shiftVal
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
        | Cnv -> true
        | Cal -> false

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
        | _ -> Error "can't be both negative and zero"


    /// executes the instruction
    let exeInstr cpuData parseOut =

        match parseOut with
        | Some (Ok exeInfo) ->
            let instrRoot = exeInfo.PInstr.instruction
            let suffix = exeInfo.PInstr.suff
            let opa = exeInfo.PInstr.opA            
            let opb = exeInfo.PInstr.opB
            let opc = exeInfo.PInstr.opC
            let regContent r =  Map.tryFind r (cpuData.Regs)
            let carryNum = System.Convert.ToUInt32(cpuData.Fl.C)

            /// Returns a 2 tuple of (evaluated flexible opperator,carry) given an Opperand
            let evalRegOrFlexOps op = 
                match op with
                | Some (Reg (Some rName)) -> Some ((regContent rName),carryNum)
                | Some (Flex (Some flexOp)) -> Some (flexEval cpuData suffix flexOp)
                | _ -> None
            let evalCarry op =
                match op with 
                | Some (_,c) -> Some c
                | _ -> None
            let evalOp op = 
                match op with
                | Some (n, _) -> n
                | _ -> None
            let calcCarry = evalRegOrFlexOps >> evalCarry      
            let calcOp = evalRegOrFlexOps >> evalOp
            let evalInstruction =    
                match exeCond cpuData.Fl exeInfo.PCond with
                | true ->
                    match instrRoot with
                    | AND -> 
                        let impInstr =
                            match calcOp opb,calcOp opc with 
                            | Some n1,Some n2 -> Some (AndTst n1 n2)
                            | _ -> None
                        match impInstr,calcCarry opc,suffix with    
                        | Some n,_,"" -> Some (n,carryNum)
                        | Some n,Some c,"S" -> Some (n,c)
                        | _ -> None  
                    | ORR -> 
                        let impInstr =
                            match calcOp opb,calcOp opc with 
                            | Some n1,Some n2 -> Some (Orr n1 n2)
                            | _ -> None
                        match impInstr,calcCarry opc,suffix with
                        | Some n,_,"" -> Some (n,carryNum)    
                        | Some n,Some c,"S" -> Some (n,c)
                        | _ -> None                          
                    | EOR -> 
                        let impInstr =
                            match calcOp opb,calcOp opc with 
                            | Some n1,Some n2 -> Some (EorTeq n1 n2)
                            | _ -> None
                        match impInstr,calcCarry opc,suffix with
                        | Some n,_,"" -> Some (n,carryNum)    
                        | Some n,Some c,"S" -> Some (n,c)
                        | _ -> None                                    
                    | BIC -> 
                        let impInstr =
                            match calcOp opb,calcOp opc with 
                            | Some n1,Some n2 -> Some (Bic n1 n2)
                            | _ -> None
                        match impInstr,calcCarry opc,suffix with
                        | Some n,_,"" -> Some (n,carryNum)    
                        | Some n,Some c,"S" -> Some (n,c)
                        | _ -> None                                                              
                    | LSL -> 
                        match calcOp opb,calcOp opc,suffix with
                        | Some n1,Some n2,"" -> Some (Lsl n1 n2,carryNum) 
                        | Some n1,Some n2,"S" -> Some (Lsl n1 n2,shiftCarry n1 n2 Lsl selectMSB)
                        | _ -> None                                      
                    | LSR -> 
                        match calcOp opb,calcOp opc,suffix with 
                        | Some n1,Some n2,"" -> Some (Lsl n1 n2,carryNum) 
                        | Some n1,Some n2,"S" -> Some (Lsl n1 n2,shiftCarry n1 n2 Lsr selectLSB)
                        | _ -> None                          
                    | ROR -> 
                        match calcOp opb,calcOp opc,suffix with
                        | Some n1,Some n2,"" -> Some (Lsl n1 n2,carryNum)  
                        | Some n1,Some n2,"S" -> Some (Lsl n1 n2,shiftCarry n1 n2 Ror selectLSB)
                        | _ -> None                       
                    | ASR -> 
                        match calcOp opb,calcOp opc,suffix with
                        | Some n1,Some n2,"" -> Some (Lsl n1 n2,carryNum)  
                        | Some n1,Some n2,"S" -> Some (Lsl n1 n2,shiftCarry n1 n2 Asr selectLSB)
                        | _ -> None                      
                    | MOV -> 
                        match calcOp opb,calcCarry opb,suffix with
                        | Some n,_,"" -> Some (n,carryNum)
                        | Some n,Some c,"S" -> Some (n,c)
                        | _ -> None  
                    | MVN ->
                        match calcOp opb,calcCarry opb,suffix with
                        | Some n,_,"" -> Some (~~~ n,carryNum)
                        | Some n,Some c,"S" -> Some (~~~ n,c)
                        | _ -> None                                    
                    | TST -> 
                        match opa,calcOp opb with
                        | Some r,Some n -> 
                            match regContent r,calcCarry opb with 
                            | Some rCont,Some c -> Some (AndTst rCont n,c)
                            | _ -> None
                        | _ -> None
                    | TEQ -> 
                        match opa,calcOp opb with
                        | Some r,Some n -> 
                            match regContent r,calcCarry opb with 
                            | Some rCont,Some c -> Some (EorTeq rCont n,c)
                            | _ -> None
                        | _ -> None                
                    | RRX -> 
                        match calcOp opb,suffix with
                        | Some n,"" -> Some (Rrx n carryNum,carryNum)
                        | Some n,"S" -> Some (Rrx n carryNum,selectLSB n)
                        | _ -> None               
                | false -> None

            match instrRoot with 
            | TST | TEQ ->
                match evalInstruction with 
                | Some (num,c) ->
                    match updateNZC cpuData.Fl num c with 
                    | Ok flgs -> Ok {cpuData with Fl = flgs}
                    | _ -> Error "failed when updating flags in TST or TEQ"
                | _ -> Error "Could not evaluate instruction"
            | _ -> 
                match evalInstruction,opa with
                | Some (num,c),Some reg ->
                    let regsUpdate = Map.add reg num cpuData.Regs
                    match suffix with
                    | "S" -> 
                        match updateNZC cpuData.Fl num c with
                        | Ok flgs -> Ok {cpuData with Regs = regsUpdate ; Fl = flgs}
                        | _ -> Error "failed when updating flags in TST or TEQ" 
                    | "" -> Ok {cpuData with Regs = regsUpdate}
                    | _ -> Error ""
                | _ -> Error ""

        | _ -> Error ""