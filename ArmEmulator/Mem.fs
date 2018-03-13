//Unresolved Comments from Xav
//Line 175 let ADRSpec = {...
// Need to put ADR into the MEM class and work out how
// to give it a different spec while being part of the 
// same class
//Line 342 let labelSpec = {
// Need to put Label into MISC instruction class

//COMMENTS FOR XAV
//  - The bit in parseMemIns with the long regex (Line 134). Is
//    there because STR and LDR can be implemented in 7
//    different ways. (See my READ_ME for details). So the
//    best way I could think of to get all the information
//    out was a big regex. If you have a better alternative
//    I am happy to change it, I just thought it was the
//    best way of doing it at a time.
//  - instTypeTmp Line 371, Not sure what you mean here
//  - Line 766, if (x <> y) then... I'm stuggling to 
//    implement this as a match
//  - Line 265, Error "evalExpression... needs to be one
//    long line so that I can test it in my unit tests. If
//    I put it on seperate lines it makes matching it in 
//    tests a nightmate.
//Mem
module Mem

    open CommonData
    open CommonLex
    open System
    open System.Text.RegularExpressions
    open BitArithmetic





//----------MEMORY INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type MemInstrType = LDR | STR 


    ///The Record type which encapsulates all the 
    /// information needed to execute an LDR or STR 
    /// instruction
    type MemInstr =
        {
            InstructionType: Result<MemInstrType,String>;
            DestSourceReg: Result<RName,String>;
            AddressReg: Result<RName,String>;
            BytesNotWords: Result<bool,String>;
            IncrementValue: int;
            PreIndexRb: bool;
            PostIndexRb: bool;
            ExtraAddressReg: RName option;
            ShiftExtraRegBy: int option;
        }

    /// parse error (dummy, but will do)
    type ErrInstr = string

    /// sample specification for set of instructions
    /// very incomplete!
    let memSpec = {
        InstrC = MEM
        Roots = ["LDR";"STR"]
        Suffixes = [""; "B"]
    }


    ///Match the pattern using a cached compiled Regex
    let (|Match|_|) pattern input =
        if isNull input then None
        else
            let m = Regex.Match(input, pattern, RegexOptions.Compiled)
            if m.Success then Some [for x in m.Groups -> x]
            else None
    ///Match a Regex pattern multiple times
    let (|Matches|_|) pattern input =
        if isNull input then None
        else
            let m = Regex.Matches(input, pattern, RegexOptions.Compiled)
            if m.Count > 0 then Some ([ for x in m -> x.Value])
            else None

    ///A record to return all the data from the regexs
    type FromOps = {Ra:Result<RName,String>; Rb: Result<RName,String>; IncrVal: int;
                        Post: bool; Rc: RName option;
                        Shift: int option}

    let regNamesTryFindMonad (item: Group) =
        match (regNames.TryFind item.Value) with
        | None   -> Error (sprintf "parseAdrIns: Destination 
                    register (%A) identified but was
                    not present in regNames" item)
        | Some x -> Ok x
                
    let resultDotBindTwoInp operator u v = 
        match (u,v) with
        | (Error x, Error y) -> if x <> y then Error (x+"\n"+y)
                                else Error x
        | (Error x, _) -> Error x
        | (_, Error y) -> Error y
        | (Ok x, Ok y) -> Ok (operator x y)

    let selectFirst x _ = x

    let (|GreaterThanZero|) x = if (x>=0) then GreaterThanZero
    let isPos (x: Group) (y: Group) = match (x.Value|>int) with 
                                      | GreaterThanZero -> (Some (x.Value|>int),(Some regNames.[y.Value])) 
                                      | _ -> (None,None)

    ///Parse function for Memory instructions such as LDR and
    /// STR. Returns a record with all the information needed
    /// to execute an LDR or STR instruction.
    let parseMemIns root suffix ls =
        let instTypeTmp = 
            match root with
            | "LDR" -> Ok LDR 
            | "STR" -> Ok STR
            | _     -> Error (sprintf "parseMemIns: Unexpected root (%A)\nls: %A" root ls)
        let bytes = 
            match suffix with
            | ""  -> Ok false
            | "B" -> Ok true
            | _   -> Error (sprintf "parseMemIns: Unexpected suffix (%A)\nls: %A" suffix ls)
        let pre = ((ls.Operands).Trim()).LastIndexOf("!") = (((ls.Operands).Trim()).Length-1)
        let errorMessage1 = resultDotBindTwoInp selectFirst instTypeTmp bytes 
        let defaultRecord ra rb = {Ra= regNamesTryFindMonad ra; Rb=regNamesTryFindMonad rb; IncrVal=0; Post= false; Rc= None; Shift= None;}
        let parseOps ops =
            match ops with
            | Match @"(R[0-9]|1[0-5]) *, *\[ *(R[0-9]|1[0-5]) *] *, *#([0-9]+)" [_; rA; rB; incV] -> //Post Increment
                Ok {(defaultRecord rA rB) with IncrVal=(incV.Value|>int); Post= true;}
            | Match @"(R[0-9]|R1[0-5]) *, *\[ *(R[0-9]|R1[0-5]) *]" [_; rA; rB] -> //Base Case
                Ok (defaultRecord rA rB)
            | Match @"(R[0-9]|R1[0-5]) *, *\[ *(R[0-9]|R1[0-5]) *, *#([0-9]+) *\]" [_; rA; rB; incV] -> //Num Increment
                Ok {(defaultRecord rA rB) with IncrVal=(incV.Value|>int);}
            | Match @"(R[0-9]|R1[0-5]) *, *\[ *(R[0-9]|R1[0-5]) *, *(R[0-9]|R1[0-5]) *\]" [_; rA; rB; rC] -> //Adding Registers
                Ok {(defaultRecord rA rB) with Rc= Some regNames.[rC.Value];}
            | Match @"(R[0-9]|R1[0-5]) *, *\[ *(R[0-9]|R1[0-5]) *, *(R[0-9]|R1[0-5]) *, *LSL *#(-*[0-9]+) *\]" [_; rA; rB; rC0; shft0] -> //Shifting
                let shft, rC = isPos shft0 rC0
                Ok {(defaultRecord rA rB) with Rc= rC; Shift= shft;}
            | _ -> 
                Error (sprintf "ops didn't match anything\nops: %A\n" ops)

        let makeOutFromParseOps _ (x: FromOps) =
            {InstructionType= instTypeTmp;
                DestSourceReg= x.Ra; AddressReg= x.Rb;
                BytesNotWords= bytes; IncrementValue= x.IncrVal;
                PreIndexRb= pre; PostIndexRb= x.Post; 
                ExtraAddressReg= x.Rc;
                ShiftExtraRegBy= x.Shift;}
        // resultDotBindTwoInp 
        //     makeOutFromParseOps (parseOps ((ls.Operands).Trim())) errorMessage1 
        parseOps ((ls.Operands).Trim())
        |> resultDotBindTwoInp makeOutFromParseOps errorMessage1
        




//----------ADR INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type ADRInstrType = ADRm
    //The m is needed to it doesn't interfere with ADR of
    // type InstrClass in Common Lex


    ///A record which encapsulates all the necessary 
    /// information for executing an ADR instruction
    type ADRInstr =
        {
            InstructionType: Result<ADRInstrType,String>;
            DestReg: Result<RName,String>;
            SecondOp: Result<uint32,String>;
        }

    ///A spec for the ADR Class of instrucions
    let ADRSpec = {
        InstrC = ADR
        Roots = ["ADR"]
        Suffixes = [""]
    }



    type Literal = {Base: uint32; R: int} // best practice, see later

    let checkLiteralMonad (l:uint32) =
        let rotate (k:uint32) n = (k >>> n) ||| (k <<< 32 - n),n
        let literal = 
            [0..2..30] 
            |> List.map (rotate 0xFFu)
            |> List.tryFind (fun (mask,_) -> (mask &&& l) = l)
            |> Option.map (fun (_,n) -> { Base=(rotate l (32 - n)) |> fst; R=n/2})
        match literal with 
        | None -> Error (sprintf "Expression result (%A) cannot be made by a rotated 8 bit number" l)
        | Some _ -> Ok l



    ///Evaluates an expression involving +-* and labels
    /// which evaluate to the addresses they represent
    ///NEEDS DOING:
    /// - Add multiple bracket functionality 
    ///   Eg 2*(6+(3*4)-(6+3))*5
    /// - Add working CheckLiteral function which works for -ve's
    let evalExpression (exp0: string) (symTab: SymbolTable) (labels: bool) =
        let trimmedList op (list0: string list) = 
            match (list0.[0],List.last list0) with
            | ("","") -> 
                        match op with
                        | "-" -> List.append ["0"] list0.[1..(List.length list0)-2]
                        | _ -> list0.[1..(List.length list0)-2]
            | ("",_) -> 
                       match op with 
                       | "-" -> List.append ["0"] list0.[1..(List.length list0)-1]
                       | _ -> list0.[1..(List.length list0)-1]
            | (_, "") -> list0.[0..(List.length list0)-2]
            | (_,_) -> list0
        let rec evalExpression' (exp: string) = 
            if String.exists (fun c -> (c ='(')||(c =')')) exp then
                let mapFunction (x: string) = 
                    if (String.exists (fun c -> (c ='(')||(c =')')) x) then 
                        match ((evalExpression' x.[1..(x.Length-2)])) with
                        | (Ok y)  -> Ok (y|>string)
                        | Error m -> Error m
                    else (Ok x)                   
                match exp with 
                | Matches @"((\([^)]*\)*)|[^()]*)" tl -> 
                                                        let bracketsEvaled = 
                                                            tl
                                                            |> List.map (mapFunction)
                                                            |> List.reduce (resultDotBindTwoInp (+))
                                                        match bracketsEvaled with
                                                        | (Ok x)  -> evalExpression' x
                                                        | Error m -> Error m
                | _ -> Error (sprintf "evalExpression: Brackets were present, but exp (%A) did not match with brakcets regex" exp)
            elif String.exists (fun c -> (c ='+')) exp then
                exp.Split('+') 
                |> Seq.toList
                |> trimmedList "+" 
                |> List.map (evalExpression')
                |> List.reduce (resultDotBindTwoInp (+))
            elif String.exists (fun c -> (c ='-')) exp then
                exp.Split('-') 
                |> Seq.toList
                |> trimmedList "-"
                |> List.map (evalExpression')
                |> List.reduce (resultDotBindTwoInp (-))
            elif String.exists (fun c -> (c ='*')) exp then
                exp.Split('*') 
                |> Seq.toList
                |> trimmedList "*"
                |> List.map (evalExpression')
                |> List.reduce (resultDotBindTwoInp (*))
            else 
                let numberOrLabel (item: string) =
                    let symTabTryFindMonad item =
                        match symTab.TryFind item with
                        | None -> Error (sprintf "evalExpression: Couldn't find label (%A)
                                  in the Symbol Table" item)
                        | Some x -> x |> string |> uint32 |> Ok
                    match (labels, (System.UInt32.TryParse item)) with
                    | (_, (true, num)) -> Ok num
                    | (true, (false, _)) -> symTabTryFindMonad item
                    | (false, (false, _)) -> Error "evalExpression-numberOrLabel: Attempting to parse label when labels are not allowed, ie for Fill"
                match (exp.Trim()) with 
                | Match @"(0x[0-9]+)" [_; ex] -> ex.Value |> uint32 |> Ok //Matching a hex number, Eg 0x5
                | Match @"(&[0-9]+)" [_; ex]  -> ("0x"+(ex.Value).[1..(ex.Length-1)]) |> uint32 |> Ok //Matching a hex number, Eg &5
                | Match @"(0b[0-1]+)" [_; ex] -> ex.Value |> uint32 |> Ok //Matching binary number, Eg 0b11
                | Match @"(\w+)" [_; _]     -> numberOrLabel (exp.Trim()) //Matching decimal numbers and labels
                | _ -> Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)"
        evalExpression' exp0



    
    ///Parses and ADR instruction and returns a result of
    /// a record which contains all the information needed
    /// to execute an ADR instruction.
    let parseAdrIns root ls =
        let adrInstrTypeTmp =
            match root with
            | "ADR" -> Ok ADRm
            | _     -> Error (sprintf "parseAdrIns: root passed to
                       function not 'ADR' (%A)" root)
        let rD =    
            match ((ls.Operands).Trim()) with
            | Match @"(R1[0-5]|R[0-9])" [_; rA] -> //Indentifying Destination Register
                regNamesTryFindMonad rA
            | _ ->
                Error (sprintf "parseAdrIns: No destination register identified in parseAdrIns\nls.Operands: %A" (ls.Operands))
        let labelExpVal = 
            match ls.SymTab with
            | None   -> Error "parseAdrIns: ls.SymTab = None"
            | Some x -> 
                       match ((ls.Operands).Trim()) with
                       | Match @"(R[0-9]|1[0-5]) *, *(.*)" [_; _; expression] -> 
                          evalExpression expression.Value x true
                       | _ -> Error (sprintf "parseAdrIns: Line Data in incorrect form\nls.Operands: %A" ls.Operands)
        let makeOutFrom x _ =
            {InstructionType= adrInstrTypeTmp;
                DestReg= rD;
                SecondOp= Ok x;}
        let errorMessage1 = resultDotBindTwoInp selectFirst adrInstrTypeTmp rD  
        resultDotBindTwoInp makeOutFrom (labelExpVal) errorMessage1 








//----------MEMORY INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type LabelInstrType = EQU | FILL | DCD
   //EquExpr is either:
   // a register relative address
   // a PC relative address
   // an absolute address
   // or a 32 bit integer
    type EquExpr = uint32 
    type ValueList = string list
    type LabelL = Result<string option,string>



    ///A record that contains all the information needed
    /// to execute a label instruction (EQU, FILL of DCD)
    type LabelInstr =
        {
            InstructionType: Result<LabelInstrType,String>;
            Name: LabelL;
            //The value to assign to the label
            EQUExpr: Result<uint32,String> option;
            //What to fill the memory with
            DCDValueList: Result<ValueList,string> option; 
            //The number of memory cells to initialise as 0
            FillN: Result<uint32,String> option;
        }

    ///Specification for label instructions
    let labelSpec = {
        InstrC = LABEL
        Roots = ["EQU";"FILL";"DCD"]
        Suffixes = [""]
    }


    /// map of all possible opcodes recognised
    let opCodesADR = opCodeExpand ADRSpec
    let opCodesMem = opCodeExpand memSpec
    let opCodesLabel = opCodeExpand labelSpec
            
    let checkPosAndDivFour (uint32Res: Result<uint32,string>) =
        match uint32Res with
        | Error m -> Error m
        | Ok x -> if (((x|>int) % 4 =0)&&((x|>int)>=0)) then Ok x
                  else Error (sprintf "parseLabelIns: Fill expression (%A) <0 or not divisible by four" x)

    ///Parse function for Label based instructions such as EQU
    /// FILL and DCD. Returns a record with all the information
    /// needed to execute an LDR or STR instruction.
    let parseLabelIns root ls =
        let evalExprHandler ops symT labels =
            match (labels, symT) with 
            | (true, None) -> Error (sprintf "parseLabelIns: ls.SymTab = None
                              \nroot: %A\nls: %A" root ls)
            | (false, None) -> (evalExpression ops ([] |> Map.ofList) false)
            | (_, Some x) -> (evalExpression ops x labels)
        let instTypeTmp = 
            match root with
            | "EQU"  -> Ok EQU 
            | "FILL" -> Ok FILL
            | "DCD"  -> Ok DCD
            | _      -> Error (sprintf "parseLabelIns: root (%A) was 
                        not EQU, FILL or DCD" root) 
        let (fillN, valList, equExp) =
            match instTypeTmp with
            | Ok EQU  -> let equExp1 = evalExprHandler ls.Operands (ls.SymTab) true
                         (None, None, (Some equExp1))  
            | Ok FILL -> let fillN1 = evalExprHandler ls.Operands (ls.SymTab) false
                                      |> checkPosAndDivFour
                         (Some fillN1, None, None)
            | Ok DCD  -> let valList = (ls.Operands).Split(',') 
                                     |> Array.map (fun s-> s.Trim()) 
                                     |> Seq.toList
                         let valListRet =
                            let symTab = [] |> Map.ofList
                            let checkAllLiterals = 
                                let checkLiteral x =
                                    match (evalExpression x symTab true) with
                                    | Ok _ -> 0
                                    | Error _ -> 1
                                List.map (checkLiteral) valList
                                |> List.reduce (+)
                            match checkAllLiterals with 
                            | 0 -> Ok valList
                            | _ -> Error "parseLabelIns: Input to DCD function not valid (No input etc)"
                         (None, Some valListRet, None)
            | _       -> (None, None, None)                   
        let nameOut = 
            match ls.Label with
            | None -> if ((instTypeTmp = Ok EQU)||(instTypeTmp = Ok DCD))
                      then Error (sprintf "parseLabelIns: EQU and DCD 
                           instructions (%A) must have a label\nls: %A" root ls)
                      else Ok None
            | Some _ -> Ok ls.Label
        let makeLabelOut (nO: string option) _ =
            {InstructionType = instTypeTmp; Name = Ok nO; 
                EQUExpr = equExp; DCDValueList = valList; 
                FillN = fillN}
        let realOut =
            match (instTypeTmp, nameOut) with
            | (Error x, Error y) -> Error (x+"\n"+y)
            | (Error x, _) -> Error x
            | (_, Error y) -> Error y
            | (Ok _, Ok _) ->  Ok {InstructionType = instTypeTmp; Name = nameOut; 
                                EQUExpr = equExp; DCDValueList = valList; 
                                FillN = fillN}
        realOut



    type ReturnInstr = 
        | LabelO of Result<LabelInstr,string> 
        | MemO of Result<MemInstr,string> 
        | AdrO of Result<ADRInstr,string>


    
    ///Finds the amount of memory that the instruction 
    /// will use for the PSize field of the record returned
    /// by parse.
    let findDcdPSize pInstr =
        let removeRes x =
            let removeOpt y =
                match y with 
                | Some z -> 
                           match z with
                           | Ok u -> ((List.length u)*4)|>uint32
                           | _ -> 0u
                | None -> 0u 
            match x with
            | Ok y -> removeOpt y.DCDValueList
            | _ ->  0u
        match pInstr with     
        | LabelO x -> removeRes x
        | _ -> 0u


    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse (ls: LineData) = //: Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =

            let (WA la) = ls.LoadAddr // address this instruction is loaded into memory
            // this does the real work of parsing
            // dummy return for now
            
            let pInstrTmp =
                match instrC with
                | LABEL -> (Ok (LabelO (parseLabelIns root ls)))
                | MEM   -> (Ok (MemO (parseMemIns root suffix ls)))
                | ADR   -> (Ok (AdrO (parseAdrIns root ls)))
                | _ -> Error "Instruction class not supported."


            match pInstrTmp with
            | Ok x -> 
                let pSizeTmp =
                    match root with
                    | "EQU" -> 0u
                    | "DCD" -> findDcdPSize x
                    | _     -> 4u
                Ok { 
                    // Normal (non-error) return from result monad
                    // This is the instruction determined from opcode, suffix and parsing
                    // the operands. Not done in the sample.
                    // Note the record type returned must be written by the module author.
                    // PInstr={InstructionType= (); DestSourceReg= (); SecondOp= ();}; 
                    PInstr = x


                    // This is normally the line label as contained in
                    // ls together with the label's value which is normally
                    // ls.LoadAddr. Some type conversion is needed since the
                    // label value is a number and not necessarily a word address
                    // it does not have to be div by 4, though it usually is
                    PLabel = ls.Label |> Option.map (fun lab -> lab, la) ; 


                    // this is the number of bytes taken by the instruction
                    // word loaded into memory. For arm instructions it is always 4 bytes. 
                    // For data definition DCD etc it is variable.
                    //  For EQU (which does not affect memory) it is 0
                    PSize = pSizeTmp


                    // the instruction condition is detected in the opcode and opCodeExpand                 
                    // has already calculated condition already in the opcode map.
                    // this part never changes
                    PCond = pCond 
                    }
            | Error s -> Error s     
        [opCodesLabel; opCodesMem; opCodesADR]
        |> List.choose (Map.tryFind ls.OpCode)
        |> function 
            // should only be a single result, if so, parse it
            | [instr] -> Some (parse' instr)
            | _ -> None








    ///I have already checked regName and val0 in 
    /// resultDotBindTwoInp so I don't need to again.
    /// Will update the register regName with val0
    let updateRegister (dP: DataPath<'INS>) (regName: RName) (val0:uint32) = (dP.Regs).Add(regName,val0)
    let updateDataPathRegs (dP: DataPath<'INS>) x = {dP with Regs = x}

    ///Returns a value from memory as a result. Called by
    /// execLDR
    let accessMemoryLocation (dP: DataPath<'INS>) (wordAddress: WAddr) =
        match (dP.MM).TryFind wordAddress with
        | Some x -> 
                   match x with
                   | DataLoc y -> Ok y
                   | _ -> Error (sprintf "execLDR-accessMemoryLocation: MemLoc value (%A) 
                            at %A was not of type DataLoc" x wordAddress)
        | _ -> Error (sprintf "execLDR-accessMemoryLocation: %A was not present in memory" wordAddress)

    ///Adds a value to memory. Called by execSTR
    let updatedMachineMemory (dP: DataPath<'INS>) (wordAddress: WAddr) val0 =
        match System.UInt32.TryParse val0 with
        | (true, x) -> (Ok ((dP.MM).Add(wordAddress, DataLoc x)))
        | (false,_) -> Error (sprintf "execSTR-updateMemoryLocation: val0 (%A) could not be converted to a uint32" val0)

    ///Returns end value of Ra, Rb. Called by execSTR and 
    /// LRD exec
    let interpretingRecord (dP: DataPath<'INS>) (inputRecord: MemInstr) =
        ///Finds Original Ra and Rb values
        let getOrigVal inputRecord (dP: DataPath<'INS>) = 
            let getOriginalRegisterVals (Ra: RName) (Rb: RName) =
                (Map.find Ra dP.Regs, Map.find Rb dP.Regs)
            resultDotBindTwoInp getOriginalRegisterVals inputRecord.DestSourceReg inputRecord.AddressReg

        let makeBytes (bytes: bool) (x,y) =
            match bytes with
            | false -> (x,y)
            | true -> (x&&&0xFFu,y&&&0xFFu)

        let incrementRbValue (inputRecord: MemInstr) (dP: DataPath<'INS>) =                
            let powerF (baseF: uint32) (expF: uint32) = 
                    ((baseF|>float)**(expF|>float))|>uint32
            match inputRecord.ExtraAddressReg with
            | None -> Ok (inputRecord.IncrementValue |> uint32)
            | Some x -> 
                       let rC = Map.find x dP.Regs
                       match inputRecord.ShiftExtraRegBy with
                       | None -> Ok (rC)
                       | Some y -> if y >= 0 then Ok (rC*(powerF 2u (y|>uint32)))
                                    else Ok 0u

        ///Returns the Ra and Rb values
        let preOrPost pre post (inputRecord: MemInstr) (dP: DataPath<'INS>) x (rABTup: (uint32 * uint32)) =
            let rA = fst rABTup
            let rB = snd rABTup
            match inputRecord.InstructionType with
            | Ok LDR -> 
                       match (pre,post) with
                       | (true,true) -> Error "execLDR-interpretingRecord: pre and post can't both be true"
                       | (true, _) -> Ok (accessMemoryLocation dP (WA (rB+x)), rB+x)
                       | (_, true) -> Ok (accessMemoryLocation dP (WA (rB)), rB+x)
                       | (false,false) -> Ok (accessMemoryLocation dP (WA (rB+x)), rB)
            | Ok STR -> 
                       match (pre,post) with
                       | (true,true) -> Error "execLDR-interpretingRecord: pre and post can't both be true"
                       | (true, _) -> Ok (Ok rA, rB+x)
                       | (_, true) -> Ok (Ok rA, rB+x)
                       | (false,false) -> Ok (Ok rA, rB) 
            | Error m -> Error (sprintf "interpretingRecord-preOrPost: Given instruction type not STR of LDR\n%A" m)                    

        ///Removes unnecessary levels of Result
        let changeType (v: Result<Result<(Result<uint32,string> * uint32),string>,string>) = 
            match v with
            | Ok x ->   
                     match x with
                     | Ok y -> 
                              match y with
                              | (Ok z, u) -> Ok (z,u)
                              | (_, _) -> Error "execLDR-interpretingRecord: Error accesing memory location"
                     | Error m -> Error m
            | Error m -> Error m

        let changedToBytes (inputRecord: MemInstr) (a: Result<(uint32 * uint32),string>) =
            resultDotBindTwoInp makeBytes inputRecord.BytesNotWords a 


        changedToBytes inputRecord 
            (changeType 
                (resultDotBindTwoInp 
                    (preOrPost inputRecord.PreIndexRb inputRecord.PostIndexRb inputRecord dP) 
                    (incrementRbValue inputRecord dP) 
                    (getOrigVal inputRecord dP)))


    let makeTuple a b = (a,b)

    let abstractResults (x: Result<(Result<DataPath<'INS>,string> * Result<Map<string,uint32>,string>),string>) =
        match x with
        | Error m -> Error m
        | Ok (a, b) -> resultDotBindTwoInp makeTuple a b

    
    ///Will just update the DataPath, SymbolTable is untouched
    let execLDR (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: MemInstr) = 
        
        let fstRecTuple x =
            match x with 
            | Ok (a,_) -> Ok a
            | Error m -> Error m 
        let sndRecTuple x =
            match x with 
            | Ok (_,b) -> Ok b
            | Error m -> Error m 
        let regsMapF (dPF: Result<DataPath<'INS>,string>) reg x = 
            match dPF with 
            | Ok y -> (resultDotBindTwoInp (updateRegister y) reg x)
            | Error m -> Error m
        let interpretedRecord = interpretingRecord dP inputRecord

        let updatedDP1 = 
            interpretedRecord
            |> fstRecTuple
            |> regsMapF (Ok dP) (inputRecord.DestSourceReg)
            |> resultDotBindTwoInp updateDataPathRegs (Ok dP) 
        let updatedDP2 = 
            interpretedRecord
            |> sndRecTuple 
            |> regsMapF updatedDP1 (inputRecord.AddressReg)
            |> resultDotBindTwoInp updateDataPathRegs updatedDP1

        abstractResults (Ok (updatedDP2, Ok symbolTab))



    ///Will just update the DataPath, SymbolTable is untouched
    let execSTR (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: MemInstr) = 

        let interpretedRecord = interpretingRecord dP inputRecord

        let updatedSTRMachineMem (tuple: Result<(uint32 * uint32),string>) =
            match tuple with
            | Ok (a,b) -> (updatedMachineMemory dP (WA b) (a|>string))
            | Error m -> Error m

        let makeDataPath x = {dP with MM = x}
        abstractResults (Ok (Result.map makeDataPath (updatedSTRMachineMem interpretedRecord), Ok symbolTab))



    ///Updates the symbol table
    ///Used for execDCD, execEQU and execFILL
    let updateSymbolTable (symbolTab: SymbolTable) (inputRecord: LabelInstr) field = 
        let getLabel (rec1: LabelInstr) = 
            match rec1.Name with
            | Ok y -> 
                     match y with
                     | Some z -> Ok z
                     | None -> Error (sprintf "updateSymbolTable: (Ok (%A).Name) = None" rec1)
            | Error n -> Error (n+"\n"+(sprintf "updateSymbolTable: (%A).Name = Error" rec1))
        let getValue field inputRecord =
            match inputRecord.InstructionType with
            | Ok EQU -> 
                       match field with
                       | Some y -> y
                       | None -> Error (sprintf "updateSymbolTable-getValue: (Ok (%A).EQUExpr) = None" field)
            | Ok FILL -> 
                        match field with
                        | Some y ->
                                   match y with
                                   | Ok _ -> Ok 0u
                                   | Error m -> Error m
                        | None -> Error (sprintf "updateSymbolTable-getValue: (Ok (%A).FillN) = None" field)
            | Ok DCD  ->
                        match field with
                        | Some y -> y
                        | None -> Error (sprintf "updateSymbolTable-getValue: (Ok (%A).FillN) = None" field)
            | _ -> Error (sprintf "updateSymbolTable-getValue: InstructionType (%A) not EQU, DCD or FILL" inputRecord.InstructionType)
        let addToSymTab x y = symbolTab.Add(x,y)
        resultDotBindTwoInp addToSymTab (getLabel inputRecord) (getValue field inputRecord)

    ///Updates the Memory in the DataPath (Adds something
    /// to memory). Called by execDCD and execFILL
    let updateMemoryDataPath (inputRecord: LabelInstr) (dP: DataPath<'INS>) =
        let dataValList = 
            let fillNf = 
                match inputRecord.FillN with
                | Some x -> 
                           match x with 
                           | Ok y -> y|>int
                           | _ -> 0
                | _ -> 0
            match inputRecord.InstructionType with
            | Ok x ->   
                    match x with
                    | DCD ->    
                            match inputRecord.DCDValueList with
                            | Some z -> 
                                       match z with
                                       | Ok u -> Ok (List.map (fun y -> (y|>int|>uint32)) u)
                                       | Error m -> Error m
                            | None -> Error "updateMemoryDataPath-dataValList: DCDValueList = None"
                    | FILL -> 
                             match inputRecord.FillN with
                             | Some y -> 
                                        match y with 
                                        | Ok _ -> Ok (List.replicate fillNf 0u)
                                        | Error m -> Error m
                             | None -> Error "updateMemoryDataPath-dataValList: FillN = None" 
                    | EQU -> 
                            match inputRecord.EQUExpr with
                            | Some y -> 
                                       match y with 
                                       | Ok _ -> Ok []
                                       | Error m -> Error m
                            | None -> Error "updateMemoryDataPath-dataValList: EQUExpr = None" 
            | Error m -> Error m

        let getAddrList lst length =
            let rec getAddrList' list len =
                match len with 
                | 0 -> list
                | 1 -> list
                | _ ->  getAddrList' (List.append list [((List.last list)+4u)]) (len-1)
            match length with
            | Ok x -> Ok (getAddrList' lst x)
            | Error m -> Error m

        let findMaxAddr (dP: DataPath<'INS>) = 
            let waToUint32 (k,_) =
                match k with
                | WA y -> y
            dP.MM
            |> Map.toSeq
            |> Seq.map waToUint32
            |> Seq.max 

        let findAddrs (dP: DataPath<'INS>) :Result<uint32 list, string>=
            if ((dP.MM).IsEmpty) then
                getAddrList [0x100u] (Result.map (List.length) dataValList)
            else 
                getAddrList [findMaxAddr dP] (Result.map (List.length) dataValList)

        let rec updateMachineMemory' (addrListRes: Result<uint32 list,string>) (dPMM: MachineMemory<'INS>) (dataValListRes') =
            match (addrListRes, dataValListRes') with
            | (Error x, Error y) -> if (x <> y) then Error (x+"\n"+y)
                                    else Error x
            | (Error x, _) -> Error x
            | (_, Error y) -> Error y
            | (Ok addrList, Ok (dataValList': uint32 list)) ->  match dataValList' with
                                                                | [] -> Ok dPMM
                                                                | _ ->  
                                                                      let remainingAddrList = Ok addrList.[1..(List.length addrList)-1]
                                                                      let remainingValueList = Ok dataValList'.[1..(List.length dataValList')-1]
                                                                      let updatedMachineMem = dPMM.Add(WA (addrList.[0]+4u), DataLoc dataValList'.[0])
                                                                      updateMachineMemory' remainingAddrList updatedMachineMem remainingValueList
        match (updateMachineMemory' (findAddrs dP) dP.MM dataValList) with
        | Ok x -> Ok {Fl = dP.Fl; Regs = dP.Regs; MM = x}
        | Error m -> Error m
        

    ///Taken out for testing - (Could not reference it in
    /// memInstructionTests when it was a subfunction of
    /// execDCD)
    let removeOptionD (x:Result<ValueList,string> option) =   
        let makeType (x:ValueList)  =
            let y = (x.[0])|>uint32
            let temp =
                match Ok y with
                       | Ok y -> Ok y
                       | _ -> Error "should never happen"
            Some temp
        let removeResult x =
            match x with 
            | Ok y -> makeType y
            | Error m -> Some (Error m)

        match x with
        | Some y -> removeResult y
        | _ -> None

    ///Executes the DCD instruction, taking in the symbol table,
    /// DataPath and labelInstr record and outputing updated 
    /// Symbol tables and DataPaths
    let execDCD (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: LabelInstr) = 
        removeOptionD (inputRecord.DCDValueList)
        |> updateSymbolTable symbolTab inputRecord
        |> fun b -> ((updateMemoryDataPath inputRecord dP), b)
        |> Ok
        |> abstractResults

    ///Takes in the current state of the program in the form
    /// of the SymbolTable and DataPath, as well as the 
    /// information record labelInstr and updates the current
    /// state of the program by executing an EQU instruction
    ///Updates SymbolTable only
    let execEQU (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: LabelInstr) = 
        inputRecord.EQUExpr
        |> updateSymbolTable symbolTab inputRecord
        |> fun b -> (Ok dP, b)
        |> Ok
        |> abstractResults 
    
    ///Executes a Fill instruction, updates Symbol Table and DataPath
    let execFILL (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: LabelInstr) = 
        inputRecord.FillN
        |> updateSymbolTable symbolTab inputRecord 
        |> fun b -> (updateMemoryDataPath inputRecord dP, b)
        |> Ok
        |> abstractResults 




    let execADR (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: ADRInstr) = 
        let updateRegMap (dP: DataPath<'INS>) (inputRecord: ADRInstr) = 
            resultDotBindTwoInp (updateRegister dP) inputRecord.DestReg inputRecord.SecondOp 
        inputRecord
        |> updateRegMap dP 
        |> Result.map (updateDataPathRegs dP) 
        |> fun a -> (a, Ok symbolTab)
        |> Ok
        |> abstractResults 

        

    let execInstr (dP: DataPath<'INS>) (symbolTab: SymbolTable) (parseInputRecord: Parse<ReturnInstr>) =
        let inputRecord = parseInputRecord.PInstr
        let labelInstructionsHandler (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: LabelInstr) =
            let matchLabIns x =
                match x with
                | EQU -> (execEQU symbolTab dP inputRecord)
                | DCD -> (execDCD symbolTab dP inputRecord)
                | FILL -> (execFILL symbolTab dP inputRecord)
            Result.bind matchLabIns inputRecord.InstructionType
        let memInstructionsHandler (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: MemInstr) =
            let matchMemIns x =
                match x with
                | LDR -> (execLDR symbolTab dP inputRecord)
                | STR -> (execSTR symbolTab dP inputRecord)
            Result.bind matchMemIns inputRecord.InstructionType

        match inputRecord with
        | LabelO x -> Result.bind (labelInstructionsHandler symbolTab dP) x
        | AdrO x -> Result.bind (execADR symbolTab dP) x
        | MemO x -> Result.bind (memInstructionsHandler symbolTab dP) x




    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse
