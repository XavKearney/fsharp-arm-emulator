//MemInstructions
module MemInstructions

    open CommonData
    open CommonLex
    open System
    open System.Text.RegularExpressions






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


    /// Parse Active Pattern used by top-level code
    // let (|IMatch|_|) = parse


    ///Match the pattern using a cached compiled Regex
    let (|Match|_|) pattern input =
        if input = null then None
        else
            let m = Regex.Match(input, pattern, RegexOptions.Compiled)
            if m.Success then Some [for x in m.Groups -> x]
            else None
    ///Match a Regex pattern multiple times
    let (|Matches|_|) pattern input =
        if input = null then None
        else
            let m = Regex.Matches(input, pattern, RegexOptions.Compiled)
            if m.Count > 0 then Some ([ for x in m -> x.Value])
            else None


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
    

    let (|GreaterThanZero|ErrorGZ|) x = if (x>=0) then GreaterThanZero else ErrorGZ
    let isPos (x: Group) (y: Group) = match (x.Value|>int) with GreaterThanZero -> (Some (x.Value|>int),(Some regNames.[y.Value])) | ErrorGZ -> (None,None)

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
        let parseOps ops =
            match ops with
            | Match @"(R[0-9]|1[0-5]) *, *\[ *(R[0-9]|1[0-5]) *] *, *#([0-9]+)" [_; rA; rB; incV] -> //Post Increment
                Ok ({Ra=regNamesTryFindMonad rA; Rb=regNamesTryFindMonad rB; IncrVal=(incV.Value|>int); Post= true; Rc= None; Shift= None;})
            | Match @"(R[0-9]|R1[0-5]) *, *\[ *(R[0-9]|R1[0-5]) *]" [_; rA; rB] -> //Base Case
                Ok ({Ra=regNamesTryFindMonad rA; Rb=regNamesTryFindMonad rB; IncrVal=0; Post= false; Rc= None; Shift= None;})
            | Match @"(R[0-9]|R1[0-5]) *, *\[ *(R[0-9]|R1[0-5]) *, *#([0-9]+) *\]" [_; rA; rB; incV] -> //Num Increment
                Ok ({Ra=regNamesTryFindMonad rA; Rb=regNamesTryFindMonad rB; IncrVal=(incV.Value|>int); Post= false; Rc= None; Shift= None;})
            | Match @"(R[0-9]|R1[0-5]) *, *\[ *(R[0-9]|R1[0-5]) *, *(R[0-9]|R1[0-5]) *\]" [_; rA; rB; rC] -> //Adding Registers
                Ok ({Ra=regNamesTryFindMonad rA; Rb=regNamesTryFindMonad rB; IncrVal=0; Post= false; Rc= Some regNames.[rC.Value]; Shift= None;})
            | Match @"(R[0-9]|R1[0-5]) *, *\[ *(R[0-9]|R1[0-5]) *, *(R[0-9]|R1[0-5]) *, *LSL *#(-*[0-9]+) *\]" [_; rA; rB; rC; shft] -> //Shifting
                let Shft, RC = isPos shft rC
                Ok ({Ra=regNamesTryFindMonad rA; Rb=regNamesTryFindMonad rB; IncrVal=0; Post= false; Rc= RC; Shift= Shft;})
            | _ -> 
                Error (sprintf "ops didn't match anything\nops: %A\n" ops)

        let makeOutFromParseOps (x: FromOps) _ =
            {InstructionType= instTypeTmp;
                DestSourceReg= x.Ra; AddressReg= x.Rb;
                BytesNotWords= bytes; IncrementValue= x.IncrVal;
                PreIndexRb= pre; PostIndexRb= x.Post; 
                ExtraAddressReg= x.Rc;
                ShiftExtraRegBy= x.Shift;}
        resultDotBindTwoInp makeOutFromParseOps (parseOps ((ls.Operands).Trim())) errorMessage1 

        












//----------ADR INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type ADRInstrType = ADRm 


    /// instruction (dummy: must change)
    type ADRInstr =
        {
            InstructionType: Result<ADRInstrType,String>;
            DestReg: Result<RName,String>;
            SecondOp: Result<uint32,String>;
        }

    /// parse error (dummy, but will do)

    /// sample specification for set of instructions
    /// very incomplete!
    let ADRSpec = {
        InstrC = ADR
        Roots = ["ADR"]
        Suffixes = [""]
    }

    /// map of all possible opcodes recognised
    // let opCodesADR = opCodeExpand ADRSpec


    /// Parse Active Pattern used by top-level code
    // let (|IMatch|_|) = parse


    // let (|BitRotatable|_|) num =
    //     match num with
    //     | None -> None
    //     | Error(x) -> Error(x)
    //     | _    ->   

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
        | Some x -> Ok l
    let test1 = checkLiteralMonad 2u
    let test2 = checkLiteralMonad 4u
    let test3 = checkLiteralMonad 255u
    let test4 = checkLiteralMonad 511u
    let test5 = checkLiteralMonad 510u
    let test6 = checkLiteralMonad (-1|>uint32)
    let test7 = checkLiteralMonad (0xCCCCCCCCu)
    let test8 = checkLiteralMonad (-257|>uint32)



    ///Evaluates an expression involving +-* and labels
    /// which evaluate to the addresses they represent
    ///NEEDS DOING:
    /// - Labels with numbers in them maybe make number 
    ///   match statement (^[0-9]|(([^a-z]| )[0-9]+([^a-z]| |$)))
    /// - Add multiple bracket functionality 
    ///   Eg 2*(6+(3*4)-(6+3))*5
    /// - Add working CheckLiteral function which works for -ve's
    let evalExpression (exp0: string) (symTab: SymbolTable) =
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
            elif String.exists (fun c -> (c ='+')) exp then
                let list = exp.Split('+') |> Seq.toList
                if ((list.[0]="")&&((List.last list)="")) 
                then list.[1..(List.length list)-2]
                elif (list.[0]="")
                then list.[1..(List.length list)-1]
                elif ((List.last list)="")
                then list.[0..(List.length list)-2]
                else list //Should return an error monad above here
                |> List.map (fun x -> evalExpression' x)
                |> List.reduce (resultDotBindTwoInp (+))
            elif String.exists (fun c -> (c ='-')) exp then
                let list = exp.Split('-') |> Seq.toList
                if ((list.[0]="")&&((List.last list)="")) 
                then List.append ["0"] list.[1..(List.length list)-2]
                elif (list.[0]="")
                then List.append ["0"] list.[1..(List.length list)-1]
                elif ((List.last list)="")
                then list.[0..(List.length list)-2]
                else list //Should return an error monad above here
                |> List.map (fun x -> evalExpression' x)
                |> List.reduce (resultDotBindTwoInp (-))
            elif String.exists (fun c -> (c ='*')) exp then
                let list = exp.Split('*') |> Seq.toList
                if ((list.[0]="")&&((List.last list)="")) 
                then list.[1..(List.length list)-2]
                elif (list.[0]="")
                then list.[1..(List.length list)-1]
                elif ((List.last list)="")
                then list.[0..(List.length list)-2]
                else list //Should return an error monad above here
                |> List.map (fun x -> evalExpression' x)
                |> List.reduce (resultDotBindTwoInp (*))
            else 
                let numberOrLabel (item: string) =
                    let symTabTryFindMonad item =
                        match symTab.TryFind item with
                        | None -> Error (sprintf "evalExpression: Couldn't find label (%A)
                                  in the Symbol Table" item)
                        | Some x -> x |> string |> uint32 |> Ok
                    match (System.UInt32.TryParse item) with
                    | (true, num) -> Ok num
                    | (false, _) -> symTabTryFindMonad item
                match (exp.Trim()) with 
                | Match @"(0x[0-9]+)" [_; ex] -> ex.Value |> uint32 |> Ok //Matching a hex number, Eg 0x5
                | Match @"(&[0-9]+)" [_; ex]  -> ("0x"+(ex.Value).[1..(ex.Length-1)]) |> uint32 |> Ok //Matching a hex number, Eg &5
                | Match @"(0b[0-1]+)" [_; ex] -> ex.Value |> uint32 |> Ok //Matching binary number, Eg 0b11
                | Match @"(\w+)" [_; lab]     -> numberOrLabel (exp.Trim()) //Matching decimal numbers and labels
                | _ -> Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)"
        evalExpression' exp0



    
    ///Parses and ADR instruction and returns a result of
    /// a record which contains all the information needed
    /// to execute an ADR instruction.
    let parseAdrIns root ls =
        let ADRInstrTypeTmp =
            match root with
            | "ADR" -> Ok ADRm
            | _     -> Error (sprintf "parseAdrIns: root passed to
                       function not 'ADR' (%A)" root)
        let Rd =    
            match ((ls.Operands).Trim()) with
            | Match @"(R1[0-5]|R[0-9])" [_; rA] -> //Indentifying Destination Register
                regNamesTryFindMonad rA
            | _ ->
                Error (sprintf "parseAdrIns: No destination register identified in parseAdrIns\nls.Operands: %A" (ls.Operands))
        let labelExpVal = 
            match ls.SymTab with
            | None   -> Error "parseAdrIns: ls.SymTab = None"
            | Some x -> match ((ls.Operands).Trim()) with
                        | Match @"(R[0-9]|1[0-5]) *, *(.*)" [_; rA; expression] -> 
                            evalExpression expression.Value x
                        | _ -> Error (sprintf "parseAdrIns: Line Data in incorrect form\nls.Operands: %A" ls.Operands)
        let makeOutFrom x _ =
            {InstructionType= ADRInstrTypeTmp;
                DestReg= Rd;
                SecondOp= Ok x;}
        let errorMessage1 = resultDotBindTwoInp selectFirst ADRInstrTypeTmp Rd  
        resultDotBindTwoInp makeOutFrom (labelExpVal) errorMessage1 








//----------MEMORY INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type LabelInstrType = EQU | FILL | DCD
                                   //EquExpr is either:
                                   //a register relative address
                                   //a PC relative address
                                   //an absolute address
                                   //or a 32 bit integer
    type EquExpr = uint32 
    type ValueList = string list
    type LabelL = Result<string option,string>



    /// instruction (dummy: must change)
    type labelInstr =
        {
            InstructionType: Result<LabelInstrType,String>;
            Name: LabelL;
            EQUExpr: Result<uint32,String> option;
            DCDValueList: Result<ValueList,string> option;            //What to fill the memory with
            FillN: Result<uint32,String> option;
        }

    /// parse error (dummy, but will do)

    /// sample specification for set of instructions
    /// very incomplete!
    let labelSpec = {
        InstrC = LABEL
        Roots = ["EQU";"FILL";"DCD"]
        Suffixes = [""]
    }

    /// map of all possible opcodes recognised


    /// Parse Active Pattern used by top-level code
    // let (|IMatch|_|) = parse

    type labelMemOrADR = LabelInstrType | ADRInstrType | MemInstrType


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
            match symT with 
            | None -> Error (sprintf "parseLabelIns: ls.SymTab = None
                      \nroot: %A\nls: %A" root ls)
            | Some x -> (evalExpression ops x)
        let instTypeTmp = 
            match root with
            | "EQU"  -> Ok EQU 
            | "FILL" -> Ok FILL
            | "DCD"  -> Ok DCD
            | _      -> Error (sprintf "parseLabelIns: root (%A) was 
                        not EQU, FILL or DCD" root) 
        let (fillN, valList, equExp) =
            match instTypeTmp with
            | Ok EQU  ->   let equExp1 = evalExprHandler ls.Operands (ls.SymTab) true
                           (None, None, (Some equExp1))  
            | Ok FILL ->   let fillN1 = evalExprHandler ls.Operands (ls.SymTab) false
                                            |> checkPosAndDivFour
                           (Some fillN1, None, None)
            | Ok DCD  ->   let valList = (ls.Operands).Split(',') 
                                        |> Array.map (fun s-> s.Trim()) 
                                        |> Seq.toList
                           let valListRet =
                                let symTab:SymbolTable = ["irrelevant",256u] |> Map.ofList
                                let checkAllLiterals = 
                                    let checkLiteral x =
                                        match (evalExpression x symTab) with
                                        | Ok y -> 0
                                        | Error m -> 1
                                    List.map (checkLiteral) valList
                                    |> List.reduce (fun a b -> a+b)
                                match checkAllLiterals with 
                                | 0 -> Ok valList
                                | _ -> Error "parseLabelIns: Input to DCD function not valid (No input etc)"
                                // match valList with
                                // | [""] -> Error "parseLabelIns: No input to DCD function" 
                                // | _ -> Ok valList
                           (None, Some valListRet, None)
            | _       ->   (None, None, None)                   
        let nameOut = 
            match ls.Label with
            | None -> if ((instTypeTmp = Ok EQU)||(instTypeTmp = Ok DCD))
                      then Error (sprintf "parseLabelIns: EQU and DCD 
                           instructions (%A) must have a label\nls: %A" root ls)
                      else Ok None
            | Some _ -> Ok ls.Label
        let errorMessage1 =
            let checkRes x = 
                match x with
                | Ok y -> Ok 1
                | _ -> Error (sprintf "parseLabelIns: One of fillN, valList and equExp is an Error")
            match (fillN, valList, equExp) with
            | (Some x, None, None) -> checkRes x
            | (None, Some x, None) -> checkRes x
            | (None, None, Some x) -> checkRes x
            | _ -> Error (sprintf "parseLabelIns: should never happen, more or less than one of fillN(%A), valList(%A) and equExp(%A) are Some x" fillN valList equExp)
        let errorMessage2 = resultDotBindTwoInp selectFirst errorMessage1 instTypeTmp 
        let makeLabelOut (nO: string option) _ =
            {InstructionType = instTypeTmp; Name = Ok nO; 
                EQUExpr = equExp; DCDValueList = valList; 
                FillN = fillN}
        let out = resultDotBindTwoInp makeLabelOut nameOut errorMessage2 
        let realOut =
            match (instTypeTmp, nameOut) with
            | (Error x, Error y) -> Error (x+"\n"+y)
            | (Error x, _) -> Error x
            | (_, Error y) -> Error y
            | (Ok x, Ok y) ->  Ok {InstructionType = instTypeTmp; Name = nameOut; 
                                EQUExpr = equExp; DCDValueList = valList; 
                                FillN = fillN}
        // printfn "\n\n\nout = %A\nrealOut = %A" out realOut
        // if out <> realOut then printfn "out and realOut not equal: \nout = %A\nrealOut = %A\nroot = %A\nls = %A" out realOut root ls
        realOut



    type LabelAndMemGeneralParse = LabelO of Result<labelInstr,string> 
                                                | MemO of Result<MemInstr,string> 
                                                | AdrO of Result<ADRInstr,string>


    
    ///Finds the amount of memory that the instruction 
    /// will use for the PSize field of the record returned
    /// by parse.
    let findDcdPSize pInstr =
        let removeRes x =
            let removeOpt y =
                match y with 
                | Some z -> match z with
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
            
            let PInstrTmp =
                match instrC with
                | LABEL -> LabelO (parseLabelIns root ls)
                | MEM   -> MemO (parseMemIns root suffix ls)
                | ADR   -> AdrO (parseAdrIns root ls)
            let PSizeTmp =
                match root with
                | "EQU" -> 0u
                | "DCD" -> findDcdPSize PInstrTmp
                | _     -> 4u


            if 1=1 then           
            Ok { 
                // Normal (non-error) return from result monad
                // This is the instruction determined from opcode, suffix and parsing
                // the operands. Not done in the sample.
                // Note the record type returned must be written by the module author.
                // PInstr={InstructionType= (); DestSourceReg= (); SecondOp= ();}; 
                PInstr = PInstrTmp


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
                PSize = PSizeTmp


                // the instruction condition is detected in the opcode and opCodeExpand                 
                // has already calculated condition already in the opcode map.
                // this part never changes
                PCond = pCond 
                }
            else Error "parse: Should never happen, just setting the type"
        if (Map.tryFind ls.OpCode opCodesLabel |> Option.map parse') <> None 
        then (Map.tryFind ls.OpCode opCodesLabel |> Option.map parse')
        elif (Map.tryFind ls.OpCode opCodesMem |> Option.map parse') <> None 
        then (Map.tryFind ls.OpCode opCodesMem |> Option.map parse')
        else 
        (Map.tryFind ls.OpCode opCodesADR |> Option.map parse')



    /// Parse Active Pattern used by top-level code
    // let (|IMatch|_|) = parse





    ///I have already checked regName and val0 in 
    /// resultDotBindTwoInp so I don't need to again.
    /// Will update the register regName with val0
    let updateRegister (dP: DataPath<'INS>) (regName: RName) (val0:uint32) = (dP.Regs).Add(regName,val0)
    let updateDataPathRegs (dP: DataPath<'INS>) x = {dP with Regs = x}


    ///Will just update the DataPath, SymbolTable is untouched
    let LDRexec (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: MemInstr) = 
        let accessMemoryLocation dP (wordAddress: WAddr) =
            match (dP.MM).TryFind wordAddress with
            | Some x -> match x with
                        | DataLoc y -> Ok y
                        | _ -> Error (sprintf "STRexec-accessMemoryLocation: MemLoc value (%A) at %A was not of type DataLoc" x wordAddress)
            | _ -> Error (sprintf "STRexec-accessMemoryLocation: %A was not present in memory" wordAddress)
        

        //Returns end value of Ra, Rb
        let interpretingRecord inputRecord =
            let makeBytes (bytes: bool) (x,y) =
                match bytes with
                | false -> (x,y)
                | true -> (x&&&0xFFu,y&&&0xFFu)
            let incrementRbValue =
                let powerF (baseF: uint32) (expF: uint32) = 
                        ((baseF|>float)**(expF|>float))|>uint32
                match inputRecord.ExtraAddressReg with
                | None -> Ok (inputRecord.IncrementValue |> uint32)
                | Some x -> let Rc = Map.find x dP.Regs
                            match inputRecord.ShiftExtraRegBy with
                            | None -> Ok Rc
                            | Some y -> Ok (powerF Rc (y|>uint32))

            ///Finds Original Ra and Rb values
            let getOrigVal = 
                let getOriginalRegisterVals (Ra: RName) (Rb: RName) =
                    (Map.find Ra dP.Regs, Map.find Rb dP.Regs)
                resultDotBindTwoInp getOriginalRegisterVals inputRecord.DestSourceReg inputRecord.AddressReg
            
            ///Returns the Ra and Rb values
            let preOrPost pre post x (rABTup: (uint32 * uint32)) =
                let Ra = fst rABTup
                let Rb = snd rABTup
                match (pre,post) with
                | (true,true) -> Error "LDRexec-interpretingRecord: pre and post can't both be true"
                | (true, _) -> Ok (accessMemoryLocation dP (WA (Rb+x)), Rb+x)
                | (_, true) -> Ok (accessMemoryLocation dP (WA (Rb)), Rb+x)
                | (false,false) -> Ok (accessMemoryLocation dP (WA (Rb+x)), Rb)
            
            ///Removes unnecessary levels of Result
            let changeType (v: Result<Result<(Result<uint32,string> * uint32),string>,string>) = 
                match v with
                | Ok x ->   match x with
                            | Ok y -> match y with
                                      | (Ok z, u) -> Ok (z,u)
                                      | (_, u) -> Error "LDRexec-interpretingRecord: Error accesing memory location"
                            | Error m -> Error m
                | Error m -> Error m
            let changedToBytes (a: Result<(uint32 * uint32),string>) =
                resultDotBindTwoInp makeBytes inputRecord.BytesNotWords a 
            changedToBytes (changeType (resultDotBindTwoInp (preOrPost inputRecord.PreIndexRb inputRecord.PostIndexRb) incrementRbValue getOrigVal))
        
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
        let interpretedRecord = interpretingRecord inputRecord

        let updatedDP1 = resultDotBindTwoInp updateDataPathRegs (Ok dP) (regsMapF (Ok dP) (inputRecord.DestSourceReg) (fstRecTuple interpretedRecord))
        let updatedDP2 = resultDotBindTwoInp updateDataPathRegs updatedDP1 (regsMapF updatedDP1 (inputRecord.AddressReg) (sndRecTuple interpretedRecord))


        (Ok symbolTab, updatedDP2)



    ///Will just update the DataPath, SymbolTable is untouched
    let STRexec (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: MemInstr) = 
        //insert psuedo code here
        //Expect to return a uint32

        (Ok symbolTab, Ok dP)



    ///Updates the symbol table
    ///Used for DCDexec, EQUexec and FILLexec
    let updateSymbolTable (symbolTab: SymbolTable) (inputRecord: labelInstr) field = 
        let getLabel (rec1: labelInstr) = 
            match rec1.Name with
            | Ok y -> match y with
                      | Some z -> Ok z
                      | None -> Error (sprintf "updateSymbolTable: (Ok (%A).Name) = None" rec1)
            | Error n -> Error (n+"\n"+(sprintf "updateSymbolTable: (%A).Name = Error" rec1))
        let getValue field inputRecord =
            match inputRecord.InstructionType with
            | Ok EQU -> match field with
                        | Some y -> y
                        | None -> Error (sprintf "updateSymbolTable-getValue: (Ok (%A).EQUExpr) = None" field)
            | Ok FILL -> match field with
                         | Some y -> match y with
                                     | Ok _ -> Ok 0u
                                     | Error m -> Error m
                         | None -> Error (sprintf "updateSymbolTable-getValue: (Ok (%A).FillN) = None" field)
            | Ok DCD  ->match field with
                        | Some y -> y
                        | None -> Error (sprintf "updateSymbolTable-getValue: (Ok (%A).FillN) = None" field)
            | _ -> Error (sprintf "updateSymbolTable-getValue: InstructionType (%A) not EQU, DCD or FILL" inputRecord.InstructionType)
        let addToSymTab x y = symbolTab.Add(x,y)
        resultDotBindTwoInp addToSymTab (getLabel inputRecord) (getValue field inputRecord)

    ///Updates the Memory in the DataPath (Adds something
    /// to memory). Called by DCDexec and FILLexec
    let updateMemoryDataPath (inputRecord: labelInstr) (dP: DataPath<'INS>) =
        let dataValList = 
            let fillNf = 
                match inputRecord.FillN with
                | Some x -> match x with 
                            | Ok y -> y|>int
                            | _ -> 0
                | _ -> 0
            let rec makeZeroList length =
                match length with
                | 0 -> []
                | _ -> List.append (makeZeroList (length-1)) [0u]
            match inputRecord.InstructionType with
            | Ok x ->   match x with
                        | DCD ->    match inputRecord.DCDValueList with
                                    | Some z -> match z with
                                                | Ok u -> Ok (List.map (fun y -> (y|>int|>uint32)) u)
                                                | Error m -> Error m
                                    | None -> Error "updateMemoryDataPath-dataValList: DCDValueList = None"
                        | FILL -> match inputRecord.FillN with
                                  | Some y -> match y with 
                                              | Ok z -> Ok (makeZeroList fillNf)
                                              | Error m -> Error m
                                  | None -> Error "updateMemoryDataPath-dataValList: FillN = None" 
                        | EQU -> match inputRecord.EQUExpr with
                                 | Some y -> match y with 
                                             | Ok z -> Ok []
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
            // let doProcessing (dPMMf: MachineMemory<'INS>) (x: uint32 list) y = 
            //     match y with
            //     | [] -> Ok dPMMf
            //     | _ ->  
            //             let remainingAddrList = Ok x.[1..(List.length x)-1]
            //             let remainingValueList = Ok y.[1..(List.length y)-1]
            //             let updatedMachineMem = dPMMf.Add(WA (x.[0]+4u), DataLoc y.[0])
            //             updateMachineMemory' remainingAddrList updatedMachineMem remainingValueList
            // resultDotBindTwoInp (doProcessing dPMM) addrListRes dataValListRes'


        match (updateMachineMemory' (findAddrs dP) dP.MM dataValList) with
        | Ok x -> Ok {Fl = dP.Fl; Regs = dP.Regs; MM = x}
        | Error m -> Error m
        

    ///Taken out for testing - (Could not reference it in
    /// memInstructionTests when it was a subfunction of
    /// DCDexec)
    let removeOptionD (x:Result<ValueList,string> option) =   
        let makeType (x:ValueList)  =
            let y = (x.[0])|>uint32
            let temp =
                match Ok y with
                       | Ok y -> Ok y
                       | _ -> Error "should never happen"
            match x with
            | x -> Some temp 
            | _ -> None
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
    let DCDexec (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: labelInstr) = 
        let removeOptionD (x:Result<ValueList,string> option) =   
            let makeType (x:ValueList)  =
                let y = (x.[0])|>uint32
                let temp =
                    match Ok y with
                           | Ok y -> Ok y
                           | _ -> Error "should never happen"
                match x with
                | x -> Some temp 
                | _ -> None
            let removeResult x =
                match x with 
                | Ok y -> makeType y
                | Error m -> Some (Error m)

            match x with
            | Some y -> removeResult y
            | _ -> None

        (updateSymbolTable symbolTab inputRecord (removeOptionD (inputRecord.DCDValueList)), (updateMemoryDataPath inputRecord dP))                

    ///Takes in the current state of the program in the form
    /// of the SymbolTable and DataPath, as well as the 
    /// information record labelInstr and updates the current
    /// state of the program by executing an EQU instruction
    ///Updates SymbolTable only
    let EQUexec (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: labelInstr) = 
        (updateSymbolTable symbolTab inputRecord inputRecord.EQUExpr, Ok dP)                
    
    ///Executes a Fill instruction, updates Symbol Table and DataPath
    let FILLexec (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: labelInstr) = 
        (updateSymbolTable symbolTab inputRecord ((inputRecord.FillN)), (updateMemoryDataPath inputRecord dP))                




    let ADRexec (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: ADRInstr) = 
        let updateRegMap (dP: DataPath<'INS>) (inputRecord: ADRInstr) = 
            resultDotBindTwoInp (updateRegister dP) inputRecord.DestReg inputRecord.SecondOp 
        (Ok symbolTab,Result.map (updateDataPathRegs dP) (updateRegMap dP inputRecord))
    


    let generalExecHandler (inputRecord: LabelAndMemGeneralParse) (symbolTab: SymbolTable) (dP: DataPath<'INS>) =
        let labelInstructionsHandler (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: labelInstr) =
            let matchLabIns x =
                match x with
                | EQU -> Ok (EQUexec symbolTab dP inputRecord)
                | DCD -> Ok (DCDexec symbolTab dP inputRecord)
                | FILL -> Ok (FILLexec symbolTab dP inputRecord)
                | _ -> Error (sprintf "labelInstructionsHandler: InstructionType (%A) not recognised" x)
            Result.bind matchLabIns inputRecord.InstructionType
        let memInstructionsHandler (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: MemInstr) =
            let matchMemIns x =
                match x with
                | LDR -> Ok (LDRexec symbolTab dP inputRecord)
                | STR -> Ok (STRexec symbolTab dP inputRecord)
                | _ -> Error (sprintf "memInstructionsHandler: InstructionType (%A) not recognised" x)
            Result.bind matchMemIns inputRecord.InstructionType

        match inputRecord with
        | LabelO x -> Result.bind (labelInstructionsHandler symbolTab dP) x
        | AdrO x -> Result.map (ADRexec symbolTab dP) x
        // | MemO x -> 
