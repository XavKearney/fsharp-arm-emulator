//Unresolved Comments from Xav
//Line 175 let ADRSpec = {...
// Need to put ADR into the MEM class and work out how
// to give it a different spec while being part of the 
// same class

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
    open ParseExpr





//----------MEMORY INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type MemInstrType = LDR | STR 
    type PreOrPostIndex = Pre | Post | Neither


    ///The Record type which encapsulates all the 
    /// information needed to execute an LDR or STR 
    /// instruction
    type MemInstr =
        {
            InstructionType: MemInstrType;
            DestSourceReg: RName;
            AddressReg: RName;
            BytesNotWords: bool;
            IncrementValue: int;
            PreOrPostIndRb: PreOrPostIndex;
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
            if m.Success then
                [0..m.Groups.Count-1]
                |> List.map (fun i -> m.Groups.[i])
                |> Some
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

    let intIsPos x =
        match x with
        | v when v >= 0 -> x
        | _ -> -1 

    let isPos (x: Group) (y: Group) = match (x.Value|>int) with 
                                      | v when v >= 0 -> (Some v,(Some regNames.[y.Value])) 
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
        let defaultRecord ra rb = {Ra= regNamesTryFindMonad ra; Rb=regNamesTryFindMonad rb; IncrVal=0; Post= false; Rc= None; Shift= None;}
        let parseOps ops =
            match ops with
            | Match @"(R1[0-5]|R[0-9]) *, *\[ *(R1[0-5]|R[0-9]) *] *, *#(-?[0-9]+)" [_; rA; rB; incV] -> //Post Increment
                Ok {(defaultRecord rA rB) with IncrVal=(incV.Value|>int); Post= true;}
            | Match @"(R1[0-5]|R[0-9]) *, *\[ *(R1[0-5]|R[0-9]) *]" [_; rA; rB] -> //Base Case
                Ok (defaultRecord rA rB)
            | Match @"(R1[0-5]|R[0-9]) *, *\[ *(R1[0-5]|R[0-9]) *, *#(-?[0-9]+) *\]" [_; rA; rB; incV] -> //Num Increment
                Ok {(defaultRecord rA rB) with IncrVal=(incV.Value|>int);}
            | Match @"(R1[0-5]|R[0-9]) *, *\[ *(R1[0-5]|R[0-9]) *, *(R1[0-5]|R[0-9]) *\]" [_; rA; rB; rC] -> //Adding Registers
                Ok {(defaultRecord rA rB) with Rc= Some regNames.[rC.Value];}
            | Match @"(R1[0-5]|R[0-9]) *, *\[ *(R1[0-5]|R[0-9]) *, *(R1[0-5]|R[0-9]) *, *LSL *#(-?[0-9]+) *\]" [_; rA; rB; rC0; shft0] -> //Shifting
                let shft, rC = isPos shft0 rC0
                Ok {(defaultRecord rA rB) with Rc= rC; Shift= shft;}
            | _ -> 
                Error (sprintf "ops didn't match anything, ops: %A" ops)
        let pre (z: FromOps) = 
            match (((ls.Operands).Trim()).LastIndexOf("!") = (((ls.Operands).Trim()).Length-1), z.Post) with
            | (true, false) -> Ok Pre
            | (false, true) -> Ok Post
            | (false, false) -> Ok Neither
            | (true, true) -> Error "parseMemIns: Both Pre and Post indexing"
        let defaultRecord = Ok {InstructionType= LDR;
                DestSourceReg= R0; AddressReg= R1;
                BytesNotWords= true; IncrementValue= 0;
                PreOrPostIndRb= Neither; 
                ExtraAddressReg= Some R0;
                ShiftExtraRegBy= Some 0;}
        let testOut (z: FromOps) =
            defaultRecord
            |> resultDotBindTwoInp (fun y x -> {x with InstructionType = y}) instTypeTmp
            |> resultDotBindTwoInp (fun y x -> {x with DestSourceReg = y}) z.Ra
            |> resultDotBindTwoInp (fun y x -> {x with AddressReg = y}) z.Rb
            |> resultDotBindTwoInp (fun y x -> {x with BytesNotWords = y}) bytes 
            |> resultDotBindTwoInp (fun y x -> {x with IncrementValue = y}) (Ok z.IncrVal)
            |> resultDotBindTwoInp (fun y x -> {x with PreOrPostIndRb = y}) (pre z)
            |> resultDotBindTwoInp (fun y x -> {x with ExtraAddressReg = y}) (Ok z.Rc)
            |> resultDotBindTwoInp (fun y x -> {x with ShiftExtraRegBy = y}) (Ok z.Shift)
        parseOps ((ls.Operands).Trim())
        |> Result.bind testOut
        




//----------ADR INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type ADRInstrType = ADRm
    //The m is needed to it doesn't interfere with ADR of
    // type InstrClass in Common Lex


    ///A record which encapsulates all the necessary 
    /// information for executing an ADR instruction
    type ADRInstr =
        {
            InstructionType: ADRInstrType;
            DestReg: RName;
            SecondOp: uint32;
        }

    ///A spec for the ADR Class of instrucions
    let ADRSpec = {
        InstrC = ADR
        Roots = ["ADR"]
        Suffixes = [""]
    }



    ///Evaluates an expression involving +-* and labels
    /// which evaluate to the addresses they represent
    ///NEEDS DOING:
    /// - Add multiple bracket functionality 
    ///   Eg 2*(6+(3*4)-(6+3))*5
    /// - Add working CheckLiteral function which works for -ve's
    let evalExpression (exp0: string) (symTab: SymbolTable) (labels: bool) =
        evalExpr symTab labels exp0 



    
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
            | None   -> 
                   match ((ls.Operands).Trim()) with
                   | Match @"(R[0-9]|1[0-5]) *, *(.*)" [_; _; expression] -> 
                      evalExpression expression.Value ([] |> Map.ofList) false
                   | _ -> Error (sprintf "parseAdrIns: Line Data in incorrect form\nls.Operands: %A" ls.Operands)
            | Some x -> 
                   match ((ls.Operands).Trim()) with
                   | Match @"(R[0-9]|1[0-5]) *, *(.*)" [_; _; expression] -> 
                      evalExpression expression.Value x true
                   | _ -> Error (sprintf "parseAdrIns: Line Data in incorrect form\nls.Operands: %A" ls.Operands)
        let makeOutFromSO x aITT =
            {InstructionType= aITT;
                //Will be overwritten so just a dummy to type-check
                DestReg= R0; 
                SecondOp= x;}
        let makeOutFromSO2 rD x =
            {x with DestReg= rD;
            }
        let errorMessage1 = resultDotBindTwoInp selectFirst adrInstrTypeTmp rD  
        resultDotBindTwoInp makeOutFromSO labelExpVal errorMessage1 
        |> resultDotBindTwoInp makeOutFromSO2 rD







//----------MEMORY INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type LabelInstrType = EQU | FILL | DCD
   //EquExpr is either:
   // a register relative address
   // a PC relative address
   // an absolute address
   // or a 32 bit integer
    type EquDcdFillSpecific = 
        | Eq of uint32
        | Vl of string list
        | Fl of uint32


    ///A record that contains all the information needed
    /// to execute a label instruction (EQU, FILL of DCD)
    type LabelInstr =
        {
            InstructionType: LabelInstrType;
            Name: string option;
            //The value to assign to the label for EQU
            //Or what to fill the memory with for DCD
            //Or the number of memory cells to initialise as 
            // 0 for Fill
            EquDcdFill: EquDcdFillSpecific;
        }

    ///Specification for label instructions
    let labelSpec = {
        InstrC = MISC
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

///This function returns an idicator of whether or not one
/// of the three instruction specific fields of the label
/// instruction is an error. As they are all different 
/// if cannot return the field if it is Ok _ so it returns
/// an integer instead. 
    let optionResultAbstract u v w = 
        match (u,v,w) with
        | (Some x, _, _) ->
                           match x with 
                           | Ok _ -> Ok 0
                           | Error m -> Error m
        | (_, Some y, _) -> 
                           match y with 
                           | Ok _ -> Ok 0
                           | Error m -> Error m
        | (_, _, Some z) -> 
                           match z with 
                           | Ok _ -> Ok 0
                           | Error m -> Error m        
        | (_, _, _) -> Error "optionResultAbstract: All inputs none"

    ///Parse function for Label based instructions such as EQU
    /// FILL and DCD. Returns a record with all the information
    /// needed to execute an LDR or STR instruction.
    let parseLabelIns root ls =
        let evalExprHandler ops symT labels =
            match symT with 
            | None -> (evalExpression ops ([] |> Map.ofList) false)
            | Some x -> (evalExpression ops x labels)
        let instTypeTmp = 
            match root with
            | "EQU"  -> Ok EQU 
            | "FILL" -> Ok FILL
            | "DCD"  -> Ok DCD
            | _      -> Error (sprintf "parseLabelIns: root (%A) was not EQU, FILL or DCD" root) 
        let insSpecificReturn =
            match instTypeTmp with
            | Ok EQU  -> 
                let equExp1 = evalExprHandler ls.Operands (ls.SymTab) true
                match equExp1 with
                | Ok x -> Ok (Eq x)
                | Error m -> Error m
            | Ok FILL -> 
                let fillN1 = evalExprHandler ls.Operands (ls.SymTab) false
                              |> checkPosAndDivFour
                match fillN1 with
                | Ok x -> Ok (Fl x)
                | Error m -> Error m
            | Ok DCD  -> 
                let valList = (ls.Operands).Split(',') 
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
                match valListRet with
                | Ok x -> Ok (Vl x)
                | Error m -> Error m
            | Error m -> Error m
        let nameOut = 
            match ls.Label with
            | None -> if ((instTypeTmp = Ok EQU)||(instTypeTmp = Ok DCD))
                      then Error (sprintf "parseLabelIns: EQU and DCD instructions (%A) must have a label\nls: %A" root ls)
                      else Ok None
            | Some _ -> Ok ls.Label
        let defaultReturn = 
            Ok {InstructionType = DCD; Name = Some "defaultLabel"; 
                EquDcdFill = (Eq 4u)}
        defaultReturn
        |> resultDotBindTwoInp (fun y x -> {x with InstructionType = y}) instTypeTmp 
        |> resultDotBindTwoInp (fun y x -> {x with Name = y}) nameOut 
        |> resultDotBindTwoInp (fun y x -> {x with EquDcdFill = y}) insSpecificReturn 



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
               ((List.length y)*4)|>uint32
            match x with
            | Ok y -> 
                match y.EquDcdFill with
                | Vl z -> removeOpt z
                | _ -> 0u
            | Error _ -> 0u
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
                | MISC -> (Ok (LabelO (parseLabelIns root ls)))
                | MEM   -> (Ok (MemO (parseMemIns root suffix ls)))
                | ADR   -> (Ok (AdrO (parseAdrIns root ls)))
                | _ -> Error "parse: Instruction class not supported."

            let outputRec pIn = 
                let pSizeTmp =
                    match root with
                    | "EQU" -> 0u
                    | "DCD" -> findDcdPSize pIn
                    | _     -> 4u
                Ok { 
                    // Normal (non-error) return from result monad
                    // This is the instruction determined from opcode, suffix and parsing
                    // the operands. Not done in the sample.
                    // Note the record type returned must be written by the module author.
                    // PInstr={InstructionType= (); DestSourceReg= (); SecondOp= ();}; 
                    PInstr = pIn


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
            match pInstrTmp with
            | Ok x -> 
                match x with
                | LabelO (Ok _) | MemO (Ok _) | AdrO (Ok _) -> outputRec x
                | LabelO (Error m) | MemO (Error m) | AdrO (Error m) -> Error m
            | Error m -> Error m     
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

    ///Returns end value of Ra, Rb. Called by execSTR and 
    /// LRD exec
    let interpretingRecord (dP: DataPath<'INS>) (inputRecord: MemInstr) =
        ///Finds Original Ra and Rb values
        let getOrigVal inputRecord (dP: DataPath<'INS>) = 
            let getOriginalRegisterVals (Ra: RName) (Rb: RName) =
                (Map.find Ra dP.Regs, Map.find Rb dP.Regs)
            getOriginalRegisterVals inputRecord.DestSourceReg inputRecord.AddressReg

        let makeBytes (bytes: bool) (x,y,z) =
            match bytes with
            | false -> (x,y,z)
            | true -> (x&&&0xFFu,y,z)
        ///Finds the value to increment Rb by, considering
        /// extra registers and shifts        
        let incrementRbValue (inputRecord: MemInstr) (dP: DataPath<'INS>) =                
            let powerF (baseF: uint32) (expF: uint32) = 
                    ((baseF|>float)**(expF|>float))|>uint32
            match inputRecord.ExtraAddressReg with
            | None -> inputRecord.IncrementValue |> uint32
            | Some x -> 
                       let rC = Map.find x dP.Regs
                       match inputRecord.ShiftExtraRegBy with
                       | None -> rC
                       | Some y -> if y >= 0 then rC*(powerF 2u (y|>uint32))
                                    else 0u


        ///Returns the Ra and Rb values
        let preOrPost preOrPost (inputRecord: MemInstr) (dP: DataPath<'INS>) (rABTup: (uint32 * uint32)) x =
            let rA = fst rABTup
            let rB = snd rABTup
            match inputRecord.InstructionType with
            | LDR -> 
                       match preOrPost with
                       | Pre -> accessMemoryLocation dP (WA (rB+x)), rB+x, 0u
                       | Post -> accessMemoryLocation dP (WA (rB)), rB+x, 0u
                       | Neither -> accessMemoryLocation dP (WA (rB+x)), rB, x
            | STR -> 
                       match preOrPost with
                       | Pre -> Ok rA, rB+x, 0u
                       | Post -> Ok rA, rB+x, (-1|>uint32)*x
                       | Neither -> Ok rA, rB, x
        ///Removes unnecessary levels of Result
        let abstractTupleResult v = 
            match v with
            | (Ok z, u, v) -> Ok (z,u,v)
            | (_, _, _) -> Error "execLDR-interpretingRecord: Error accessing memory location"
        let changedToBytes (inputRecord: MemInstr) (a: Result<(uint32 * uint32 * uint32),string>) =
            resultDotBindTwoInp makeBytes (Ok inputRecord.BytesNotWords) a 
        incrementRbValue inputRecord dP
        |> preOrPost inputRecord.PreOrPostIndRb inputRecord dP
            (getOrigVal inputRecord dP)
        |> abstractTupleResult
        |> changedToBytes inputRecord                    

    let makeTuple a b = (a,b)

    let abstractResults (x: Result<(Result<DataPath<'INS>,string> * Result<Map<string,uint32>,string>),string>) =
        match x with
        | Error m -> Error m
        | Ok (a, b) -> resultDotBindTwoInp makeTuple a b

    
    ///Will just update the DataPath, SymbolTable is untouched
    let execLDR (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: MemInstr) = 
        let fstRecTuple x =
            match x with 
            | Ok (a,_,_) -> Ok a
            | Error m -> Error m 
        let sndRecTuple x = 
            match x with 
            | Ok (_,b,_) -> Ok b
            | Error m -> Error m 
        let regsMapF (dPF: Result<DataPath<'INS>,string>) reg x = 
            match dPF with 
            | Ok y -> (resultDotBindTwoInp (updateRegister y) (Ok reg) x)
            | Error m -> Error m

        let updatedDP2 = 
            let updateDP f field dP0 = 
                inputRecord
                |> interpretingRecord dP 
                |> f 
                |> regsMapF dP0 field 
                |> resultDotBindTwoInp updateDataPathRegs dP0
            (Ok dP)
            |> updateDP fstRecTuple inputRecord.DestSourceReg 
            |> updateDP sndRecTuple inputRecord.AddressReg

        (updatedDP2, Ok symbolTab)
        |> Ok
        |> abstractResults



    ///Will just update the DataPath, SymbolTable is untouched
    let execSTR (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: MemInstr) = 
        let updatedSTRMachineMem (tuple: Result<(uint32 * uint32 * uint32),string>) =
            match tuple with
            | Ok (a,b,c) -> Ok ((dP.MM).Add(WA (b+c), DataLoc a))
            | Error m -> Error m
        let updatedRegMap = 
            let regVals = interpretingRecord dP inputRecord
            let sndTup tup = 
                match tup with
                | (_,y,_) -> y
            let thrdTup tup = 
                match tup with
                | (_,_,z) -> z
            match regVals with
            | Error _ -> Map.empty                       
            | Ok y -> updateRegister dP inputRecord.AddressReg (sndTup y)
        inputRecord
        |> interpretingRecord dP 
        |> updatedSTRMachineMem 
        |> Result.map (fun x -> {dP with MM = x; Regs = updatedRegMap}) 
        |> fun a -> Ok (a, Ok symbolTab)
        |> abstractResults


    ///Taken out for testing - (Could not reference it in
    /// memInstructionTests when it was a subfunction of
    /// execDCD)
    let makeType (x: string list) =   
        let y = (x.[0])|>uint32
        match Ok y with
        | Ok y -> Ok y
        | _ -> Error "makeType: should never happen"

    let findMaxAddr (dP: DataPath<'INS>) = 
        let waToUint32 (k,_) =
            match k with
            | WA y -> y
        let checkBigEnough x =
            match x with
            | v when v >= 0xFCu -> x
            | _ -> 0xFCu
        match dP.MM with
        | x when x = Map.empty -> 0xFCu
        | _ ->
            dP.MM
            |> Map.toSeq
            |> Seq.map waToUint32
            |> Seq.max
            |> checkBigEnough


    ///Updates the symbol table
    ///Used for execDCD, execEQU and execFILL
    let updateSymbolTable (symbolTab: SymbolTable) (inputRecord: LabelInstr) (dP: DataPath<'INS>) (field: EquDcdFillSpecific) = 
        let getLabel rec1 = 
            match rec1.Name with
            | Some z -> Ok z
            | None -> Error (sprintf "updateSymbolTable: (Ok (%A).Name) = None" rec1)
        let getValue (field: EquDcdFillSpecific) inputRecord =
            match inputRecord.InstructionType with
            | EQU -> 
                match field with
                | Eq y -> Ok y
                | _ -> Error (sprintf "updateSymbolTable-getValue: InstructionType = EQU but EquDcdFillSpecific != Eq _")
            | FILL -> 
                Ok ((findMaxAddr dP)+4u)
            | DCD  ->
                Ok ((findMaxAddr dP)+4u)
        let addToSymTab x y = symbolTab.Add(x,y)
        resultDotBindTwoInp addToSymTab (getLabel inputRecord) (getValue field inputRecord)


    ///Updates the Memory in the DataPath (Adds something
    /// to memory). Called by execDCD and execFILL
    let updateMemoryDataPath (inputRecord: LabelInstr) (dP: DataPath<'INS>) =
        let dataValList = 
            let fillNf = 
                match inputRecord.EquDcdFill with
                | Fl x -> 
                   (x/4u)|>int
                | _ -> 0
            match inputRecord.InstructionType with
            | DCD ->    
                match inputRecord.EquDcdFill with
                | Vl z -> 
                    Ok (List.map (fun y -> (y|>int|>uint32)) z)
                | _ -> Error "updateMemoryDataPath-dataValList: Instruction type = DCD but EquDcdFill != Vl x"
            | FILL -> 
                match inputRecord.EquDcdFill with
                | Fl _ -> 
                    Ok (List.replicate fillNf 0u)
                | _ -> Error "updateMemoryDataPath-dataValList: Instruction type = FILL but EquDcdFill != Fl x"
            | EQU -> 
                match inputRecord.EquDcdFill with
                | Eq _ -> 
                    Ok []
                | _ -> Error "updateMemoryDataPath-dataValList: Instruction type = EQU but EquDcdFill != Eq x"

        let getAddrList lst length =
            let rec getAddrList' list len =
                match len with 
                | 0 -> list
                | 1 -> list
                | _ ->  getAddrList' (List.append list [((List.last list)+4u)]) (len-1)
            match length with
            | Ok x -> Ok (getAddrList' lst x)
            | Error m -> Error m

        let findAddrs (dP: DataPath<'INS>) :Result<uint32 list, string>=
            if ((dP.MM).IsEmpty) then
                getAddrList [0xFCu] (Result.map (List.length) dataValList)
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
        

    ///Executes the DCD instruction, taking in the symbol table,
    /// DataPath and labelInstr record and outputing updated 
    /// Symbol tables and DataPaths
    let execDCD (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: LabelInstr) = 
        inputRecord.EquDcdFill
        |> updateSymbolTable symbolTab inputRecord dP
        |> fun b -> Ok ((updateMemoryDataPath inputRecord dP), b)
        |> abstractResults

    ///Takes in the current state of the program in the form
    /// of the SymbolTable and DataPath, as well as the 
    /// information record labelInstr and updates the current
    /// state of the program by executing an EQU instruction
    ///Updates SymbolTable only
    let execEQU (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: LabelInstr) = 
        inputRecord.EquDcdFill
        |> updateSymbolTable symbolTab inputRecord dP
        |> fun b -> Ok (Ok dP, b)
        |> abstractResults 
    
    ///Executes a Fill instruction, updates Symbol Table and DataPath
    let execFILL (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: LabelInstr) = 
        inputRecord.EquDcdFill
        |> updateSymbolTable symbolTab inputRecord dP
        |> fun b -> Ok (updateMemoryDataPath inputRecord dP, b)
        |> abstractResults 




    let execADR (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: ADRInstr) = 
        let updateRegMap (dP: DataPath<'INS>) (inputRecord: ADRInstr) = 
            updateRegister dP inputRecord.DestReg inputRecord.SecondOp
        inputRecord
        |> updateRegMap dP 
        |> updateDataPathRegs dP
        |> fun a -> Ok (Ok a, Ok symbolTab)
        |> abstractResults 

        

    let execInstr (dP: DataPath<'INS>) (symbolTab: SymbolTable) (parseInputRecord: Parse<ReturnInstr>) =
        let inputRecord = parseInputRecord.PInstr
        let labelInstructionsHandler (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: LabelInstr) =
            let matchLabIns x =
                match x with
                | EQU -> (execEQU symbolTab dP inputRecord)
                | DCD -> (execDCD symbolTab dP inputRecord)
                | FILL -> (execFILL symbolTab dP inputRecord)
            matchLabIns inputRecord.InstructionType
        let memInstructionsHandler (symbolTab: SymbolTable) (dP: DataPath<'INS>) (inputRecord: MemInstr) =
            let matchMemIns x =
                match x with
                | LDR -> (execLDR symbolTab dP inputRecord)
                | STR -> (execSTR symbolTab dP inputRecord)
            Result.bind matchMemIns (Ok inputRecord.InstructionType)

        match inputRecord with
        | LabelO x -> Result.bind (labelInstructionsHandler symbolTab dP) x
        | AdrO x -> Result.bind (execADR symbolTab dP) x
        | MemO x -> Result.bind (memInstructionsHandler symbolTab dP) x




    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse
