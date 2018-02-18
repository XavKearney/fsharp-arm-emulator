//MemInstructions
module memInstructions

    open CommonData
    open CommonLex
    open System
    open System.Text.RegularExpressions






//----------MEMORY INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type MemInstrType = LDR | STR 
    type LabelM = string option


    /// instruction (dummy: must change)
    type memInstr =
        {
            InstructionType: MemInstrType;
            DestSourceReg: RName;
            AddressReg: RName;
            BytesNotWords: bool;
            IncrementValue: int;
            PreIndexRbBy: bool;
            PostIndexRbBy: bool;
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

    /// map of all possible opcodes recognised
    // let opCodesMem = opCodeExpand memSpec


    /// Parse Active Pattern used by top-level code
    // let (|IMatch|_|) = parse

    let testS = "R0!, [R1, #N]!"
    let testTrim = testS.Trim()
    let len = testS.Length
    let IO = testTrim.LastIndexOf("!")
    
    /// Match a regular expression
    /// Return Some (m,grps) where m is the match string,
    /// grps is the list of match groups (if any)
    /// return None on no match
    let regexMatch (regex:string) (str:string) =
        let m = Text.RegularExpressions.Regex(regex).Match(str)
        printfn "m: %A" m
        if m.Success
        then
            let mLst = [ for x in m.Groups -> x.Value ]
            Some (List.head mLst, List.tail mLst)
        else None


    ///Match the pattern using a cached compiled Regex
    let (|Match|_|) pattern input =
        if input = null then None
        else
            let m = Regex.Match(input, pattern, RegexOptions.Compiled)
            if m.Success then Some [for x in m.Groups -> x]
            else None


    type Name = {First:string; Middle:option<string>; Last:string}

    let parseName name =
        match name with
        | Match @"^(\w+) (\w+) (\w+)$" [_; first; middle; last] ->
            Some({First=first.Value; Middle=Some(middle.Value); Last=last.Value})
        | Match @"^(\w+) (\w+)$" [_; first; last] ->
            Some({First=first.Value; Middle=None; Last=last.Value})
        | _ -> 
            None

    let testP = parseName "Alistair Patrick Wallace"

    // let RE = regexMatch "R[0-5]+" testS
    // let RE3 = regexMatch "(R)\w+" testS
    // let RE2 = regexMatch "R[0-15], *\[R[0-16], *#[0-6]\]"

    type FromOps = {Ra:RName; Rb: RName; IncrVal: int;
                        Post: bool; Rc: RName option;
                        Shift: int option}

///Parse function for Memory instructions such as LDR and
/// STR. Returns a record with all the information needed
/// to execute an LDR or STR instruction.
    let parseMemIns root suffix ls =
        let instTypeTmp = 
            match root with
            | "LDR"  -> LDR 
            | "STR" -> STR
        let bytes = 
            match suffix with
            | "" -> false
            | "B" -> true
        let pre = ((ls.Operands).Trim()).LastIndexOf("!") = (((ls.Operands).Trim()).Length-1)

        let parseOps ops =
            match ops with
            | Match @"R([1-9]|1[0-5])+ *, *\[ *R([1-9]|1[0-5])+ *]" [rA; rB] -> //Base Case
                Some({Ra=regNames.[rA.Value]; Rb=regNames.[rB.Value]; IncrVal=0; Post= false; Rc= None; Shift= None;})
            | Match @"R([1-9]|1[0-5])+ *, *\[ *R([1-9]|1[0-5])+ *, *#[0-9]+ *\]" [rA; rB; incV] -> //Increment
                Some({Ra=regNames.[rA.Value]; Rb=regNames.[rB.Value]; IncrVal=(incV.Value|>int); Post= false; Rc= None; Shift= None;})
            | Match @"R([1-9]|1[0-5])+ *, *\[ *R([1-9]|1[0-5])+ *] *, *#[0-9]+" [rA; rB; incV] -> //Post Indexing
                Some({Ra=regNames.[rA.Value]; Rb=regNames.[rB.Value]; IncrVal=(incV.Value|>int); Post= true; Rc= None; Shift= None;})
            | Match @"R([1-9]|1[0-5]) *, *\[ *R([1-9]|1[0-5]) *, *R([1-9]|1[0-5]) *\]" [rA; rB; incV] -> //Adding Registers
                Some({Ra=regNames.[rA.Value]; Rb=regNames.[rB.Value]; IncrVal=(incV.Value|>int); Post= false; Rc= None; Shift= None;})
            | Match @"R([1-9]|1[0-5]) *, *\[ *R([1-9]|1[0-5]) *, *R([1-9]|1[0-5]) *, *LSL *#[0-9] *\]" [rA; rB; rC; shft] -> //Shifting
                Some({Ra=regNames.[rA.Value]; Rb=regNames.[rB.Value]; IncrVal=0; Post= false; Rc= Some regNames.[rC.Value]; Shift= Some (shft.Value|>int);})
            | _ -> 
                None
        let regExMat = Option.get (parseOps ((ls.Operands).Trim()))

        {InstructionType= instTypeTmp;
            DestSourceReg= regExMat.Ra; AddressReg= regExMat.Rb;
            BytesNotWords= bytes; IncrementValue= regExMat.IncrVal;
            PreIndexRbBy= pre; PostIndexRbBy= regExMat.Post; 
            ExtraAddressReg= regExMat.Rc;
            ShiftExtraRegBy= regExMat.Shift;}












//----------ADR INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type ADRInstrType = ADRm 
    type LabelADR = string option


    /// instruction (dummy: must change)
    type ADRInstr =
        {
            InstructionType: ADRInstrType option;
            DestSourceReg: RName option;
            SecondOp: LabelADR;
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









//----------MEMORY INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type LabelInstrType = EQU | FILL | DCD
                                   //EquExpr is either:
                                   //a register relative address
                                   //a PC relative address
                                   //an absolute address
                                   //or a 32 bit integer
    type EquExpr = uint32 
    type ValueList = string list
    type LabelL = StrLabelL of string option



    /// instruction (dummy: must change)
    type labelInstr =
        {
            InstructionType: LabelInstrType;
            Name: LabelL;
            EQUExpr: EquExpr option;
            DCDValueList: ValueList option;            //What to fill the memory with
            FillN: uint32 option;
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


    let evalExpression (exp0: string) =
        let rec evalExpression' (exp: string) = 
            if String.exists (fun c -> (c ='*')) exp then
                exp.Split('*') 
                |> Seq.toList
                |> List.map (fun x -> evalExpression' x)
                |> List.reduce (fun a b -> a*b)
            elif String.exists (fun c -> (c ='+')) exp then
                exp.Split('+') 
                |> Seq.toList
                |> List.map (fun x -> evalExpression' x)
                |> List.reduce (fun a b -> a+b)
            elif String.exists (fun c -> (c ='-')) exp then
                exp.Split('-') 
                |> Seq.toList
                |> List.map (fun x -> evalExpression' x)
                |> List.reduce (fun a b -> a-b)
            else 
                exp |> uint32
        evalExpression' exp0



    let (|PosMultFour|Error|) x = if ((x % 4 = 0)&&(x>=0)) then PosMultFour else Error
    let isPosAndDivisibleByFour x = match x with PosMultFour -> x | Error -> 0


///Parse function for Label based instructions such as EQU
/// FILL and DCD. Returns a record with all the information
/// needed to execute an LDR or STR instruction.
    let parseLabelIns root ls =
        let InstTypeTmp = 
            match root with
            | "EQU"  -> EQU 
            | "FILL" -> FILL
            | "DCD"  -> DCD
        let (fillN, valList, equExp) =
            match InstTypeTmp with
            | EQU  ->   let equExp1 = evalExpression ls.Operands
                        (None, None, (Some equExp1))  
            | FILL ->   let fillN1 = ls.Operands |> int 
                                    |> isPosAndDivisibleByFour 
                                    |> uint32
                        (Some fillN1, None, None)
            | DCD  ->   let valList1 = (ls.Operands).Split(',') 
                                        |> Array.map (fun s-> s.Trim()) 
                                        |> Seq.toList
                        (None, Some valList1, None)
        let nameOut = StrLabelL ls.Label
        {InstructionType = InstTypeTmp; Name = nameOut; 
            EQUExpr = equExp; DCDValueList = valList; 
            FillN = fillN}

    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    // let parse (ls: LineData) : Result<Parse<Instr>,string> option =
    //     let parse' (instrC, (root,suffix,pCond)) =

    //         let (WA la) = ls.LoadAddr // address this instruction is loaded into memory
    //         // this does the real work of parsing
    //         // dummy return for now
            

    //         // match instrC with
    //         // | DCD -> PInstrTmp = parseDCD suffix ls
    //         // | MEM -> PInstrTmp = parseMEM root suffix ls
    //         // | ADR -> PInstrTmp = parseADR suffix ls
    //         // | LABEL -> PInstrTmp = parseLabelIns root suffix ls
                        
    //         Ok { 
    //             // Normal (non-error) return from result monad
    //             // This is the instruction determined from opcode, suffix and parsing
    //             // the operands. Not done in the sample.
    //             // Note the record type returned must be written by the module author.
    //             // PInstr={InstructionType= (); DestSourceReg= (); SecondOp= ();}; 
    //             PInstr = 
    //                 match instrC with
    //                 | LABEL -> parseLabelIns root suffix ls
    //                 | MEM   -> parseMEM root suffix ls
    //                 | ADR   -> parseADR suffix ls


    //             // This is normally the line label as contained in
    //             // ls together with the label's value which is normally
    //             // ls.LoadAddr. Some type conversion is needed since the
    //             // label value is a number and not necessarily a word address
    //             // it does not have to be div by 4, though it usually is
    //             PLabel = ls.Label |> Option.map (fun lab -> lab, la) ; 


    //             // this is the number of bytes taken by the instruction
    //             // word loaded into memory. For arm instructions it is always 4 bytes. 
    //             // For data definition DCD etc it is variable.
    //             //  For EQU (which does not affect memory) it is 0
    //             PSize = 4u; 

    //             // the instruction condition is detected in the opcode and opCodeExpand                 
    //             // has already calculated condition already in the opcode map.
    //             // this part never changes
    //             PCond = pCond 
    //             }
    //     // Map.tryFind ls.OpCode opCodesDCD // lookup opcode to see if it is known
    //     // |> Option.map parse' // if unknown keep none, if known parse it.
    //     if (Map.tryFind ls.OpCode opCodesLabel |> Option.map parse') <> None then
    //         Map.tryFind ls.OpCode opCodesLabel |> Option.map parse'
    //     elif (Map.tryFind ls.OpCode opCodesADR |> Option.map parse') <> None then
    //         Map.tryFind ls.OpCode opCodesADR |> Option.map parse'
    //     elif (Map.tryFind ls.OpCode opCodesMem |> Option.map parse') <> None then
    //         Map.tryFind ls.OpCode opCodesMem |> Option.map parse'
    //     else 
    //         None


    /// Parse Active Pattern used by top-level code
    // let (|IMatch|_|) = parse




    //Active Pattern Matching Messing
    // let (|Odd|Even|) x = if x % 2 = 0 then Even else Odd
    // let isDivisibleByTwo x = match x with Even -> true | Odd -> false
    // let test4 = isDivisibleByTwo 4
    // let test3 = isDivisibleByTwo 3


    // let (|UpperCase|) (x:string) = x.ToUpper()
    // let result = match "foo2" with
    //              | UpperCase "FOO" -> true
    //              | _ -> false
    // assert (result = true)
    // let ucName name = match name with UpperCase result -> result
    // let testUC = ucName "test"



    // let (|ToColor|) x =
    //     match x with
    //     | "red"   -> System.Drawing.Color.Red
    //     | "blue"  -> System.Drawing.Color.Blue
    //     | "white" -> System.Drawing.Color.White
    //     | _       -> failwith "Unknown Color"
    // let form = new System.Windows.Forms.Form()
    // let (ToColor col) = "red"
    // form.BackColor <- col













    let LDRexec = 
        //insert psuedo code here
        //Eg: LDR r8, [r10]     //Loads r8 with the value 
                                // from the address in r10
        //Eg:LDR r2, [r5,#960]! //Loads r2 with the value
                                // from the address 960 bytes
                                // above the address stored
                                // in r5 and increments r5 by
                                // 960  
        //See below link for all 6 LDR uses
        //https://intranet.ee.ic.ac.uk/t.clarke/arch/html16/CT6.html
        //Needs to change the DataPath Record of identifier MM
        // MM is of type MachineMemory<'INS>
        //This has two part, WAddr is a address of uin32 
        // the other is MemLoc<'INS> which is the content of the
        // memory address. It can either be a uint32 or it can
        // be an instruction. I think this is just for storing 
        // the program in memory but apparently it is also used
        // for branching.
        0

    let STRexec = 
        //insert psuedo code here
        0

    let ADRexec (Rdest: RName) (Lab: LabelADR) = 
        //Takes the address of a label and puts it into the 
        // register. 
        //Eg: ADR		R0, BUFFIN1
        //Eg2: start		MOV		r0,#10
        //                  ADR		r4,start 
        //                  (; => SUB r4,pc,#0xc)
        //
        
        0

    let DCDexec = 
        //Takes a label and a list of values, it then stores 
        // the values in memory, they can be accessed using
        // the label.
        //Eg: test		DCD		1,3,5
        // test is the label, 1,3 and 5 are the stored values
        //Eg2: data3   DCDU    1,5,20
        // this line defines these three values at the label 
        // data3, however the U means that the values are not
        // word aligned.
        //NOTE: this line will not run in visual but it should
        0

    let EQUexec = 
        //insert psuedo code here
        //Assigns a value to a label
        //Eg: abc EQU 2 
        // this line assigns the value 2 to the label abc
        //Eg2: xyz EQU label+8
        // this assigns the address (label+8) to the label xyz

        0

    let FILLexec = 
        //insert psuedo code here
        //Eg:   data4	Fill		12
        // fills 12 data slots with 0. It basically initialises
        // the memory. The value or expression at the end must
        // evaluate to a multiple of 4.
        //Eg2:   data4	Fill		4*3
        //As you can see, the last operand can be an expression
        0
        