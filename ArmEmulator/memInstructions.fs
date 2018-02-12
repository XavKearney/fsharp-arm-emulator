//MemInstructions
module memInstructions

    open CommonData
    open CommonLex





//----------MEMORY INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type MemInstrType = LDR | STR | ADR 
    type FlexOp2Mem = | Label | Addr | AddrPlus | PreIndexed | PostIndexed | AddrPlusAddr | AddrShftOp
    type LabelM = string option


    /// instruction (dummy: must change)
    type memInstr =
        {
            InstructionType: MemInstrType option;
            DestSourceReg: RName option;
            SecondOp: FlexOp2Mem;
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
    let opCodesMem = opCodeExpand memSpec


    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse













//----------ADR INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type ADRInstrType = ADR 
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
    let opCodesMem = opCodeExpand ADRSpec


    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse









//----------MEMORY INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type LabelInstrType = EQU | Fill
    type ValueOrExpression = | Value | Expression


    /// instruction (dummy: must change)
    type labelInstr =
        {
            InstructionType: LabelInstrType option;
            SecondOp: ValueOrExpression;
        }

    /// parse error (dummy, but will do)

    /// sample specification for set of instructions
    /// very incomplete!
    let labelSpec = {
        InstrC = LABEL
        Roots = ["EQU";"Fill"]
        Suffixes = [""]
    }

    /// map of all possible opcodes recognised


    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse










//----------DCD INSTRUCTION DEFINITION AND PARSING-------------------------------------------------

    type DCDInstrType = DCD 
    type LabelDCD = string option
    type ValueList = string list option


    /// instruction (dummy: must change)
    type DCDInstr =
        {
            InstructionType: DCDInstrType option;
            Label: LabelDCD;
            UnAligned: bool;
            Value: ValueList;
        }

    /// parse error (dummy, but will do)

    /// sample specification for set of instructions
    /// very incomplete!
    let DCDSpec = {
        InstrC = DCD
        Roots = ["DCD"]
        Suffixes = ["";"U"]
    }

    /// map of all possible opcodes recognised
    let opCodesDCD = opCodeExpand DCDSpec
    let opCodesADR = opCodeExpand ADRSpec
    let opCodesMem = opCodeExpand memSpec
    let opCodesLabel = opCodeExpand labelSpec



    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse (ls: LineData) : Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =

            let (WA la) = ls.LoadAddr // address this instruction is loaded into memory
            // this does the real work of parsing
            // dummy return for now
            
            
            match instrC with
            | DCD -> Ok.PInstr = parseDCD suffix ls
            | MEM -> Ok.PInstr = parseMEM root suffix ls
            | ADR -> Ok.PInstr = parseADR suffix ls
            | LABEL -> Ok.PInstr = parseLabelIns root suffix ls
            
            Ok { 
                // Normal (non-error) return from result monad
                // This is the instruction determined from opcode, suffix and parsing
                // the operands. Not done in the sample.
                // Note the record type returned must be written by the module author.
                // PInstr={InstructionType= (); DestSourceReg= (); SecondOp= ();}; 


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
                PSize = 4u; 

                // the instruction condition is detected in the opcode and opCodeExpand                 
                // has already calculated condition already in the opcode map.
                // this part never changes
                PCond = pCond 
                }
        Map.tryFind ls.OpCode opCodesDCD // lookup opcode to see if it is known
        |> Option.map parse' // if unknown keep none, if known parse it.
        if (Map.tryFind ls.OpCode opCodesDCD |> Option.map parse') <> None then
            Map.tryFind ls.OpCode opCodesDCD |> Option.map parse'
            else 


    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse


    let parseDCD suffix (ls: LineData) = 
        let valList = ls.Operands |> ops.Split(',') |> Array.map (fun s-> s.Trim())
        let unalign =
            match suffix.Trim with 
            | "" -> false
            | "U" -> true
        {InstructionType = DCD; Label = ls.Label; UnAligned = unalign; ValueList = valList}


    let parseMEM root suffix (ls: LineData) = 
        






    let executeDCD 










    let LDR = 
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

    let STR = 
        //insert psuedo code here
        0

    let ADR (Rdest: RName) (Lab: Label) = 
        //Takes the address of a label and puts it into the 
        // register. 
        //Eg: ADR		R0, BUFFIN1
        //Eg2: start		MOV		r0,#10
        //                  ADR		r4,start 
        //                  (; => SUB r4,pc,#0xc)
        //
        
        0

    let DCD = 
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

    let EQU = 
        //insert psuedo code here
        //Assigns a value to a label
        //Eg: abc EQU 2 
        // this line assigns the value 2 to the label abc
        //Eg2: xyz EQU label+8
        // this assigns the address (label+8) to the label xyz

        0

    let FILL = 
        //insert psuedo code here
        //Eg:   data4	Fill		12
        // fills 12 data slots with 0. It basically initialises
        // the memory. The value or expression at the end must
        // evaluate to a multiple of 4.
        //Eg2:   data4	Fill		4*3
        //As you can see, the last operand can be an expression
        0
        