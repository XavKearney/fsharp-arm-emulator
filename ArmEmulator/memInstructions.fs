//MemInstructions
module memInstructions

    open CommonData
    open CommonLex
    

    type MemInstrType = LDR | STR | DCD | ADR | Fill | EQU
    type FlexOp2Mem = | Label | Addr | AddrPlus | PreIndexed | PostIndexed | AddrPlusAddr | AddrShftOp
    type Label = string

    type memInstr =
        {
            InstructionType: MemInstrType option;
            DestSourceReg: RName option;
            SecondOp: FlexOp2Mem;
        }






    /// parse error (dummy, but will do)
    type ErrInstr = string



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
        //insert psuedo code here
        0

    let EQU = 
        //insert psuedo code here
        0

    let FILL = 
        //insert psuedo code here
        0
        