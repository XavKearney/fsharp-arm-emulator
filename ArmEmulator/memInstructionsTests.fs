//A series of tests for all the functions in memInstructions
module memInstructionsTests
    open CommonData
    open CommonLex
    open Expecto
    open Expecto.ExpectoFsCheck
    open MemInstructions
    open Expecto.Logging
    open System


    [<Tests>]
    let parseLabelInsTest = 
        let x:SymbolTable = ["a",uint32 2] |> Map.ofList
        let ldFunc Lab Ops = 
                {LoadAddr= WA 100u; 
                    Label= Some Lab; 
                    SymTab= Some x;
                    OpCode= "";
                    Operands= Ops}
        let makeTest root ld name output =
            testCase name <| fun () ->
                Expect.equal (parseLabelIns root ld) output (sprintf "Label Parsing Tests '%s'" root)
        Expecto.Tests.testList "parseLabelIns Tests"
                [   

                    makeTest "EQU" (ldFunc "labelT" "4") "EQU1" {InstructionType = EQU; Name = (StrLabelL (Some "labelT")); 
                                                        EQUExpr = (Some 4u); DCDValueList = None;
                                                        FillN = None}
                    makeTest "EQU" (ldFunc "labelT" "2") "EQU2" {InstructionType = EQU; Name = (StrLabelL (Some "labelT")); 
                                                                            EQUExpr = (Some 2u); DCDValueList = None;
                                                                            FillN = None}
                    makeTest "EQU" (ldFunc "labelT" "3*4") "EQU Mult" {InstructionType = EQU; Name = (StrLabelL (Some "labelT")); 
                                                                                    EQUExpr = (Some 12u); DCDValueList = None;
                                                                                    FillN = None}
                    makeTest "EQU" (ldFunc "labelT" "1+2") "EQU Add" {InstructionType = EQU; Name = (StrLabelL (Some "labelT")); 
                                                                                    EQUExpr = (Some 3u); DCDValueList = None;
                                                                                    FillN = None}
                    makeTest "EQU" (ldFunc "labelT" "3-2") "EQU Sub" {InstructionType = EQU; Name = (StrLabelL (Some "labelT")); 
                                                                                    EQUExpr = (Some 1u); DCDValueList = None;
                                                                                    FillN = None}
                    makeTest "EQU" (ldFunc "labelT" "5-4*3-1*1+2*2*2") "EQU All" {InstructionType = EQU; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = (Some 0u); DCDValueList = None;
                                                                                    FillN = None}
                    makeTest "FILL" (ldFunc "labelT" "4") "FILL 4" {InstructionType = FILL; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 4u}
                    makeTest "FILL" (ldFunc "labelT" "64") "FILL 64" {InstructionType = FILL; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 64u}
                    makeTest "FILL" (ldFunc "labelT" "3") "FILL 3" {InstructionType = FILL; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 0u}
                    makeTest "FILL" (ldFunc "labelT" "123") "FILL 123" {InstructionType = FILL; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 0u}
                    makeTest "FILL" (ldFunc "labelT" "0") "FILL 0" {InstructionType = FILL; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 0u}
                    makeTest "FILL" (ldFunc "labelT" "-1") "FILL -1" {InstructionType = FILL; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 0u}
                    makeTest "FILL" (ldFunc "labelT" "-4") "FILL -4" {InstructionType = FILL; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 0u}
                    makeTest "DCD" (ldFunc "labelT" "1") "DCD 1" {InstructionType = DCD; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1"];
                                                                                    FillN = None}
                    makeTest "DCD" (ldFunc "labelT" "1,3,5") "DCD 1,3,5" {InstructionType = DCD; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1";"3";"5"];
                                                                                    FillN = None}
                    makeTest "DCD" (ldFunc "labelT" "1, 3, 5") "DCD 1, 3, 5" {InstructionType = DCD; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1";"3";"5"];
                                                                                    FillN = None}
                    makeTest "DCD" (ldFunc "labelT" "1, -3, 5") "DCD 1, -3, 5" {InstructionType = DCD; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1";"-3";"5"];
                                                                                    FillN = None}
                    makeTest "DCD" (ldFunc "labelT" "-1, 0, 5") "DCD -1, 0, 5" {InstructionType = DCD; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some ["-1";"0";"5"];
                                                                                    FillN = None}
                    makeTest "DCD" (ldFunc "labelT" "") "DCD no input" {InstructionType = DCD; Name = (StrLabelL (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some [""];
                                                                                    FillN = None}
                ]







    [<Tests>]
    let evalExpressionTest =
        let st:SymbolTable = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeevalExpTest name input output =
            testCase name <| fun () ->
                Expect.equal (evalExpression input st) output (sprintf "evalExpression Test '%s'" input)
        Expecto.Tests.testList "evalExpressions Tests"
                [   //parseDCD

                    makeevalExpTest "Mult Only" "1*2*3*4" 24u
                    makeevalExpTest "Add Only" "1+2" 3u
                    makeevalExpTest "Subtract Only" "3-1" 2u
                    makeevalExpTest "All" "1*2*3*4+5*3-2" 37u
                    makeevalExpTest "All2" "5-4*3-1*1+2*2*2" 0u
                    makeevalExpTest "Num Only" "3" 3u
                    makeevalExpTest "Label Only" "testL" 256u
                    makeevalExpTest "Label + 2" "testL + 2" 258u
                    makeevalExpTest "Label Right Multiply" "testL + 2*2" 260u
                    makeevalExpTest "Label Left Multiply" "2*2 + testL" 260u
                    makeevalExpTest "Label Add Hex" "testL + 0x4" 260u
                    makeevalExpTest "Label Add Hex&" "testL + &4" 260u
                    makeevalExpTest "Label Add Bin" "testL + 0b100" 260u
                    makeevalExpTest "Brackets1" "(4*2)+3" 11u
                    makeevalExpTest "Brackets2" "testL + (2*2)" 260u
                    makeevalExpTest "Label Right Left Multiply" "4*2 + testL + 2*2" 268u
                    makeevalExpTest "* first character" "*3+7" 10u
                    makeevalExpTest "+ first character" "+3+7" 10u
                    makeevalExpTest "- first character" "-3+7" 4u
                    // makeevalExpTest "NumLabel Only" "testL2" 260u

                ]


    [<Tests>]
    let parseMemInsTest = 
        let x:SymbolTable = ["a",uint32 2] |> Map.ofList
        let ldFunc Ops = 
                {LoadAddr= WA 100u; 
                    Label= Some "labelT"; 
                    SymTab= Some x;
                    OpCode= "";
                    Operands= Ops}
        let makeTest root suffix ld name output =
            testCase name <| fun () ->
                Expect.equal (parseMemIns root suffix ld) output (sprintf "Parse LDR/STR Tests\nTest: %A" name)
        Expecto.Tests.testList "parseMemIns Tests"
                [   
                    //LDR tests
                    makeTest "LDR" "" (ldFunc "R0, [R1]") "LDR Base Case" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R0; AddressReg= R1;
                                                                    BytesNotWords= false; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "" (ldFunc "R6, [R5, #4]") "LDR Num Increment" (Some{InstructionType= LDR;
                                                                    DestSourceReg= R6; AddressReg= R5;
                                                                    BytesNotWords= false; IncrementValue= 4;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "" (ldFunc "R8, [R7], #40") "LDR Post Increment" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R8; AddressReg= R7;
                                                                    BytesNotWords= false; IncrementValue= 40;
                                                                    PreIndexRb= false; PostIndexRb= true; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "" (ldFunc "R10, [R9, #4]!") "LDR Pre Increment" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R10; AddressReg= R9;
                                                                    BytesNotWords= false; IncrementValue= 4;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "" (ldFunc "R12, [R11, R1]") "LDR Adding Registers" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R12; AddressReg= R11;
                                                                    BytesNotWords= false; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "" (ldFunc "R0, [R11, R1, LSL #1]") "LDR Shifted Register" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= false; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "LDR" "" (ldFunc "R0, [R11, R1, LSL #1]!") "LDR Shifted and Pre" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= false; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "LDR" "" (ldFunc "R0, [R11, R1, LSL #0]!") "LDR 0 Shift and Pre" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= false; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 0;})
                    makeTest "LDR" "" (ldFunc "R0, [R11, R1, LSL #-1]!") "LDR -1 Shift and Pre" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= false; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})


                    //LDR tests with Bytes
                    makeTest "LDR" "B" (ldFunc "R0, [R1]") "LDR Base Case, bytes" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R0; AddressReg= R1;
                                                                    BytesNotWords= true; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "B" (ldFunc "R6, [R5, #4]") "LDR Num Increment, bytes" (Some{InstructionType= LDR;
                                                                    DestSourceReg= R6; AddressReg= R5;
                                                                    BytesNotWords= true; IncrementValue= 4;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "B" (ldFunc "R8, [R7], #40") "LDR Post Increment, bytes" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R8; AddressReg= R7;
                                                                    BytesNotWords= true; IncrementValue= 40;
                                                                    PreIndexRb= false; PostIndexRb= true; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "B" (ldFunc "R10, [R9, #4]!") "LDR Pre Increment, bytes" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R10; AddressReg= R9;
                                                                    BytesNotWords= true; IncrementValue= 4;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "B" (ldFunc "R12, [R11, R1]") "LDR Adding Registers, bytes" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R12; AddressReg= R11;
                                                                    BytesNotWords= true; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "B" (ldFunc "R0, [R11, R1, LSL #1]") "LDR Shifted Register, bytes" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= true; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "LDR" "B" (ldFunc "R0, [R11, R1, LSL #1]!") "LDR Shifted and Pre, bytes" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= true; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "LDR" "B" (ldFunc "R0, [R11, R1, LSL #0]!") "LDR 0 Shift and Pre, bytes" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= true; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 0;})
                    makeTest "LDR" "B" (ldFunc "R0, [R11, R1, LSL #-1]!") "LDR -1 Shift and Pre, bytes" (Some {InstructionType= LDR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= true; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})


                
                    //STR tests
                    makeTest "STR" "" (ldFunc "R0, [R1]") "STR Base Case" (Some {InstructionType= STR;
                                                                    DestSourceReg= R0; AddressReg= R1;
                                                                    BytesNotWords= false; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "" (ldFunc "R6, [R5, #4]") "STR Num Increment" (Some{InstructionType= STR;
                                                                    DestSourceReg= R6; AddressReg= R5;
                                                                    BytesNotWords= false; IncrementValue= 4;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "" (ldFunc "R8, [R7], #40") "STR Post Increment" (Some {InstructionType= STR;
                                                                    DestSourceReg= R8; AddressReg= R7;
                                                                    BytesNotWords= false; IncrementValue= 40;
                                                                    PreIndexRb= false; PostIndexRb= true; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "" (ldFunc "R10, [R9, #4]!") "STR Pre Increment" (Some {InstructionType= STR;
                                                                    DestSourceReg= R10; AddressReg= R9;
                                                                    BytesNotWords= false; IncrementValue= 4;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "" (ldFunc "R12, [R11, R1]") "STR Adding Registers" (Some {InstructionType= STR;
                                                                    DestSourceReg= R12; AddressReg= R11;
                                                                    BytesNotWords= false; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "" (ldFunc "R0, [R11, R1, LSL #1]") "STR Shifted Register" (Some {InstructionType= STR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= false; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "STR" "" (ldFunc "R0, [R11, R1, LSL #1]!") "STR Shifted and Pre" (Some {InstructionType= STR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= false; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "STR" "" (ldFunc "R0, [R11, R1, LSL #0]!") "STR 0 Shift and Pre" (Some {InstructionType= STR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= false; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 0;})
                    makeTest "STR" "" (ldFunc "R0, [R11, R1, LSL #-1]!") "STR -1 Shift and Pre" (Some {InstructionType= STR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= false; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                
                    //STR tests with Bytes
                    makeTest "STR" "B" (ldFunc "R0, [R1]") "STR Base Case, bytes" (Some {InstructionType= STR;
                                                                    DestSourceReg= R0; AddressReg= R1;
                                                                    BytesNotWords= true; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "B" (ldFunc "R6, [R5, #4]") "STR Num Increment, bytes" (Some{InstructionType= STR;
                                                                    DestSourceReg= R6; AddressReg= R5;
                                                                    BytesNotWords= true; IncrementValue= 4;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "B" (ldFunc "R8, [R7], #40") "STR Post Increment, bytes" (Some {InstructionType= STR;
                                                                    DestSourceReg= R8; AddressReg= R7;
                                                                    BytesNotWords= true; IncrementValue= 40;
                                                                    PreIndexRb= false; PostIndexRb= true; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "B" (ldFunc "R10, [R9, #4]!") "STR Pre Increment, bytes" (Some {InstructionType= STR;
                                                                    DestSourceReg= R10; AddressReg= R9;
                                                                    BytesNotWords= true; IncrementValue= 4;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "B" (ldFunc "R12, [R11, R1]") "STR Adding Registers, bytes" (Some {InstructionType= STR;
                                                                    DestSourceReg= R12; AddressReg= R11;
                                                                    BytesNotWords= true; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "B" (ldFunc "R0, [R11, R1, LSL #1]") "STR Shifted Register, bytes" (Some {InstructionType= STR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= true; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "STR" "B" (ldFunc "R0, [R11, R1, LSL #1]!") "STR Shifted and Pre, bytes" (Some {InstructionType= STR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= true; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "STR" "B" (ldFunc "R0, [R11, R1, LSL #0]!") "STR 0 Shift and Pre, bytes" (Some {InstructionType= STR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= true; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 0;})
                    makeTest "STR" "B" (ldFunc "R0, [R11, R1, LSL #-1]!") "STR -1 Shift and Pre, bytes" (Some {InstructionType= STR;
                                                                    DestSourceReg= R0; AddressReg= R11;
                                                                    BytesNotWords= true; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                ]



    // let Mem1 = DataLoc 2u
    // let Addr1 = WA 256u
    // let (MachMem1: MachineMemory<'INS>) = (Map.empty).Add(Addr1,Mem1) 

    // let (symTab: SymbolTable) = (Map.empty).Add("testLabel",256u)


    // let parseAdrInsTest = 
    //     let st:SymbolTable = ["testL",256u; "testL2",260u] |> Map.ofList
    //     let ldFunc symTab Ops = 
    //             {LoadAddr= WA 100u; 
    //                 Label= Some "labelT"; 
    //                 SymTab= Some symTab;
    //                 OpCode= "";
    //                 Operands= Ops}
    //     let makeTest root ld name output =
    //         testCase name <| fun () ->
    //             Expect.equal (parseAdrIns root ld) output (sprintf "Parse ADR Tests\nTest: %A" name)
    //     Expecto.Tests.testList "parseAdrIns Tests"
    //             [   
    //                 //LDR tests
    //                 makeTest "ADR" (ldFunc st "R0, testL") "ADR Base Case" {InstructionType= Some ADRm;
    //                                                                             DestSourceReg= Some R0;
    //                                                                             SecondOp= 256u;}

    //             ]




    // let test = ((Map.add ("blah":string) (5u:uint32)): SymbolTable)  
    // let test2 = Map<"blah", 97u>
    // let x:SymbolTable = ["a",uint32 2] |> Map.ofList

    // let ld = {LoadAddr= WA 100u; 
    //             Label= Some "testlabel"; 
    //             SymTab= Some x;
    //             OpCode= "AL";
    //             Operands= "1,3,5"}

    // let test = parse ld

        // let parseLabelIns root ls
        // let InstTypeTmp = 
        //     match root with
        //     | "EQU" -> Some EQU 
        //     | "Fill" -> Some Fill
        //     | "DCD" -> Some DCD

        // ({InstructionType = InstTypeTmp; 
        //     Name = ls.Label; 
        //     EquExpr = ; 
        //     DCDValueList = ; 
        //     FillN = }: 'INS)