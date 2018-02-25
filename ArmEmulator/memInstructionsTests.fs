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
                    //EQU, DCD and FILL Working Tests
                    makeTest "EQU" (ldFunc "labelT" "4") "EQU1" (Ok {InstructionType = Ok EQU; Name = (Ok (Some "labelT")); 
                                                        EQUExpr = (Some (Ok 4u)); DCDValueList = None;
                                                        FillN = None})
                    makeTest "EQU" (ldFunc "labelT" "2") "EQU2" (Ok {InstructionType = Ok EQU; Name = (Ok (Some "labelT")); 
                                                                            EQUExpr = (Some (Ok 2u)); DCDValueList = None;
                                                                            FillN = None})
                    makeTest "EQU" (ldFunc "labelT" "3*4") "EQU Mult" (Ok {InstructionType = Ok EQU; Name = (Ok (Some "labelT")); 
                                                                                    EQUExpr = (Some (Ok 12u)); DCDValueList = None;
                                                                                    FillN = None})
                    makeTest "EQU" (ldFunc "labelT" "1+2") "EQU Add" (Ok {InstructionType = Ok EQU; Name = (Ok (Some "labelT")); 
                                                                                    EQUExpr = (Some (Ok 3u)); DCDValueList = None;
                                                                                    FillN = None})
                    makeTest "EQU" (ldFunc "labelT" "3-2") "EQU Sub" (Ok {InstructionType = Ok EQU; Name = (Ok (Some "labelT")); 
                                                                                    EQUExpr = (Some (Ok 1u)); DCDValueList = None;
                                                                                    FillN = None})
                    makeTest "EQU" (ldFunc "labelT" "5-4*3-1*1+2*2*2") "EQU All" (Ok {InstructionType = Ok EQU; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = (Some (Ok 0u)); DCDValueList = None;
                                                                                    FillN = None})
                    makeTest "FILL" (ldFunc "labelT" "4") "FILL 4" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 4u})
                    makeTest "FILL" (ldFunc "labelT" "64") "FILL 64" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 64u})
                    makeTest "FILL" (ldFunc "labelT" "3") "FILL 3" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 0u})
                    makeTest "FILL" (ldFunc "labelT" "123") "FILL 123" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 0u})
                    makeTest "FILL" (ldFunc "labelT" "0") "FILL 0" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 0u})
                    makeTest "FILL" (ldFunc "labelT" "-1") "FILL -1" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 0u})
                    makeTest "FILL" (ldFunc "labelT" "-4") "FILL -4" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some 0u})
                    makeTest "DCD" (ldFunc "labelT" "1") "DCD 1" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1"];
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "1,3,5") "DCD 1,3,5" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1";"3";"5"];
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "1, 3, 5") "DCD 1, 3, 5" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1";"3";"5"];
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "1, -3, 5") "DCD 1, -3, 5" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1";"-3";"5"];
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "-1, 0, 5") "DCD -1, 0, 5" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some ["-1";"0";"5"];
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "") "DCD no input" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some [""];
                                                                                    FillN = None})
                
                
                    //EQU, DCD and FILL Error Message Tests
                
                ]







    [<Tests>]
    let evalExpressionTest =
        let st:SymbolTable = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeevalExpTest name input output =
            testCase name <| fun () ->
                Expect.equal (evalExpression input st) output (sprintf "evalExpression Test '%s'" input)
        Expecto.Tests.testList "evalExpressions Tests"
                [   
                    //evalExpression Working Tests
                    makeevalExpTest "Mult Only" "1*2*3*4" (Ok 24u)
                    makeevalExpTest "Add Only" "1+2" (Ok 3u)
                    makeevalExpTest "Subtract Only" "3-1" (Ok 2u)
                    makeevalExpTest "All" "1*2*3*4+5*3-2" (Ok 37u)
                    makeevalExpTest "All2" "5-4*3-1*1+2*2*2" (Ok 0u)
                    makeevalExpTest "Num Only" "3" (Ok 3u)
                    makeevalExpTest "Label Only" "testL" (Ok 256u)
                    makeevalExpTest "Label + 2" "testL + 2" (Ok 258u)
                    makeevalExpTest "Label Right Multiply" "testL + 2*2" (Ok 260u)
                    makeevalExpTest "Label Left Multiply" "2*2 + testL" (Ok 260u)
                    makeevalExpTest "Label Add Hex" "testL + 0x4" (Ok 260u)
                    makeevalExpTest "Label Add Hex&" "testL + &4" (Ok 260u)
                    makeevalExpTest "Label Add Bin" "testL + 0b100" (Ok 260u)
                    makeevalExpTest "Brackets1" "(4*2)+3" (Ok 11u)
                    makeevalExpTest "Brackets2" "testL + (2*2)" (Ok 260u)
                    makeevalExpTest "Label Right Left Multiply" "4*2 + testL + 2*2" (Ok 268u)
                    makeevalExpTest "* first character" "*3+7" (Ok 10u)
                    makeevalExpTest "+ first character" "+3+7" (Ok 10u)
                    makeevalExpTest "- first character" "-3+7" (Ok 4u)
                    makeevalExpTest "Negative Output" "3-7" (Ok 4294967292u)
                    // makeevalExpTest "Brackets test" "2*(6+(3*4)-(6+3))*5" 90u
                    // makeevalExpTest "NumLabel Only" "testL2" 260u

                
                    //evalExpression Error Message Tests
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
                    //LDR & STR working tests
                    //LDR tests
                    makeTest "LDR" "" (ldFunc "R0, [R1]") "LDR Base Case" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R1;
                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "" (ldFunc "R6, [R5, #4]") "LDR Num Increment" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R6; AddressReg= Ok R5;
                                                                    BytesNotWords= Ok false; IncrementValue= 4;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "" (ldFunc "R8, [R7], #40") "LDR Post Increment" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R8; AddressReg= Ok R7;
                                                                    BytesNotWords= Ok false; IncrementValue= 40;
                                                                    PreIndexRb= false; PostIndexRb= true; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "" (ldFunc "R10, [R9, #4]!") "LDR Pre Increment" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R10; AddressReg= Ok R9;
                                                                    BytesNotWords= Ok false; IncrementValue= 4;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "" (ldFunc "R12, [R11, R1]") "LDR Adding Registers" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R12; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "" (ldFunc "R0, [R11, R1, LSL #1]") "LDR Shifted Register" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "LDR" "" (ldFunc "R0, [R11, R1, LSL #1]!") "LDR Shifted and Pre" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "LDR" "" (ldFunc "R0, [R11, R1, LSL #0]!") "LDR 0 Shift and Pre" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 0;})
                    makeTest "LDR" "" (ldFunc "R0, [R11, R1, LSL #-1]!") "LDR -1 Shift and Pre" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})


                    //LDR tests with Bytes
                    makeTest "LDR" "B" (ldFunc "R0, [R1]") "LDR Base Case, bytes" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R1;
                                                                    BytesNotWords= Ok true; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "B" (ldFunc "R6, [R5, #4]") "LDR Num Increment, bytes" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R6; AddressReg= Ok R5;
                                                                    BytesNotWords= Ok true; IncrementValue= 4;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "B" (ldFunc "R8, [R7], #40") "LDR Post Increment, bytes" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R8; AddressReg= Ok R7;
                                                                    BytesNotWords= Ok true; IncrementValue= 40;
                                                                    PreIndexRb= false; PostIndexRb= true; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "B" (ldFunc "R10, [R9, #4]!") "LDR Pre Increment, bytes" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R10; AddressReg= Ok R9;
                                                                    BytesNotWords= Ok true; IncrementValue= 4;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "B" (ldFunc "R12, [R11, R1]") "LDR Adding Registers, bytes" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R12; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok true; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "LDR" "B" (ldFunc "R0, [R11, R1, LSL #1]") "LDR Shifted Register, bytes" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok true; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "LDR" "B" (ldFunc "R0, [R11, R1, LSL #1]!") "LDR Shifted and Pre, bytes" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok true; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "LDR" "B" (ldFunc "R0, [R11, R1, LSL #0]!") "LDR 0 Shift and Pre, bytes" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok true; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 0;})
                    makeTest "LDR" "B" (ldFunc "R0, [R11, R1, LSL #-1]!") "LDR -1 Shift and Pre, bytes" (Ok {InstructionType= Ok LDR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok true; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})


                
                    //STR tests
                    makeTest "STR" "" (ldFunc "R0, [R1]") "STR Base Case" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R1;
                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "" (ldFunc "R6, [R5, #4]") "STR Num Increment" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R6; AddressReg= Ok R5;
                                                                    BytesNotWords= Ok false; IncrementValue= 4;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "" (ldFunc "R8, [R7], #40") "STR Post Increment" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R8; AddressReg= Ok R7;
                                                                    BytesNotWords= Ok false; IncrementValue= 40;
                                                                    PreIndexRb= false; PostIndexRb= true; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "" (ldFunc "R10, [R9, #4]!") "STR Pre Increment" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R10; AddressReg= Ok R9;
                                                                    BytesNotWords= Ok false; IncrementValue= 4;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "" (ldFunc "R12, [R11, R1]") "STR Adding Registers" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R12; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "" (ldFunc "R0, [R11, R1, LSL #1]") "STR Shifted Register" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "STR" "" (ldFunc "R0, [R11, R1, LSL #1]!") "STR Shifted and Pre" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "STR" "" (ldFunc "R0, [R11, R1, LSL #0]!") "STR 0 Shift and Pre" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 0;})
                    makeTest "STR" "" (ldFunc "R0, [R11, R1, LSL #-1]!") "STR -1 Shift and Pre" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                
                    //STR tests with Bytes
                    makeTest "STR" "B" (ldFunc "R0, [R1]") "STR Base Case, bytes" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R1;
                                                                    BytesNotWords= Ok true; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "B" (ldFunc "R6, [R5, #4]") "STR Num Increment, bytes" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R6; AddressReg= Ok R5;
                                                                    BytesNotWords= Ok true; IncrementValue= 4;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "B" (ldFunc "R8, [R7], #40") "STR Post Increment, bytes" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R8; AddressReg= Ok R7;
                                                                    BytesNotWords= Ok true; IncrementValue= 40;
                                                                    PreIndexRb= false; PostIndexRb= true; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "B" (ldFunc "R10, [R9, #4]!") "STR Pre Increment, bytes" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R10; AddressReg= Ok R9;
                                                                    BytesNotWords= Ok true; IncrementValue= 4;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "B" (ldFunc "R12, [R11, R1]") "STR Adding Registers, bytes" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R12; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok true; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= None;})
                    makeTest "STR" "B" (ldFunc "R0, [R11, R1, LSL #1]") "STR Shifted Register, bytes" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok true; IncrementValue= 0;
                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "STR" "B" (ldFunc "R0, [R11, R1, LSL #1]!") "STR Shifted and Pre, bytes" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok true; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 1;})
                    makeTest "STR" "B" (ldFunc "R0, [R11, R1, LSL #0]!") "STR 0 Shift and Pre, bytes" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok true; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= Some R1;
                                                                    ShiftExtraRegBy= Some 0;})
                    makeTest "STR" "B" (ldFunc "R0, [R11, R1, LSL #-1]!") "STR -1 Shift and Pre, bytes" (Ok {InstructionType= Ok STR;
                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                    BytesNotWords= Ok true; IncrementValue= 0;
                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;})
                
                
                
                    //LDR & STR Error Message Tests
                ]


    [<Tests>]
    let parseAdrInsTest = 
        let st:SymbolTable = ["testL",256u; "testL2",260u] |> Map.ofList
        let ldFunc symTab Ops = 
                {LoadAddr= WA 100u; 
                    Label= Some "labelT"; 
                    SymTab= Some symTab;
                    OpCode= "";
                    Operands= Ops}
        let makeTest root ld name output =
            testCase name <| fun () ->
                Expect.equal (parseAdrIns root ld) output (sprintf "Parse ADR Tests\nTest: %A" name)
        Expecto.Tests.testList "parseAdrIns Tests"
                [   
                    //ADR Working tests
                    makeTest "ADR" (ldFunc st "R0, testL") "ADR Base Case" (Ok {InstructionType= Ok ADRm;
                                                                                DestReg= Ok R0;
                                                                                SecondOp= Ok 256u;})
                    makeTest "ADR" (ldFunc st "R9, testL") "ADR R9 Test" (Ok {InstructionType= Ok ADRm;
                                                                                DestReg= Ok R9;
                                                                                SecondOp= Ok 256u;})
                    makeTest "ADR" (ldFunc st "R10, testL") "ADR R10 Test" (Ok {InstructionType= Ok ADRm;
                                                                                DestReg= Ok R10;
                                                                                SecondOp= Ok 256u;})
                    makeTest "ADR" (ldFunc st "R15, testL") "ADR R15 Test" (Ok {InstructionType= Ok ADRm;
                                                                                DestReg= Ok R15;
                                                                                SecondOp= Ok 256u;})


                    //ADR Error Message Tests
                ]






    [<Tests>]
    let checkLiteralMonadTests = 
        let makeTest inp name output =
            testCase name <| fun () ->
                Expect.equal (checkLiteralMonad inp) output (sprintf "checkLiteral Tests\nTest: %A" name)
        Expecto.Tests.testList "checkLiteralMonad Tests"
                [   
                    //ADR Working tests
                    makeTest 2u "Base Test" (Ok 2u)
                    makeTest 4u "Base Test2" (Ok 4u)
                    makeTest 255u "8 bits positive" (Ok 255u)
                    makeTest 2u "Base Test Again" (Ok 2u)
                    // makeTest (-127|>uint32) "Negative 1" (Ok (-127|>uint32))
                    // makeTest (-240|>uint32) "Negative 2" (Ok (-240|>uint32))
                    // makeTest CCCCCCCCu "Base Test" Error
                    // makeTest (-257|>uint32) "Base Test" Error
                    // makeTest 510u "Base Test" Error
                    // makeTest 511u "Should Fail Test" Error
                    // makeTest (-257|>uint32) "Should Fail Test" (Error "Expression result (4294967039u) cannot be made by a rotated 8 bit number")


                ]





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