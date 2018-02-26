//A series of tests for all the functions in memInstructions
module memInstructionsTests
    open CommonData
    open CommonLex
    open Expecto
    open Expecto.ExpectoFsCheck
    open MemInstructions
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
                    //EQU Working Test
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
                    //EQU Error Message Tests
                    
                    //Fill Working Tests
                    makeTest "FILL" (ldFunc "labelT" "4") "FILL 4" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some (Ok 4u)})
                    makeTest "FILL" (ldFunc "labelT" "64") "FILL 64" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some (Ok 64u)})
                    makeTest "FILL" (ldFunc "labelT" "0") "FILL 0" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some (Ok 0u)})
                    //Fill Error Message Tests
                    makeTest "FILL" (ldFunc "labelT" "3") "FILL 3" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    // FillN = Some (Error "parseLabelIns: Fill expression (3u) does not evaluate to something which is a positive multiple of four")})
                                                                                    FillN = Some (Error "parseLabelIns: Fill expression (3u) >0 and divisible by four")})
                    makeTest "FILL" (ldFunc "labelT" "123") "FILL 123" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some (Error "parseLabelIns: Fill expression (123u) >0 and divisible by four")})
                    makeTest "FILL" (ldFunc "labelT" "-1") "FILL -1" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some (Error "parseLabelIns: Fill expression (4294967295u) >0 and divisible by four")})
                    makeTest "FILL" (ldFunc "labelT" "-4") "FILL -4" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some (Error "parseLabelIns: Fill expression (4294967292u) >0 and divisible by four")})
                    
                    //DCD Working Tests
                    makeTest "DCD" (ldFunc "labelT" "1") "DCD 1" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1"];
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "1,3,5") "DCD 1,3,5" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1";"3";"5"];
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "1, 3, 5") "DCD 1, 3, 5" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1";"3";"5"];
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "  1, 3, 5  ") "DCD   1, 3, 5  " (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
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
                    //DCD Error Message Tests
                
                
                ]







    [<Tests>]
    let evalExpressionTest =
        let st:SymbolTable = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeEvalExpTest name input labels output =
            testCase name <| fun () ->
                Expect.equal (evalExpression input st labels) output (sprintf "evalExpression Test '%s'" input)
        Expecto.Tests.testList "evalExpressions Tests"
                [   
                    //evalExpression Working Tests
                    makeEvalExpTest "Mult Only" "1*2*3*4" true (Ok 24u)
                    makeEvalExpTest "Add Only" "1+2" true (Ok 3u)
                    makeEvalExpTest "Subtract Only" "3-1" true (Ok 2u)
                    makeEvalExpTest "All" "1*2*3*4+5*3-2" true (Ok 37u)
                    makeEvalExpTest "All2" "5-4*3-1*1+2*2*2" true (Ok 0u)
                    makeEvalExpTest "Num Only" "3" true (Ok 3u)
                    makeEvalExpTest "Label Only" "testL" true (Ok 256u)
                    makeEvalExpTest "Label + 2" "testL + 2" true (Ok 258u)
                    makeEvalExpTest "Label Right Multiply" "testL + 2*2" true (Ok 260u)
                    makeEvalExpTest "Label Left Multiply" "2*2 + testL" true (Ok 260u)
                    makeEvalExpTest "Label Add Hex" "testL + 0x4" true (Ok 260u)
                    makeEvalExpTest "Label Add Hex&" "testL + &4" true (Ok 260u)
                    makeEvalExpTest "Label Add Bin" "testL + 0b100" true (Ok 260u)
                    makeEvalExpTest "Brackets1" "(4*2)+3" true (Ok 11u)
                    makeEvalExpTest "Brackets2" "testL + (2*2)" true (Ok 260u)
                    makeEvalExpTest "Label Right Left Multiply" "4*2 + testL + 2*2" true (Ok 268u)
                    makeEvalExpTest "* first character" "*3+7" true (Ok 10u)
                    makeEvalExpTest "+ first character" "+3+7" true (Ok 10u)
                    makeEvalExpTest "- first character" "-3+7" true (Ok 4u)
                    makeEvalExpTest "Negative Output" "3-7" true (Ok 4294967292u)
                    // makeEvalExpTest "Brackets test" "2*(6+(3*4)-(6+3))*5" 90u
                    // makeEvalExpTest "NumLabel Only" "testL2" 260u

                
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
                    makeTest "ADR" (ldFunc st "R0, 4") "ADR Number Only Exp" (Ok {InstructionType= Ok ADRm;
                                                                                DestReg= Ok R0;
                                                                                SecondOp= Ok 4u;})


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




    [<Tests>]
    let checkParse = 
        let st:SymbolTable = ["testL",256u; "testL2",260u] |> Map.ofList
        let ldFunc symTab opCodeIn ops = 
                {LoadAddr= WA 100u; 
                    Label= Some "labelTest"; 
                    SymTab= Some symTab;
                    OpCode= opCodeIn;
                    Operands= ops}
        let ldFuncAll wAd lab symT opCodeIn ops = 
                    {LoadAddr= WA wAd; 
                    Label= Some lab; 
                    SymTab= Some symT;
                    OpCode= opCodeIn;
                    Operands= ops}
        let makeTest ls name output =
            testCase name <| fun () ->
                Expect.equal (parse ls) output (sprintf "parse Tests\nTest: %A" name)
        Expecto.Tests.testList "Total Parse Tests"
                [   
                    //parse Working Tests
                    //ADR
                    makeTest (ldFunc st "ADR" "R0, 4") "Base Test" (Some (Ok {PInstr = AdrO (Ok {InstructionType= Ok ADRm;
                                                                                DestReg= Ok R0;
                                                                                SecondOp= Ok 4u;});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))
                    makeTest (ldFunc st "ADR" "R9, testL") "parse ADR Label" (Some (Ok {PInstr = AdrO (Ok {InstructionType= Ok ADRm;
                                                                                DestReg= Ok R9;
                                                                                SecondOp= Ok 256u;});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))
                    makeTest (ldFunc st "ADR" "R15, testL") "parse ADR R15" (Some (Ok {PInstr = AdrO (Ok {InstructionType= Ok ADRm;
                                                                                DestReg= Ok R15;
                                                                                SecondOp= Ok 256u;});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))
                    //DCD
                    makeTest (ldFunc st "DCD" "1") "Base DCD Test" (Some (Ok {PInstr = LabelO (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelTest"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1"];
                                                                                    FillN = None});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))
                    makeTest (ldFunc st "DCD" "1,3,5") "DCD 1,3,5 Test" (Some (Ok {PInstr = LabelO (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelTest"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1";"3";"5"];
                                                                                    FillN = None});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 12u; PCond = Cal}))
                    makeTest (ldFuncAll 200u "labelTestTwo" st "DCD" "1,3,5") "DCD Word Address Test" (Some (Ok {PInstr = LabelO (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelTestTwo"));
                                                                                    EQUExpr = None; DCDValueList = Some ["1";"3";"5"];
                                                                                    FillN = None});
                                                                    PLabel = Some ("labelTestTwo", 200u);
                                                                    PSize = 12u; PCond = Cal}))
                    //EQU
                    makeTest (ldFuncAll 100u "labelTest" st "EQU" "2") "EQU Base Case" (Some (Ok {PInstr = LabelO (Ok {InstructionType = Ok EQU; Name = (Ok (Some "labelTest"));
                                                                                    EQUExpr = (Some (Ok 2u)); DCDValueList = None;
                                                                                    FillN = None});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 0u; PCond = Cal}))
                    makeTest (ldFuncAll 100u "labelTest" st "EQU" "5-4*3-1*1+2*2*2") "EQU +-* Eg Case" (Some (Ok {PInstr = LabelO (Ok {InstructionType = Ok EQU; Name = (Ok (Some "labelTest"));
                                                                                    EQUExpr = (Some (Ok 0u)); DCDValueList = None;
                                                                                    FillN = None});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 0u; PCond = Cal}))
                    //Fill
                    makeTest (ldFuncAll 100u "labelTest" st "FILL" "4") "Fill Base Case" (Some (Ok {PInstr = LabelO (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelTest"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some (Ok 4u)});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))
                    makeTest (ldFuncAll 100u "labelTest" st "FILL" "-1") "Fill Error Case" (Some (Ok {PInstr = LabelO (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelTest"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some (Error "parseLabelIns: Fill expression (4294967295u) >0 and divisible by four")});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))

                ]



    [<Tests>]
    //updateSymbolTable (symbolTab: SymbolTable) (inputRecord: labelInstr) field
    let checkUpdateSymbolTable = 
        let st:SymbolTable = ["testL",256u; "testL2",260u] |> Map.ofList
        let (stOneItem: SymbolTable) = ["testL",256u] |> Map.ofList
        //Record for labelTest EQU 2
        let baseInpRec = (Some (Ok {PInstr = LabelO (Ok {InstructionType = Ok EQU; Name = (Ok (Some "labelTest"));
                            EQUExpr = (Some (Ok 2u)); DCDValueList = None;
                            FillN = None});
                            PLabel = Some ("labelTest", 100u);
                            PSize = 0u; PCond = Cal}))
        let ldFuncAll wAd lab symT opCodeIn ops = 
                    {LoadAddr= WA wAd; 
                    Label= Some lab; 
                    SymTab= Some symT;
                    OpCode= opCodeIn;
                    Operands= ops}
        let baseEQU2 =  parse (ldFuncAll 100u "labelT" st "EQU" "2")
        let baseEQU2' = ({InstructionType = Ok EQU; Name = (Ok (Some "labelT")); 
                            EQUExpr = (Some (Ok 2u)); DCDValueList = None;
                            FillN = None})
        let makeTest symTab inpRec field name output =
            testCase name <| fun () ->
                Expect.equal (updateSymbolTable symTab inpRec field) output (sprintf "checkLiteral Tests\nTest: %A" name)
        Expecto.Tests.testList "checkLiteralMonad Tests"
                [   
                    //ADR Working tests
                    makeTest stOneItem baseEQU2' (baseEQU2'.EQUExpr) "updateSymbolTable Base Test" (["testL",256u; "labelT",2u] |> Map.ofList |> Ok)
                ]

//Functions to test
//  - updatedMemoryDataPath
//  - updatedSymbolTable
//  - DCDexec
//  - EQUexec
//  - FILLexec
