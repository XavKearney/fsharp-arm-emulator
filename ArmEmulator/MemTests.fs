//A series of tests for all the functions in Mem module
module MemTests
    open CommonData
    open CommonLex
    open Expecto
    open Mem




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
                                                                                    FillN = Some (Error "parseLabelIns: Fill expression (3u) <0 or not divisible by four")})
                    makeTest "FILL" (ldFunc "labelT" "123") "FILL 123" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some (Error "parseLabelIns: Fill expression (123u) <0 or not divisible by four")})
                    makeTest "FILL" (ldFunc "labelT" "-1") "FILL -1" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some (Error "parseLabelIns: Fill expression (4294967295u) <0 or not divisible by four")})
                    makeTest "FILL" (ldFunc "labelT" "-4") "FILL -4" (Ok {InstructionType = Ok FILL; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = None;
                                                                                    FillN = Some (Error "parseLabelIns: Fill expression (4294967292u) <0 or not divisible by four")})
                    
                    //DCD Working Tests
                    makeTest "DCD" (ldFunc "labelT" "1") "DCD 1" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some (Ok ["1"]);
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "1,3,5") "DCD 1,3,5" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some (Ok ["1";"3";"5"]);
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "1, 3, 5") "DCD 1, 3, 5" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some (Ok ["1";"3";"5"]);
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "  1, 3, 5  ") "DCD   1, 3, 5  " (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some (Ok ["1";"3";"5"]);
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "1, -3, 5") "DCD 1, -3, 5" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some (Ok ["1";"-3";"5"]);
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "-1, 0, 5") "DCD -1, 0, 5" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some (Ok ["-1";"0";"5"]);
                                                                                    FillN = None})
                    //DCD Error Message Tests
                    makeTest "DCD" (ldFunc "labelT" "") "DCD no input" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some (Error "parseLabelIns: Input to DCD function not valid (No input etc)");
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "a") "DCD invalid input" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some (Error "parseLabelIns: Input to DCD function not valid (No input etc)");
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "1, ,5") "DCD invalid space list input" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some (Error "parseLabelIns: Input to DCD function not valid (No input etc)");
                                                                                    FillN = None})
                    makeTest "DCD" (ldFunc "labelT" "1, a, 5") "DCD invalid list input" (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelT"));
                                                                                    EQUExpr = None; DCDValueList = Some (Error "parseLabelIns: Input to DCD function not valid (No input etc)");
                                                                                    FillN = None})


                
                ]







    [<Tests>]
    let evalExpressionTest =
        let st:SymbolTable = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeEvalExpTest name input output =
            testCase name <| fun () ->
                Expect.equal (evalExpression input st) output (sprintf "evalExpression Test '%s'" input)
        Expecto.Tests.testList "evalExpressions Tests"
                [   
                    //evalExpression Working Tests
                    makeEvalExpTest "evalExpressions: Mult Only" "1*2*3*4" (Ok 24u)
                    makeEvalExpTest "evalExpressions: Add Only" "1+2" (Ok 3u)
                    makeEvalExpTest "evalExpressions: Subtract Only" "3-1" (Ok 2u)
                    makeEvalExpTest "evalExpressions: All" "1*2*3*4+5*3-2" (Ok 37u)
                    makeEvalExpTest "evalExpressions: All2" "5-4*3-1*1+2*2*2" (Ok 0u)
                    makeEvalExpTest "evalExpressions: Num Only" "3" (Ok 3u)
                    makeEvalExpTest "evalExpressions: Label Only" "testL" (Ok 256u)
                    makeEvalExpTest "evalExpressions: Label + 2" "testL + 2" (Ok 258u)
                    makeEvalExpTest "evalExpressions: Label Right Multiply" "testL + 2*2" (Ok 260u)
                    makeEvalExpTest "evalExpressions: Label Left Multiply" "2*2 + testL" (Ok 260u)
                    makeEvalExpTest "evalExpressions: Label Add Hex" "testL + 0x4" (Ok 260u)
                    makeEvalExpTest "evalExpressions: Label Add Hex&" "testL + &4" (Ok 260u)
                    makeEvalExpTest "evalExpressions: Label Add Bin" "testL + 0b100" (Ok 260u)
                    makeEvalExpTest "evalExpressions: Brackets1" "(4*2)+3" (Ok 11u)
                    makeEvalExpTest "evalExpressions: Brackets2" "testL + (2*2)" (Ok 260u)
                    makeEvalExpTest "evalExpressions: Label Right Left Multiply" "4*2 + testL + 2*2" (Ok 268u)
                    makeEvalExpTest "evalExpressions: * first character" "*3+7" (Ok 10u)
                    makeEvalExpTest "evalExpressions: + first character" "+3+7" (Ok 10u)
                    makeEvalExpTest "evalExpressions: - first character" "-3+7" (Ok 4u)
                    makeEvalExpTest "evalExpressions: Negative Output" "3-7" (Ok 4294967292u)
                    makeEvalExpTest "evalExpressions: NumLabel Only" "testL2" (Ok 260u)
                    // makeEvalExpTest "Brackets test" "2*(6+(3*4)-(6+3))*5" 90u

                
                    //evalExpression Error Message Tests
                    makeEvalExpTest "No Input" "" (Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)")

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
                    makeTest "ADR" (ldFunc st "R2, testL2") "ADR NumLabel Test" (Ok {InstructionType= Ok ADRm;
                                                                                DestReg= Ok R2;
                                                                                SecondOp= Ok 260u;})
                    makeTest "ADR" (ldFunc st "R0, 4") "ADR Number Only Exp" (Ok {InstructionType= Ok ADRm;
                                                                                DestReg= Ok R0;
                                                                                SecondOp= Ok 4u;})
                    //ADR Error Message Tests
                    makeTest "ADR" (ldFunc st "R0, ") "ADR No Input" (Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)" )
                    makeTest "ADR" (ldFunc st ", 4") "ADR No Register" (Error "parseAdrIns: Line Data in incorrect form\nls.Operands: \", 4\"\nparseAdrIns: No destination register identified in parseAdrIns\nls.Operands: \", 4\"")


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
                                                                                    EQUExpr = None; DCDValueList = Some (Ok ["1"]);
                                                                                    FillN = None});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))
                    makeTest (ldFunc st "DCD" "1,3,5") "DCD 1,3,5 Test" (Some (Ok {PInstr = LabelO (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelTest"));
                                                                                    EQUExpr = None; DCDValueList = Some (Ok ["1";"3";"5"]);
                                                                                    FillN = None});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 12u; PCond = Cal}))
                    makeTest (ldFuncAll 200u "labelTestTwo" st "DCD" "1,3,5") "DCD Word Address Test" (Some (Ok {PInstr = LabelO (Ok {InstructionType = Ok DCD; Name = (Ok (Some "labelTestTwo"));
                                                                                    EQUExpr = None; DCDValueList = Some (Ok ["1";"3";"5"]);
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
                                                                                    FillN = Some (Error "parseLabelIns: Fill expression (4294967295u) <0 or not divisible by four")});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))

                ]



    [<Tests>]
    //updateSymbolTable (symbolTab: SymbolTable) (inputRecord: labelInstr) field
    let checkUpdateSymbolTable = 
        let (stOneItem: SymbolTable) = ["testL",256u] |> Map.ofList
        //Record for labelTest EQU 2
        let ldFuncAll wAd lab symT opCodeIn ops = 
                    {LoadAddr= WA wAd; 
                    Label= Some lab; 
                    SymTab= Some symT;
                    OpCode= opCodeIn;
                    Operands= ops}

        let baseEquFunc lab valO = ({InstructionType = Ok EQU; Name = (Ok (Some lab)); 
                                    EQUExpr = (Some (Ok valO)); DCDValueList = None;
                                    FillN = None})
        let makeLabelInstr label root input =
            let ldFuncEQU Lab Ops = 
                    {LoadAddr= WA 100u; 
                        Label= Some Lab; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= Ops}
            let removeRecord x =
                match x with
                | Ok y -> y 
            ldFuncEQU label input
            |> parseLabelIns root
            |> removeRecord 
        let makeTest symTab inpRec field name output =
            testCase name <| fun () ->
                Expect.equal (updateSymbolTable symTab inpRec field) output (sprintf "checkUpdateSymbolTable Tests\nTest: %A" name)
        Expecto.Tests.testList "checkUpdateSymbolTable Tests"
                [   
                    //EQU Working tests
                    makeTest stOneItem ((baseEquFunc "labelT" 2u)) ((baseEquFunc "labelT" 2u).EQUExpr) "updateSymbolTable EQU 2 Base Test" (["testL",256u; "labelT",2u] |> Map.ofList |> Ok)
                    makeTest stOneItem ((baseEquFunc "labelT" 4u)) ((baseEquFunc "labelT" 4u).EQUExpr) "updateSymbolTable EQU 4 Base Test" (["testL",256u; "labelT",4u] |> Map.ofList |> Ok)
                    makeTest stOneItem (makeLabelInstr "labelT" "EQU" "2") ((makeLabelInstr "labelT" "EQU" "2").EQUExpr) "updateSymbolTable EQU parseLabelIns generation Test" (["testL",256u; "labelT",2u] |> Map.ofList |> Ok)
                    makeTest stOneItem (makeLabelInstr "labelT2" "EQU" "5-4*3-1*1+2*2*2") ((makeLabelInstr "labelT" "EQU" "5-4*3-1*1+2*2*2").EQUExpr) "updateSymbolTable EQU +*- Test" (["testL",256u; "labelT2",0u] |> Map.ofList |> Ok)
                    //EQU Error Message tests
                    makeTest stOneItem (makeLabelInstr "labelT2" "EQU" "") ((makeLabelInstr "labelT" "EQU" "").EQUExpr) "updateSymbolTable EQU Invalid Input Test" (Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)")


                    //FILL Working tests
                    makeTest stOneItem (makeLabelInstr "labelT" "FILL" "4") ((makeLabelInstr "labelT" "FILL" "4").FillN) "updateSymbolTable FILL Base Case" (["testL",256u; "labelT",0u] |> Map.ofList |> Ok)
                    makeTest stOneItem (makeLabelInstr "labelT" "FILL" "4*3") ((makeLabelInstr "labelT" "FILL" "4*3").FillN) "updateSymbolTable FILL Mult Case" (["testL",256u; "labelT",0u] |> Map.ofList |> Ok)
                    makeTest stOneItem (makeLabelInstr "labelT" "FILL" "0") ((makeLabelInstr "labelT" "FILL" "0").FillN) "updateSymbolTable FILL Zero Case" (["testL",256u; "labelT",0u] |> Map.ofList |> Ok)
                    //Fill Error Message Tests
                    makeTest stOneItem (makeLabelInstr "labelT" "FILL" "-4") ((makeLabelInstr "labelT" "FILL" "-4").FillN) "updateSymbolTable FILL -4 Case" (Error "parseLabelIns: Fill expression (4294967292u) <0 or not divisible by four" )

                    //DCD Working tests
                    makeTest stOneItem (makeLabelInstr "labelT" "DCD" "1") (removeOptionD ((makeLabelInstr "labelT" "DCD" "1").DCDValueList)) "updateSymbolTable DCD Base Case" (["testL",256u; "labelT",1u] |> Map.ofList |> Ok)
                    makeTest stOneItem (makeLabelInstr "labelT" "DCD" "1,3,5") (removeOptionD (makeLabelInstr "labelT" "DCD" "1,3,5").DCDValueList) "updateSymbolTable DCD List Base Case" (["testL",256u; "labelT",1u] |> Map.ofList |> Ok)
                    //DCD Error Message Tests
                    makeTest stOneItem (makeLabelInstr "labelT" "DCD" "") (removeOptionD (makeLabelInstr "labelT" "DCD" "").DCDValueList) "updateSymbolTable DCD No input Case" (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    makeTest stOneItem (makeLabelInstr "labelT" "DCD" "a") (removeOptionD (makeLabelInstr "labelT" "DCD" "a").DCDValueList) "updateSymbolTable DCD Invalid input Case" (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    makeTest stOneItem (makeLabelInstr "labelT" "DCD" "1, ,5") (removeOptionD (makeLabelInstr "labelT" "DCD" "1, ,5").DCDValueList) "updateSymbolTable DCD List No input Case" (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    makeTest stOneItem (makeLabelInstr "labelT" "DCD" "1, a, 5") (removeOptionD (makeLabelInstr "labelT" "DCD" "1, a, 5").DCDValueList) "updateSymbolTable DCD List Invalid input Case" (Error "parseLabelIns: Input to DCD function not valid (No input etc)")


                ]

    [<Tests>]
    let updateMemoryDataPathTest =
        let makeLabelInstr label root input =
            let (stOneItem: SymbolTable) = ["testL",256u] |> Map.ofList
            let ldFuncEQU Lab Ops = 
                    {LoadAddr= WA 200u; 
                        Label= Some Lab; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= Ops}
            let removeRecord x =
                match x with
                | Ok y -> y 
            ldFuncEQU label input
            |> parseLabelIns root
            |> removeRecord 
        let BaseDataPath : DataPath<'INS> =
            let memory : MachineMemory<'INS> = [WA 0x100u,DataLoc 5u] |> Map.ofList
            let registers : Map<RName,uint32> = [(R0: RName), 2u] |> Map.ofList
            let flags : Flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}

        let makeTest name inputRec inpDataPath output =
            testCase name <| fun () ->
                Expect.equal (updateMemoryDataPath inputRec inpDataPath) output (sprintf "updateMemoryDataPathTest Test '%s'" name)
        Expecto.Tests.testList "updateMemoryDataPathTest Tests"
                [   
                    //DCD Working Tests
                    makeTest "DCD Base Case" (makeLabelInstr "labelT" "DCD" "1") BaseDataPath (Ok {BaseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 1u] |> Map.ofList)})
                    makeTest "DCD List Base Case" (makeLabelInstr "labelT" "DCD" "1,3,5") BaseDataPath (Ok {BaseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 1u; WA 0x108u, DataLoc 3u; WA 0x10Cu, DataLoc 5u] |> Map.ofList)})
                    //DCD Error Message Tests
                    makeTest "DCD No Input" (makeLabelInstr "labelT" "DCD" "") BaseDataPath (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    makeTest "DCD Invalid Input" (makeLabelInstr "labelT" "DCD" "a") BaseDataPath (Error "parseLabelIns: Input to DCD function not valid (No input etc)")

                    //Fill Working Tests
                    makeTest "Fill Base Case" (makeLabelInstr "labelT" "FILL" "4") BaseDataPath (Ok {BaseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 0u; WA 0x108u, DataLoc 0u; WA 0x10Cu, DataLoc 0u; WA 0x110u, DataLoc 0u] |> Map.ofList)})
                    makeTest "Fill Changed given address" (makeLabelInstr "labelT" "FILL" "4") {BaseDataPath with MM =([WA 0x0u,DataLoc 4u] |> Map.ofList)} (Ok {BaseDataPath with MM = ([WA 0x0u,DataLoc 4u; WA 0x4u, DataLoc 0u; WA 0x8u, DataLoc 0u; WA 0xCu, DataLoc 0u; WA 0x10u, DataLoc 0u] |> Map.ofList)})
                    //Fill Error Message Tests
                    makeTest "Fill input/4 != int" (makeLabelInstr "labelT" "FILL" "1") {BaseDataPath with MM =([WA 0x10u,DataLoc 3u] |> Map.ofList)} (Error "parseLabelIns: Fill expression (1u) <0 or not divisible by four")
                    makeTest "Fill input/4 != int and negative" (makeLabelInstr "labelT" "FILL" "-3") {BaseDataPath with MM =([WA 0x100u,DataLoc 7u] |> Map.ofList)} (Error "parseLabelIns: Fill expression (4294967293u) <0 or not divisible by four")
                    makeTest "Fill No Input" (makeLabelInstr "labelT" "FILL" "") BaseDataPath (Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)")

                    //EQU Working Tests
                    makeTest "EQU Base Case" (makeLabelInstr "labelT" "EQU" "2") BaseDataPath (Ok BaseDataPath)
                    //EQU Error Message Tests
                    makeTest "EQU No Input" (makeLabelInstr "labelT" "EQU" "") BaseDataPath (Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)")

                ]



    [<Tests>]
    let DCDexecTest =
        let (stOneItem: SymbolTable) = ["testL",256u] |> Map.ofList
        let makeLabelInstr label root input =
            let ldFuncEQU Lab Ops = 
                    {LoadAddr= WA 200u; 
                        Label= Some Lab; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= Ops}
            let removeRecord x =
                match x with
                | Ok y -> y 
            ldFuncEQU label input
            |> parseLabelIns root
            |> removeRecord 
        let BaseDataPath : DataPath<'INS> =
            let memory : MachineMemory<'INS> = [WA 0x100u,DataLoc 5u] |> Map.ofList
            let registers : Map<RName,uint32> = [(R0: RName), 2u] |> Map.ofList
            let flags : Flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}

        let makeTest name inputSymTab inputRec inpDataPath output =
            testCase name <| fun () ->
                Expect.equal (DCDexec inputSymTab inpDataPath inputRec) output (sprintf "DCDexecTest Test '%s'" name)
        Expecto.Tests.testList "DCDexecTest Tests"
                [   
                    //DCD Working Tests
                    makeTest "DCDexec: DCD Base Case" stOneItem (makeLabelInstr "labelT" "DCD" "1") BaseDataPath ((["testL",256u; "labelT",1u] |> Map.ofList |> Ok),(Ok {BaseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 1u] |> Map.ofList)}))
                    makeTest "DCDexec: DCD List Base Case" stOneItem (makeLabelInstr "labelT" "DCD" "1, 3, 5") BaseDataPath ((["testL",256u; "labelT",1u] |> Map.ofList |> Ok),(Ok {BaseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 1u; WA 0x108u, DataLoc 3u; WA 0x10Cu, DataLoc 5u] |> Map.ofList)}))
                    makeTest "DCDexec: DCD Negative List" stOneItem (makeLabelInstr "labelT" "DCD" "1, -3, 5") BaseDataPath ((["testL",256u; "labelT",1u] |> Map.ofList |> Ok),(Ok {BaseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 1u; WA 0x108u, DataLoc 4294967293u; WA 0x10Cu, DataLoc 5u] |> Map.ofList)}))

                    // //DCD Error Message Tests
                    makeTest "DCDexec: DCD No Input" stOneItem (makeLabelInstr "labelT" "DCD" "") BaseDataPath ((Error "parseLabelIns: Input to DCD function not valid (No input etc)"),(Error "parseLabelIns: Input to DCD function not valid (No input etc)"))
                    makeTest "DCDexec: DCD Invalid Input" stOneItem (makeLabelInstr "labelT" "DCD" "a") BaseDataPath ((Error "parseLabelIns: Input to DCD function not valid (No input etc)"),(Error "parseLabelIns: Input to DCD function not valid (No input etc)"))
                    makeTest "DCDexec: DCD No Input List" stOneItem (makeLabelInstr "labelT" "DCD" "1, ,5") BaseDataPath ((Error "parseLabelIns: Input to DCD function not valid (No input etc)"),(Error "parseLabelIns: Input to DCD function not valid (No input etc)"))
                    makeTest "DCDexec: DCD Invalid List Input" stOneItem (makeLabelInstr "labelT" "DCD" "1,a,5") BaseDataPath ((Error "parseLabelIns: Input to DCD function not valid (No input etc)"),(Error "parseLabelIns: Input to DCD function not valid (No input etc)"))

                ]

    [<Tests>]
    let EQUexecTest =
        let (stOneItem: SymbolTable) = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeLabelInstr label root input =
            let ldFuncEQU Lab Ops = 
                    {LoadAddr= WA 200u; 
                        Label= Some Lab; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= Ops}
            let removeRecord x =
                match x with
                | Ok y -> y 
            ldFuncEQU label input
            |> parseLabelIns root
            |> removeRecord 
        let BaseDataPath : DataPath<'INS> =
            let memory : MachineMemory<'INS> = [WA 0x100u,DataLoc 5u] |> Map.ofList
            let registers : Map<RName,uint32> = [(R0: RName), 2u] |> Map.ofList
            let flags : Flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}

        let makeTest name inputSymTab inputRec inpDataPath output =
            testCase name <| fun () ->
                Expect.equal (EQUexec inputSymTab inpDataPath inputRec) output (sprintf "EQUexecTest Test '%s'" name)
        Expecto.Tests.testList "EQUexecTest Tests"
                [   
                    //EQU Working Tests
                    makeTest "EQUexec: EQU Base Case" stOneItem (makeLabelInstr "labelT" "EQU" "2") BaseDataPath ((["testL",256u; "testL2",260u; "labelT",2u] |> Map.ofList |> Ok),(Ok BaseDataPath))
                    makeTest "EQUexec: EQU *+- Test" stOneItem (makeLabelInstr "labelT" "EQU" "5-4*3-1*1+2*2*2") BaseDataPath ((["testL",256u; "testL2",260u; "labelT",0u] |> Map.ofList |> Ok),(Ok BaseDataPath))
                    makeTest "EQUexec: EQU label test" stOneItem (makeLabelInstr "labelT" "EQU" "testL") BaseDataPath ((["testL",256u; "testL2",260u; "labelT",256u] |> Map.ofList |> Ok),(Ok BaseDataPath))
                    makeTest "EQUexec: EQU number label addition test" stOneItem (makeLabelInstr "labelT" "EQU" "testL + testL2") BaseDataPath ((["testL",256u; "testL2",260u; "labelT",516u] |> Map.ofList |> Ok),(Ok BaseDataPath))


                    //EQU Error Message Tests
                    makeTest "EQUexec: EQU No Input test" stOneItem (makeLabelInstr "labelT" "EQU" "") BaseDataPath ((Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)"),(Ok BaseDataPath))

                ]



    [<Tests>]
    let FILLexecTest =
        let (stOneItem: SymbolTable) = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeLabelInstr label root input =
            let ldFuncEQU Lab Ops = 
                    {LoadAddr= WA 200u; 
                        Label= Some Lab; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= Ops}
            let removeRecord x =
                match x with
                | Ok y -> y 
            ldFuncEQU label input
            |> parseLabelIns root
            |> removeRecord 
        let BaseDataPath : DataPath<'INS> =
            let memory : MachineMemory<'INS> = [WA 0x100u,DataLoc 5u] |> Map.ofList
            let registers : Map<RName,uint32> = [(R0: RName), 2u] |> Map.ofList
            let flags : Flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}

        let makeTest name inputSymTab inputRec inpDataPath output =
            testCase name <| fun () ->
                Expect.equal (FILLexec inputSymTab inpDataPath inputRec) output (sprintf "FILLexecTest Test '%s'" name)
        Expecto.Tests.testList "FILLexecTest Tests"
                [   
                    //FILL Working Tests
                    makeTest "FILLexec: FILL Base Case" stOneItem (makeLabelInstr "labelT" "FILL" "4") BaseDataPath ((["testL",256u; "testL2",260u; "labelT",0u] |> Map.ofList |> Ok),(Ok {BaseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 0u; WA 0x108u, DataLoc 0u; WA 0x10Cu, DataLoc 0u; WA 0x110u, DataLoc 0u] |> Map.ofList)}))
                    makeTest "FILLexec: FILL Changed base address" stOneItem (makeLabelInstr "labelT" "FILL" "4") {BaseDataPath with MM =([WA 0x0u,DataLoc 5u] |> Map.ofList)} ((["testL",256u; "testL2",260u; "labelT",0u] |> Map.ofList |> Ok),(Ok {BaseDataPath with MM = ([WA 0x0u,DataLoc 5u; WA 0x4u, DataLoc 0u; WA 0x8u, DataLoc 0u; WA 0xCu, DataLoc 0u; WA 0x10u, DataLoc 0u] |> Map.ofList)}))

                    //FILL Error Message Tests
                    makeTest "FILLexec: Fill input/4 != int" stOneItem (makeLabelInstr "labelT" "FILL" "1") BaseDataPath ((Error "parseLabelIns: Fill expression (1u) <0 or not divisible by four"),(Error "parseLabelIns: Fill expression (1u) <0 or not divisible by four"))
                    makeTest "FILLexec: Fill input/4 != int and negative" stOneItem (makeLabelInstr "labelT" "FILL" "-3") BaseDataPath ((Error "parseLabelIns: Fill expression (4294967293u) <0 or not divisible by four"),(Error "parseLabelIns: Fill expression (4294967293u) <0 or not divisible by four"))
                    makeTest "FILLexec: No Input" stOneItem (makeLabelInstr "labelT" "FILL" "") BaseDataPath ((Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)"),(Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)"))
                ]





    [<Tests>]
    let ADRexecTest =
        let (stOneItem: SymbolTable) = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeADRInstr label root input =
            let ldFuncEQU Lab Ops = 
                    {LoadAddr= WA 200u; 
                        Label= Some Lab; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= Ops}
            let removeRecord x =
                match x with
                | Ok y -> y 
            ldFuncEQU label input
            |> parseAdrIns root
            |> removeRecord 
        let BaseDataPath : DataPath<'INS> =
            let memory : MachineMemory<'INS> = [WA 0x100u,DataLoc 5u] |> Map.ofList
            let registers : Map<RName,uint32> = [(R0: RName), 2u] |> Map.ofList
            let flags : Flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}

        let makeTest name symTab inpDataPath inputRec output =
            testCase name <| fun () ->
                Expect.equal (ADRexec symTab inputRec inpDataPath) output (sprintf "ADRexecTest Test '%s'" name)
        Expecto.Tests.testList "ADRexecTest Tests"
                [   
            //ADRexec (dP: DataPath<'INS>) (inputRecord: ADRInstr)
                    //ADR Working Tests
                    makeTest "ADRexec: ADR Base Case" stOneItem (makeADRInstr "labelT" "ADR" "R1, 4") BaseDataPath (Ok stOneItem,(Ok {BaseDataPath with Regs = ([(R0: RName), 2u; R1, 4u] |> Map.ofList)}))
                    makeTest "ADRexec: ADR Label Base Case" stOneItem (makeADRInstr "labelT" "ADR" "R1, testL") BaseDataPath (Ok stOneItem,(Ok {BaseDataPath with Regs = ([(R0: RName), 2u; R1, 256u] |> Map.ofList)}))
                    makeTest "ADRexec: ADR Replacing Reg Value" stOneItem (makeADRInstr "labelT" "ADR" "R0, testL") BaseDataPath (Ok stOneItem,(Ok {BaseDataPath with Regs = ([(R0: RName), 256u] |> Map.ofList)}))

                    //ADR Error Message Tests
                    // makeTest "ADRexec: ADR No Input" (makeADRInstr "labelT" "ADR" "R0, ") BaseDataPath (Ok {BaseDataPath with Regs = ([(R0: RName), 256u] |> Map.ofList)})

                ]







    [<Tests>]
    let LDRexecTest =
        let (stOneItem: SymbolTable) = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeMemInstr root suffix input =
            let ldFuncEQU Ops = 
                    {LoadAddr= WA 200u; 
                        Label= Some "labelT"; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= Ops}
            let removeRecord x =
                match x with
                | Ok y -> y 
            ldFuncEQU input
            |> parseMemIns root suffix
            |> removeRecord 
        let BaseDataPath1 : DataPath<'INS> =
            let memory : MachineMemory<'INS> = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u] |> Map.ofList
            let registers : Map<RName,uint32> = [R0, 2u; R1, 0x100u] |> Map.ofList
            let flags : Flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}
        let BaseDataPath2 : DataPath<'INS> =
            let memory : MachineMemory<'INS> = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers : Map<RName,uint32> = [R10, 2u; R11, 0xCu; R15, 0x100u] |> Map.ofList
            let flags : Flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}
        let BaseDataPath3 : DataPath<'INS> =
            let memory : MachineMemory<'INS> = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers : Map<RName,uint32> = [R10, 2u; R11, 0x100u; R15, 0x1u] |> Map.ofList
            let flags : Flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}
        let BaseDataPath4 : DataPath<'INS> =
            let memory : MachineMemory<'INS> = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers : Map<RName,uint32> = [R10, 2u; R11, 0x100u; R15, 0x4u] |> Map.ofList
            let flags : Flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}
        let BaseDataPath5 : DataPath<'INS> =
            let memory : MachineMemory<'INS> = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers : Map<RName,uint32> = [R10, 2u; R11, 0x100u; R15, 0x4u] |> Map.ofList
            let flags : Flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}


        let makeTest name symTab inpDataPath inputRec output =
            testCase name <| fun () ->
                Expect.equal (LDRexec symTab inputRec inpDataPath) output (sprintf "LDRexecTest Test '%s'" name)
        Expecto.Tests.testList "LDRexecTest Tests"
                [   
                    //LDR Working Tests
                    makeTest "LDRexec: LDR Base Case" stOneItem (makeMemInstr "LDR" "" "R0, [R1]") BaseDataPath1 (Ok stOneItem,(Ok {BaseDataPath1 with Regs = ([(R0: RName), 5u; R1, 0x100u] |> Map.ofList)}))
                    makeTest "LDRexec: LDR two digit Registers" stOneItem (makeMemInstr "LDR" "" "R10, [R15]") BaseDataPath2 (Ok stOneItem,(Ok {BaseDataPath2 with Regs = ([R10, 5u; R11, 0xCu; R15, 0x100u] |> Map.ofList)}))
                    makeTest "LDRexec: LDR Num Increment" stOneItem (makeMemInstr "LDR" "" "R0, [R1, #4]") BaseDataPath1 (Ok stOneItem,(Ok {BaseDataPath1 with Regs = ([R0, 7u; R1, 0x100u] |> Map.ofList)}))
                    makeTest "LDRexec: LDR Post Increment" stOneItem (makeMemInstr "LDR" "" "R0, [R1], #4") BaseDataPath1 (Ok stOneItem,(Ok {BaseDataPath1 with Regs = ([R0, 5u; R1, 0x104u] |> Map.ofList)}))
                    makeTest "LDRexec: LDR Pre Increment" stOneItem (makeMemInstr "LDR" "" "R0, [R1, #4]!") BaseDataPath1 (Ok stOneItem,(Ok {BaseDataPath1 with Regs = ([R0, 7u; R1, 0x104u] |> Map.ofList)}))
                    makeTest "LDRexec: LDR Adding Registers" stOneItem (makeMemInstr "LDR" "" "R10, [R11, R15]") BaseDataPath2 (Ok stOneItem,(Ok {BaseDataPath2 with Regs = ([R10, 11u; R11, 0xCu; R15, 0x100u] |> Map.ofList)}))
                    makeTest "LDRexec: LDR Adding Shifted Register" stOneItem (makeMemInstr "LDR" "" "R10, [R11, R15, LSL #2]") BaseDataPath3 (Ok stOneItem,(Ok {BaseDataPath3 with Regs = ([R10, 7u; R11, 0x100u; R15, 0x1u] |> Map.ofList)}))
                    makeTest "LDRexec: LDR Shifted and Pre" stOneItem (makeMemInstr "LDR" "" "R10, [R11, R15, LSL #2]!") BaseDataPath3 (Ok stOneItem,(Ok {BaseDataPath3 with Regs = ([R10, 7u; R11, 0x104u; R15, 0x1u] |> Map.ofList)}))
                    makeTest "LDRexec: LDR 0 Shift and Pre" stOneItem (makeMemInstr "LDR" "" "R10, [R11, R15, LSL #0]!") BaseDataPath4 (Ok stOneItem,(Ok {BaseDataPath4 with Regs = ([R10, 7u; R11, 0x104u; R15, 0x4u] |> Map.ofList)}))


                    // //LDR Error Message Tests
                    makeTest "LDRexec: LDR -1 Shift and Pre" stOneItem (makeMemInstr "LDR" "" "R10, [R11, R15, LSL #-1]!") BaseDataPath5 (Ok stOneItem,(Ok {BaseDataPath5 with Regs = ([R10, 5u; R11, 0x100u; R15, 0x4u] |> Map.ofList)}))


                ]






    [<Tests>]
    let STRexecTest =
        let (stOneItem: SymbolTable) = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeMemInstr root suffix input =
            let ldFuncEQU Ops = 
                    {LoadAddr= WA 200u; 
                        Label= Some "labelT"; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= Ops}
            let removeRecord x =
                match x with
                | Ok y -> y 
            ldFuncEQU input
            |> parseMemIns root suffix
            |> removeRecord 
        let BaseDataPath1 : DataPath<'INS> =
            let memory : MachineMemory<'INS> = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers : Map<RName,uint32> = [R0, 2u; R1, 0x100u] |> Map.ofList
            let flags : Flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}


        let makeTest name symTab inpDataPath inputRec output =
            testCase name <| fun () ->
                Expect.equal (STRexec symTab inputRec inpDataPath) output (sprintf "STRexecTest Test '%s'" name)
        Expecto.Tests.testList "STRexecTest Tests"
                [   
                    //STR Working Tests
                    makeTest "STRexec: STR Base Case" stOneItem (makeMemInstr "STR" "" "R0, [R1]") BaseDataPath1 (Ok stOneItem,(Ok {BaseDataPath1 with MM = ([WA 0x100u,DataLoc 2u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList)}))
                    // makeTest "STRexec: STR Num Increment" stOneItem (makeMemInstr "STR" "" "R0, [R1, #4]") BaseDataPath1 (Ok stOneItem,(Ok {BaseDataPath1 with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 2u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList)}))
                    //STR Error Message Tests

                ]


