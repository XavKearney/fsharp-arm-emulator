//A series of tests for all the functions in Mem module
module MemTests
    open CommonData
    open CommonLex
    open Expecto
    open Mem
    open VisualTest




    [<Tests>]
    let parseLabelInsTest = 
        let x:SymbolTable = ["a",uint32 2] |> Map.ofList
        let ldFunc lab ops  = 
                {LoadAddr= WA 100u; 
                    Label= Some lab; 
                    SymTab= Some x;
                    OpCode= "";
                    Operands= ops}
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
                    makeTest "EQU" (ldFunc "labelT" "") "EQU No Literal" (Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)")


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
                    makeTest "FILL" (ldFunc "labelT" "3") "FILL 3" (Error "parseLabelIns: Fill expression (3u) <0 or not divisible by four")
                    makeTest "FILL" (ldFunc "labelT" "123") "FILL 123" (Error "parseLabelIns: Fill expression (123u) <0 or not divisible by four")
                    makeTest "FILL" (ldFunc "labelT" "-1") "FILL -1" (Error "parseLabelIns: Fill expression (4294967295u) <0 or not divisible by four")
                    makeTest "FILL" (ldFunc "labelT" "-4") "FILL -4" (Error "parseLabelIns: Fill expression (4294967292u) <0 or not divisible by four")
                    
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
                    makeTest "DCD" (ldFunc "labelT" "") "DCD no input" (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    makeTest "DCD" (ldFunc "labelT" "a") "DCD invalid input" (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    makeTest "DCD" (ldFunc "labelT" "1, ,5") "DCD invalid space list input" (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    makeTest "DCD" (ldFunc "labelT" "1, a, 5") "DCD invalid list input" (Error "parseLabelIns: Input to DCD function not valid (No input etc)")


                
                ]







    [<Tests>]
    let evalExpressionTest =
        let st:SymbolTable = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeEvalExpTest name labels input output =
            testCase name <| fun () ->
                Expect.equal (evalExpression input st labels) output (sprintf "evalExpression Test '%s'" input)
        Expecto.Tests.testList "evalExpressions Tests"
                [   
                    //evalExpression Working Tests
                    makeEvalExpTest "evalExpressions: Mult Only" true "1*2*3*4" (Ok 24u)
                    makeEvalExpTest "evalExpressions: Add Only" true "1+2" (Ok 3u)
                    makeEvalExpTest "evalExpressions: Subtract Only" true "3-1" (Ok 2u)
                    makeEvalExpTest "evalExpressions: All" true "1*2*3*4+5*3-2" (Ok 37u)
                    makeEvalExpTest "evalExpressions: All2" true "5-4*3-1*1+2*2*2" (Ok 0u)
                    makeEvalExpTest "evalExpressions: Num Only" true "3" (Ok 3u)
                    makeEvalExpTest "evalExpressions: Label Only" true "testL" (Ok 256u)
                    makeEvalExpTest "evalExpressions: Label + 2" true "testL + 2" (Ok 258u)
                    makeEvalExpTest "evalExpressions: Label Right Multiply" true "testL + 2*2" (Ok 260u)
                    makeEvalExpTest "evalExpressions: Label Left Multiply" true "2*2 + testL" (Ok 260u)
                    makeEvalExpTest "evalExpressions: Label Add Hex" true "testL + 0x4" (Ok 260u)
                    makeEvalExpTest "evalExpressions: Label Add Hex&" true "testL + &4" (Ok 260u)
                    makeEvalExpTest "evalExpressions: Label Add Bin" true "testL + 0b100" (Ok 260u)
                    makeEvalExpTest "evalExpressions: Brackets1" true "(4*2)+3" (Ok 11u)
                    makeEvalExpTest "evalExpressions: Brackets2" true "testL + (2*2)" (Ok 260u)
                    makeEvalExpTest "evalExpressions: Label Right Left Multiply" true "4*2 + testL + 2*2" (Ok 268u)
                    makeEvalExpTest "evalExpressions: * first character" true "*3+7" (Ok 10u)
                    makeEvalExpTest "evalExpressions: + first character" true "+3+7" (Ok 10u)
                    makeEvalExpTest "evalExpressions: - first character" true "-3+7" (Ok 4u)
                    makeEvalExpTest "evalExpressions: Negative Output" true "3-7" (Ok 4294967292u)
                    makeEvalExpTest "evalExpressions: NumLabel Only" true "testL2" (Ok 260u)
                    // makeEvalExpTest "Brackets test" "2*(6+(3*4)-(6+3))*5" 90u

                
                    //evalExpression Error Message Tests
                    makeEvalExpTest "No Input" true "" (Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)")
                    makeEvalExpTest "evalExpressions: Labels not allowed" false "testL2" (Error "evalExpression-numberOrLabel: Attempting to parse label when labels are not allowed, ie for Fill")

                ]


    [<Tests>]
    let parseMemInsTest = 
        let x:SymbolTable = ["a",uint32 2] |> Map.ofList
        let ldFunc ops = 
                {LoadAddr= WA 100u; 
                    Label= Some "labelT"; 
                    SymTab= Some x;
                    OpCode= "";
                    Operands= ops}
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
        let ldFunc symTab ops = 
                {LoadAddr= WA 100u; 
                    Label= Some "labelT"; 
                    SymTab= Some symTab;
                    OpCode= "";
                    Operands= ops}
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
                    // OxCCCCCCCCu
                    //(-1|>uint32)
                    

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
                    makeTest (ldFunc st "ADR" "R0, 4") "parse ADR Base Test" (Some (Ok {PInstr = AdrO (Ok {InstructionType= Ok ADRm;
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
                    makeTest (ldFuncAll 100u "labelTest" st "FILL" "-1") "parse: Fill Error Case" (Some (Ok {PInstr = LabelO (Error "parseLabelIns: Fill expression (4294967295u) <0 or not divisible by four");
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))
                    //LDR
                    makeTest (ldFuncAll 100u "labelTest" st "LDR" "R0, [R1]") "parse: LDR Base Case" (Some (Ok {PInstr = MemO (Ok {InstructionType= Ok LDR;
                                                                                    DestSourceReg= Ok R0; AddressReg= Ok R1;
                                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                                    ExtraAddressReg= None;
                                                                                    ShiftExtraRegBy= None;});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))
                    makeTest (ldFuncAll 100u "labelTest" st "LDR" "R0, [R11, R1, LSL #-1]!") "parse: LDR -1 Shift and Pre" (Some (Ok {PInstr = MemO (Ok {InstructionType= Ok LDR;
                                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                                    PreIndexRb= true; PostIndexRb= false; 
                                                                                    ExtraAddressReg= None;
                                                                                    ShiftExtraRegBy= None;});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))
                    //STR
                    makeTest (ldFuncAll 100u "labelTest" st "STR" "R0, [R11, R1, LSL #1]") "parse: STR Shifted Register" (Some (Ok {PInstr = MemO (Ok {InstructionType= Ok STR;
                                                                                    DestSourceReg= Ok R0; AddressReg= Ok R11;
                                                                                    BytesNotWords= Ok false; IncrementValue= 0;
                                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                                    ExtraAddressReg= Some R1;
                                                                                    ShiftExtraRegBy= Some 1;});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))
                    makeTest (ldFuncAll 100u "labelTest" st "STR" "R8, [R7], #40") "parse: STR Post Increment" (Some (Ok {PInstr = MemO (Ok {InstructionType= Ok STR;
                                                                                    DestSourceReg= Ok R8; AddressReg= Ok R7;
                                                                                    BytesNotWords= Ok false; IncrementValue= 40;
                                                                                    PreIndexRb= false; PostIndexRb= true; 
                                                                                    ExtraAddressReg= None;
                                                                                    ShiftExtraRegBy= None;});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))
                    makeTest (ldFuncAll 100u "labelTest" st "STR" "R6, [R5, #4]") "parse: STR Num Increment" (Some (Ok {PInstr = MemO (Ok {InstructionType= Ok STR;
                                                                                    DestSourceReg= Ok R6; AddressReg= Ok R5;
                                                                                    BytesNotWords= Ok false; IncrementValue= 4;
                                                                                    PreIndexRb= false; PostIndexRb= false; 
                                                                                    ExtraAddressReg= None;
                                                                                    ShiftExtraRegBy= None;});
                                                                    PLabel = Some ("labelTest", 100u);
                                                                    PSize = 4u; PCond = Cal}))

                ]


    type ParseLabelInsField = Uin of uint32 | Vl of ValueList

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
            let ldFuncEQU lab ops  = 
                    {LoadAddr= WA 100u; 
                        Label= Some lab; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= ops}
            ldFuncEQU label input
            |> parseLabelIns root
        let makeTest symTab inpRec field name output =
            match inpRec with
            | Ok v ->   
                    match field with 
                    | "EQU" ->  testCase name <| fun () ->
                                    Expect.equal (updateSymbolTable symTab v v.EQUExpr) output (sprintf "checkUpdateSymbolTable Tests, Test: %A" name)
                    | "FILL" ->  testCase name <| fun () ->
                                    Expect.equal (updateSymbolTable symTab v v.FillN) output (sprintf "checkUpdateSymbolTable Tests, Test: %A" name)
                    | "DCD" ->  testCase name <| fun () ->
                                    Expect.equal (updateSymbolTable symTab v (removeOptionD v.DCDValueList)) output (sprintf "checkUpdateSymbolTable Tests, Test: %A" name)
                    | _ ->      testCase name <| fun () ->
                                    Expect.equal 1 2 (sprintf "checkUpdateSymbolTable Tests, Test: %A, Unexpected field value: %A" name field)
            | Error m -> testCase name <| fun () ->
                            Expect.equal 1 2 (sprintf "checkUpdateSymbolTable, Test: %A, Instruction was parsed with an error: %A" name m)
        Expecto.Tests.testList "checkUpdateSymbolTable Tests"
                [   
                    //EQU Working tests
                    makeTest stOneItem (makeLabelInstr "labelT" "EQU" "2") "EQU" "updateSymbolTable EQU 2 Base Test" (["testL",256u; "labelT",2u] |> Map.ofList |> Ok)
                    makeTest stOneItem (makeLabelInstr "labelT" "EQU" "4") "EQU" "updateSymbolTable EQU 4 Base Test" (["testL",256u; "labelT",4u] |> Map.ofList |> Ok)
                    makeTest stOneItem (makeLabelInstr "labelT" "EQU" "2") "EQU" "updateSymbolTable EQU parseLabelIns generation Test" (["testL",256u; "labelT",2u] |> Map.ofList |> Ok)
                    makeTest stOneItem (makeLabelInstr "labelT2" "EQU" "5-4*3-1*1+2*2*2") "EQU" "updateSymbolTable EQU +*- Test" (["testL",256u; "labelT2",0u] |> Map.ofList |> Ok)
                    //EQU Error Message tests
                    // makeTest stOneItem (makeLabelInstr "labelT2" "EQU" "") "EQU" "updateSymbolTable EQU Invalid Input Test" (Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)")


                    //FILL Working tests
                    makeTest stOneItem (makeLabelInstr "labelT" "FILL" "4") "FILL" "updateSymbolTable FILL Base Case" (["testL",256u; "labelT",0u] |> Map.ofList |> Ok)
                    makeTest stOneItem (makeLabelInstr "labelT" "FILL" "4*3") "FILL" "updateSymbolTable FILL Mult Case" (["testL",256u; "labelT",0u] |> Map.ofList |> Ok)
                    makeTest stOneItem (makeLabelInstr "labelT" "FILL" "0") "FILL" "updateSymbolTable FILL Zero Case" (["testL",256u; "labelT",0u] |> Map.ofList |> Ok)
                    //Fill Error Message Tests
                    // makeTest stOneItem (makeLabelInstr "labelT" "FILL" "-4") "FILL" "updateSymbolTable FILL -4 Case" (Error "parseLabelIns: Fill expression (4294967292u) <0 or not divisible by four" )

                    //DCD Working tests
                    makeTest stOneItem (makeLabelInstr "labelT" "DCD" "1") "DCD" "updateSymbolTable DCD Base Case" (["testL",256u; "labelT",1u] |> Map.ofList |> Ok)
                    makeTest stOneItem (makeLabelInstr "labelT" "DCD" "1,3,5") "DCD" "updateSymbolTable DCD List Base Case" (["testL",256u; "labelT",1u] |> Map.ofList |> Ok)
                    //DCD Error Message Tests
                    // makeTest stOneItem (makeLabelInstr "labelT" "DCD" "") "DCD" "updateSymbolTable DCD No input Case" (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    // makeTest stOneItem (makeLabelInstr "labelT" "DCD" "a") "DCD" "updateSymbolTable DCD Invalid input Case" (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    // makeTest stOneItem (makeLabelInstr "labelT" "DCD" "1, ,5") "DCD" "updateSymbolTable DCD List No input Case" (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    // makeTest stOneItem (makeLabelInstr "labelT" "DCD" "1, a, 5") "DCD" "updateSymbolTable DCD List Invalid input Case" (Error "parseLabelIns: Input to DCD function not valid (No input etc)")


                ]

    [<Tests>]
    let updateMemoryDataPathTest =
        let makeLabelInstr label root input =
            let (stOneItem: SymbolTable) = ["testL",256u] |> Map.ofList
            let ldFuncEQU lab ops  = 
                    {LoadAddr= WA 200u; 
                        Label= Some lab; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= ops}
            ldFuncEQU label input
            |> parseLabelIns root
        let baseDataPath =
            let memory = [WA 0x100u,DataLoc 5u] |> Map.ofList
            let registers = [(R0: RName), 2u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}

        let makeTest name inputRec inpDataPath output =
            match inputRec with
            | Ok v ->   testCase name <| fun () ->
                            Expect.equal (updateMemoryDataPath v inpDataPath) output (sprintf "updateMemoryDataPathTest Test '%s'" name)
            | Error m -> testCase name <| fun () ->
                            Expect.equal 1 2 (sprintf "updateMemoryDataPathTest Test '%s'\nInstruction was parsed with an error: %A" name m)
        Expecto.Tests.testList "updateMemoryDataPathTest Tests"
                [   
                    //DCD Working Tests
                    makeTest "DCD Base Case" (makeLabelInstr "labelT" "DCD" "1") baseDataPath (Ok {baseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 1u] |> Map.ofList)})
                    makeTest "DCD List Base Case" (makeLabelInstr "labelT" "DCD" "1,3,5") baseDataPath (Ok {baseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 1u; WA 0x108u, DataLoc 3u; WA 0x10Cu, DataLoc 5u] |> Map.ofList)})
                    //DCD Error Message Tests
                    // makeTest "DCD No Input" (makeLabelInstr "labelT" "DCD" "") baseDataPath (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    // makeTest "DCD Invalid Input" (makeLabelInstr "labelT" "DCD" "a") baseDataPath (Error "parseLabelIns: Input to DCD function not valid (No input etc)")

                    //Fill Working Tests
                    makeTest "Fill Base Case" (makeLabelInstr "labelT" "FILL" "4") baseDataPath (Ok {baseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 0u; WA 0x108u, DataLoc 0u; WA 0x10Cu, DataLoc 0u; WA 0x110u, DataLoc 0u] |> Map.ofList)})
                    makeTest "Fill Changed given address" (makeLabelInstr "labelT" "FILL" "4") {baseDataPath with MM =([WA 0x0u,DataLoc 4u] |> Map.ofList)} (Ok {baseDataPath with MM = ([WA 0x0u,DataLoc 4u; WA 0x4u, DataLoc 0u; WA 0x8u, DataLoc 0u; WA 0xCu, DataLoc 0u; WA 0x10u, DataLoc 0u] |> Map.ofList)})
                    //Fill Error Message Tests
                    // makeTest "Fill input/4 != int" (makeLabelInstr "labelT" "FILL" "1") {baseDataPath with MM =([WA 0x10u,DataLoc 3u] |> Map.ofList)} (Error "parseLabelIns: Fill expression (1u) <0 or not divisible by four")
                    // makeTest "Fill input/4 != int and negative" (makeLabelInstr "labelT" "FILL" "-3") {baseDataPath with MM =([WA 0x100u,DataLoc 7u] |> Map.ofList)} (Error "parseLabelIns: Fill expression (4294967293u) <0 or not divisible by four")
                    // makeTest "Fill No Input" (makeLabelInstr "labelT" "FILL" "") baseDataPath (Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)")

                    //EQU Working Tests
                    makeTest "EQU Base Case" (makeLabelInstr "labelT" "EQU" "2") baseDataPath (Ok baseDataPath)
                    //EQU Error Message Tests
                    // makeTest "EQU No Input" (makeLabelInstr "labelT" "EQU" "") baseDataPath (Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)")

                ]



    [<Tests>]
    let execDCDTest =
        let (stOneItem: SymbolTable) = ["testL",256u] |> Map.ofList
        let (stXK: SymbolTable) = ["test",0u] |> Map.ofList
        let makeLabelInstr label root input =
            let ldFuncEQU lab ops  = 
                    {LoadAddr= WA 200u; 
                        Label= Some lab; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= ops}
            ldFuncEQU label input
            |> parseLabelIns root
        let baseDataPath =
            let memory = [WA 0x100u,DataLoc 5u] |> Map.ofList
            let registers = [(R0: RName), 2u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}
        let dataPathXK =
            let memory = Map.empty
            let registers = [R0, 0u; R1, 0u; R2, 0u; R3, 0u; R4, 0u; R5, 0u; R6, 0u; R7, 0u; R8, 0u; R9, 0u; R10, 0u; R11, 0u; R12, 0u; R13, 0u; R14, 0u; R15, 0u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}


        let makeTest name inputSymTab inputRec inpDataPath output =
            match inputRec with
            | Ok v ->   testCase name <| fun () ->
                            Expect.equal (execDCD inputSymTab inpDataPath v) output (sprintf "execDCDTest Test '%s'" name)
            | Error m ->testCase name <| fun () ->
                            Expect.equal 1 2 (sprintf "execDCDTest Test '%s'\nInstruction was parsed with an error: %A" name m)
        Expecto.Tests.testList "execDCDTest Tests"
                [   
                    //DCD Working Tests
                    makeTest "execDCD: DCD Base Case" stOneItem (makeLabelInstr "labelT" "DCD" "1") baseDataPath (Ok ({baseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 1u] |> Map.ofList)}, ["testL",256u; "labelT",1u] |> Map.ofList))
                    makeTest "execDCD: DCD List Base Case" stOneItem (makeLabelInstr "labelT" "DCD" "1, 3, 5") baseDataPath (Ok ({baseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 1u; WA 0x108u, DataLoc 3u; WA 0x10Cu, DataLoc 5u] |> Map.ofList)}, ["testL",256u; "labelT",1u] |> Map.ofList))
                    makeTest "execDCD: DCD Negative List" stOneItem (makeLabelInstr "labelT" "DCD" "1, -3, 5") baseDataPath (Ok ({baseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 1u; WA 0x108u, DataLoc 4294967293u; WA 0x10Cu, DataLoc 5u] |> Map.ofList)}, ["testL",256u; "labelT",1u] |> Map.ofList))
                    makeTest "execDCD: DCD Base Case XK" stXK (makeLabelInstr "labelT" "DCD" "1") dataPathXK (Ok ({dataPathXK with MM = ([WA 0x100u,DataLoc 1u] |> Map.ofList)}, ["test",0u; "labelT", 1u] |> Map.ofList))

                    //DCD Error Message Tests
                    // makeTest "execDCD: DCD No Input" stOneItem (makeLabelInstr "labelT" "DCD" "") baseDataPath (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    // makeTest "execDCD: DCD Invalid Input" stOneItem (makeLabelInstr "labelT" "DCD" "a") baseDataPath (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    // makeTest "execDCD: DCD No Input List" stOneItem (makeLabelInstr "labelT" "DCD" "1, ,5") baseDataPath (Error "parseLabelIns: Input to DCD function not valid (No input etc)")
                    // makeTest "execDCD: DCD Invalid List Input" stOneItem (makeLabelInstr "labelT" "DCD" "1,a,5") baseDataPath (Error "parseLabelIns: Input to DCD function not valid (No input etc)")

                ]

    [<Tests>]
    let execEQUTest =
        let (stOneItem: SymbolTable) = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeLabelInstr label root input =
            let ldFuncEQU lab ops  = 
                    {LoadAddr= WA 200u; 
                        Label= Some lab; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= ops}
            ldFuncEQU label input
            |> parseLabelIns root
        let baseDataPath =
            let memory = [WA 0x100u,DataLoc 5u] |> Map.ofList
            let registers = [(R0: RName), 2u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}

        let makeTest name inputSymTab inputRec inpDataPath output =
            match inputRec with
            | Ok v ->   testCase name <| fun () ->
                            Expect.equal (execEQU inputSymTab inpDataPath v) output (sprintf "execEQUTest Test '%s'" name)
            | Error m ->testCase name <| fun () ->
                            Expect.equal 1 2 (sprintf "execEQUTest Test '%s'\nInstruction was parsed with an error: %A" name m) 
        Expecto.Tests.testList "execEQUTest Tests"
                [   
                    //EQU Working Tests
                    makeTest "execEQU: EQU Base Case" stOneItem (makeLabelInstr "labelT" "EQU" "2") baseDataPath (Ok (baseDataPath, ["testL",256u; "testL2",260u; "labelT",2u] |> Map.ofList))
                    makeTest "execEQU: EQU *+- Test" stOneItem (makeLabelInstr "labelT" "EQU" "5-4*3-1*1+2*2*2") baseDataPath (Ok (baseDataPath, ["testL",256u; "testL2",260u; "labelT",0u] |> Map.ofList))
                    makeTest "execEQU: EQU label test" stOneItem (makeLabelInstr "labelT" "EQU" "testL") baseDataPath (Ok (baseDataPath, ["testL",256u; "testL2",260u; "labelT",256u] |> Map.ofList))
                    makeTest "execEQU: EQU number label addition test" stOneItem (makeLabelInstr "labelT" "EQU" "testL + testL2") baseDataPath (Ok (baseDataPath, ["testL",256u; "testL2",260u; "labelT",516u] |> Map.ofList))


                    //EQU Error Message Tests
                    // makeTest "execEQU: EQU No Input test" stOneItem (makeLabelInstr "labelT" "EQU" "") baseDataPath (Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)")

                ]



    [<Tests>]
    let execFILLTest =
        let (stOneItem: SymbolTable) = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeLabelInstr label root input =
            let ldFuncEQU lab ops  = 
                    {LoadAddr= WA 200u; 
                        Label= Some lab; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= ops}
            ldFuncEQU label input
            |> parseLabelIns root
        let baseDataPath =
            let memory = [WA 0x100u,DataLoc 5u] |> Map.ofList
            let registers = [(R0: RName), 2u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}

        let makeTest name inputSymTab inputRec inpDataPath output =
            match inputRec with
            | Ok v ->   testCase name <| fun () ->
                            Expect.equal (execFILL inputSymTab inpDataPath v) output (sprintf "execFILLTest Test '%s'" name)
            | Error m ->testCase name <| fun () ->
                            Expect.equal 1 2 (sprintf "execFILLTest Test '%s'\nInstruction was parsed with an error: %A" name m)
        Expecto.Tests.testList "execFILLTest Tests"
                [   
                    //FILL Working Tests
                    makeTest "execFILL: FILL Base Case" stOneItem (makeLabelInstr "labelT" "FILL" "4") baseDataPath (Ok ({baseDataPath with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 0u; WA 0x108u, DataLoc 0u; WA 0x10Cu, DataLoc 0u; WA 0x110u, DataLoc 0u] |> Map.ofList)}, ["testL",256u; "testL2",260u; "labelT",0u] |> Map.ofList))
                    makeTest "execFILL: FILL Changed base address" stOneItem (makeLabelInstr "labelT" "FILL" "4") {baseDataPath with MM =([WA 0x0u,DataLoc 5u] |> Map.ofList)} (Ok ({baseDataPath with MM = ([WA 0x0u,DataLoc 5u; WA 0x4u, DataLoc 0u; WA 0x8u, DataLoc 0u; WA 0xCu, DataLoc 0u; WA 0x10u, DataLoc 0u] |> Map.ofList)}, ["testL",256u; "testL2",260u; "labelT",0u] |> Map.ofList))

                    //FILL Error Message Tests
                    // makeTest "execFILL: Fill input/4 != int" stOneItem (makeLabelInstr "labelT" "FILL" "1") baseDataPath (Error "parseLabelIns: Fill expression (1u) <0 or not divisible by four")
                    // makeTest "execFILL: Fill input/4 != int and negative" stOneItem (makeLabelInstr "labelT" "FILL" "-3") baseDataPath (Error "parseLabelIns: Fill expression (4294967293u) <0 or not divisible by four")
                    // makeTest "execFILL: No Input" stOneItem (makeLabelInstr "labelT" "FILL" "") baseDataPath (Error "evalExpression: End case did not match any of the evalExpression end case options (0x4, 2, 0b11, label2 etc)")
                ]





    [<Tests>]
    let execADRTest =
        let (stOneItem: SymbolTable) = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeADRInstr label root input =
            let ldFuncEQU lab ops  = 
                    {LoadAddr= WA 200u; 
                        Label= Some lab; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= ops}
            ldFuncEQU label input
            |> parseAdrIns root
        let baseDataPath =
            let memory = [WA 0x100u,DataLoc 5u] |> Map.ofList
            let registers = [(R0: RName), 2u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}

        let makeTest name symTab inputRec inpDataPath output =
            match inputRec with
            | Ok v ->   testCase name <| fun () ->
                            Expect.equal (execADR symTab inpDataPath v) output (sprintf "execADRTest Test '%s'" name)
            | Error m ->testCase name <| fun () ->
                            Expect.equal 1 2 (sprintf "execADRTest Test '%s'\nInstruction was parsed with an error: %A" name m)
        Expecto.Tests.testList "execADRTest Tests"
                [   
            //execADR (dP: DataPath<'INS>) (inputRecord: ADRInstr)
                    //ADR Working Tests
                    makeTest "execADR: ADR Base Case" stOneItem (makeADRInstr "labelT" "ADR" "R1, 4") baseDataPath (Ok ({baseDataPath with Regs = ([(R0: RName), 2u; R1, 4u] |> Map.ofList)}, stOneItem))
                    makeTest "execADR: ADR Label Base Case" stOneItem (makeADRInstr "labelT" "ADR" "R1, testL") baseDataPath (Ok ({baseDataPath with Regs = ([(R0: RName), 2u; R1, 256u] |> Map.ofList)}, stOneItem))
                    makeTest "execADR: ADR Replacing Reg Value" stOneItem (makeADRInstr "labelT" "ADR" "R0, testL") baseDataPath (Ok ({baseDataPath with Regs = ([(R0: RName), 256u] |> Map.ofList)}, stOneItem))

                    //ADR Error Message Tests
                    // makeTest "execADR: ADR No Input" (makeADRInstr "labelT" "ADR" "R0, ") baseDataPath (Ok {baseDataPath with Regs = ([(R0: RName), 256u] |> Map.ofList)})

                ]







    [<Tests>]
    let execLDRTest =
        let (stOneItem: SymbolTable) = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeMemInstr root suffix input =
            let ldFuncEQU ops = 
                    {LoadAddr= WA 200u; 
                        Label= Some "labelT"; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= ops}
            ldFuncEQU input
            |> parseMemIns root suffix
        let baseDataPath1 =
            let memory = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u] |> Map.ofList
            let registers = [R0, 2u; R1, 0x100u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}
        let baseDataPath2 =
            let memory = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers = [R10, 2u; R11, 0xCu; R15, 0x100u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}
        let baseDataPath3 =
            let memory = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers = [R10, 2u; R11, 0x100u; R15, 0x1u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}
        let baseDataPath4 =
            let memory = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers = [R10, 2u; R11, 0x100u; R15, 0x4u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}
        let baseDataPath5 =
            let memory = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers = [R10, 2u; R11, 0x100u; R15, 0x4u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}


        let makeTest name symTab inputRec inpDataPath output =
            match inputRec with
            | Ok v ->   testCase name <| fun () ->
                            Expect.equal (execLDR symTab inpDataPath v) output (sprintf "execLDRTest Test '%s'" name)
            | Error m ->testCase name <| fun () ->
                            Expect.equal 1 2 (sprintf "execLDRTest Test '%s'\nInstruction was parsed with an error: %A" name m)
        Expecto.Tests.testList "execLDRTest Tests"
                [   
                    //LDR Working Tests
                    makeTest "execLDR: LDR Base Case" stOneItem (makeMemInstr "LDR" "" "R0, [R1]") baseDataPath1 (Ok ({baseDataPath1 with Regs = ([(R0: RName), 5u; R1, 0x100u] |> Map.ofList)}, stOneItem))
                    makeTest "execLDR: LDR two digit Registers" stOneItem (makeMemInstr "LDR" "" "R10, [R15]") baseDataPath2 (Ok ({baseDataPath2 with Regs = ([R10, 5u; R11, 0xCu; R15, 0x100u] |> Map.ofList)}, stOneItem))
                    makeTest "execLDR: LDR Num Increment" stOneItem (makeMemInstr "LDR" "" "R0, [R1, #4]") baseDataPath1 (Ok ({baseDataPath1 with Regs = ([R0, 7u; R1, 0x100u] |> Map.ofList)}, stOneItem))
                    makeTest "execLDR: LDR Post Increment" stOneItem (makeMemInstr "LDR" "" "R0, [R1], #4") baseDataPath1 (Ok ({baseDataPath1 with Regs = ([R0, 5u; R1, 0x104u] |> Map.ofList)}, stOneItem))
                    makeTest "execLDR: LDR Pre Increment" stOneItem (makeMemInstr "LDR" "" "R0, [R1, #4]!") baseDataPath1 (Ok ({baseDataPath1 with Regs = ([R0, 7u; R1, 0x104u] |> Map.ofList)}, stOneItem))
                    makeTest "execLDR: LDR Adding Registers" stOneItem (makeMemInstr "LDR" "" "R10, [R11, R15]") baseDataPath2 (Ok ({baseDataPath2 with Regs = ([R10, 11u; R11, 0xCu; R15, 0x100u] |> Map.ofList)}, stOneItem))
                    makeTest "execLDR: LDR Adding Shifted Register" stOneItem (makeMemInstr "LDR" "" "R10, [R11, R15, LSL #2]") baseDataPath3 (Ok ({baseDataPath3 with Regs = ([R10, 7u; R11, 0x100u; R15, 0x1u] |> Map.ofList)}, stOneItem))
                    makeTest "execLDR: LDR Shifted and Pre" stOneItem (makeMemInstr "LDR" "" "R10, [R11, R15, LSL #2]!") baseDataPath3 (Ok ({baseDataPath3 with Regs = ([R10, 7u; R11, 0x104u; R15, 0x1u] |> Map.ofList)}, stOneItem))
                    makeTest "execLDR: LDR 0 Shift and Pre" stOneItem (makeMemInstr "LDR" "" "R10, [R11, R15, LSL #0]!") baseDataPath4 (Ok ({baseDataPath4 with Regs = ([R10, 7u; R11, 0x104u; R15, 0x4u] |> Map.ofList)}, stOneItem))


                    // //LDR Error Message Tests
                    makeTest "execLDR: LDR -1 Shift and Pre" stOneItem (makeMemInstr "LDR" "" "R10, [R11, R15, LSL #-1]!") baseDataPath5 (Ok ({baseDataPath5 with Regs = ([R10, 5u; R11, 0x100u; R15, 0x4u] |> Map.ofList)}, stOneItem))

                ]






    [<Tests>]
    let execSTRTest =
        let (stOneItem: SymbolTable) = ["testL",256u; "testL2",260u] |> Map.ofList
        let makeMemInstr root suffix input =
            let ldFuncEQU ops = 
                    {LoadAddr= WA 200u; 
                        Label= Some "labelT"; 
                        SymTab= Some stOneItem;
                        OpCode= "";
                        Operands= ops}
            ldFuncEQU input
            |> parseMemIns root suffix
        let baseDataPath1 =
            let memory = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers = [R0, 2u; R1, 0x100u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}
        let baseDataPath2 =
            let memory = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers = [R10, 2u; R11, 0xCu; R15, 0x100u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}
        let baseDataPath3 =
            let memory = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers = [R10, 2u; R11, 0x100u; R15, 0x1u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}
        let baseDataPath4 =
            let memory = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers = [R10, 2u; R11, 0x100u; R15, 0x4u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}
        let baseDataPath5 =
            let memory = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList
            let registers = [R10, 2u; R11, 0x100u; R15, 0x4u] |> Map.ofList
            let flags = { N= false; C=false; Z=false; V=false}
            {Fl= flags; Regs= registers; MM = memory}


        let makeTest name symTab inputRec inpDataPath output =
            match inputRec with
            | Ok v ->   testCase name <| fun () ->
                            Expect.equal (execSTR symTab inpDataPath v) output (sprintf "execSTRTest Test '%s'" name)
            | Error m ->testCase name <| fun () ->
                            Expect.equal 1 2 (sprintf "execSTRTest Test '%s'\nInstruction was parsed with an error: %A" name m)
        Expecto.Tests.testList "execSTRTest Tests"
                [   
                    //STR Working Tests
                    makeTest "execSTR: STR Base Case" stOneItem (makeMemInstr "STR" "" "R0, [R1]") baseDataPath1 (Ok ({baseDataPath1 with MM = ([WA 0x100u,DataLoc 2u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList)}, stOneItem))
                    makeTest "execSTR: STR two digit Registers" stOneItem (makeMemInstr "STR" "" "R10, [R15]") baseDataPath2 (Ok ({baseDataPath2 with MM = [WA 0x100u,DataLoc 2u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList}, stOneItem))
                    makeTest "execSTR: STR Num Increment" stOneItem (makeMemInstr "STR" "" "R0, [R1, #4]") baseDataPath1 (Ok ({baseDataPath1 with MM = ([WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 2u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList)}, stOneItem))
                    makeTest "execSTR: STR Post Increment" stOneItem (makeMemInstr "STR" "" "R0, [R1], #4") baseDataPath1 (Ok ({baseDataPath1 with Regs = ([R0, 2u; R1, 0x104u] |> Map.ofList); MM = [WA 0x100u,DataLoc 2u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList}, stOneItem))
                    makeTest "execSTR: STR Pre Increment" stOneItem (makeMemInstr "STR" "" "R0, [R1, #4]!") baseDataPath1 (Ok ({baseDataPath1 with Regs = ([R0, 2u; R1, 0x104u] |> Map.ofList); MM = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 2u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList}, stOneItem))
                    makeTest "execSTR: STR Adding Registers" stOneItem (makeMemInstr "STR" "" "R10, [R11, R15]") baseDataPath2 (Ok ({baseDataPath2 with Regs = ([R10, 2u; R11, 0xCu; R15, 0x100u] |> Map.ofList); MM = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 2u] |> Map.ofList}, stOneItem))
                    makeTest "execSTR: STR Adding Shifted Register" stOneItem (makeMemInstr "STR" "" "R10, [R11, R15, LSL #2]") baseDataPath3 (Ok ({baseDataPath3 with Regs = ([R10, 2u; R11, 0x100u; R15, 0x1u] |> Map.ofList); MM = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 2u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList}, stOneItem))
                    makeTest "execSTR: STR Shifted and Pre" stOneItem (makeMemInstr "STR" "" "R10, [R11, R15, LSL #2]!") baseDataPath3 (Ok ({baseDataPath3 with Regs = ([R10, 2u; R11, 0x104u; R15, 0x1u] |> Map.ofList); MM = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 2u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList}, stOneItem))
                    makeTest "execSTR: STR 0 Shift and Pre" stOneItem (makeMemInstr "STR" "" "R10, [R11, R15, LSL #0]!") baseDataPath4 (Ok ({baseDataPath4 with Regs = ([R10, 2u; R11, 0x104u; R15, 0x4u] |> Map.ofList); MM = [WA 0x100u,DataLoc 5u; WA 0x104u, DataLoc 2u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList}, stOneItem))

                    //STR Error Message Tests
                    makeTest "execSTR: STR -1 Shift and Pre" stOneItem (makeMemInstr "STR" "" "R10, [R11, R15, LSL #-1]!") baseDataPath5 (Ok ({baseDataPath5 with Regs = ([R10, 2u; R11, 0x100u; R15, 0x4u] |> Map.ofList); MM = [WA 0x100u,DataLoc 2u; WA 0x104u, DataLoc 7u; WA 0x108u, DataLoc 9u; WA 0x10Cu, DataLoc 11u] |> Map.ofList}, stOneItem))

                ]


