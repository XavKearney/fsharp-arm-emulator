//A series of tests for all the functions in memInstructions
module memInstructionsTests
    open CommonData
    open CommonLex
    open Expecto
    open Expecto.ExpectoFsCheck
    open memInstructions
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
                                                                                    EQUExpr = (Some 24u); DCDValueList = None;
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
        let makeevalExpTest name input output =
            testCase name <| fun () ->
                Expect.equal (evalExpression input) output (sprintf "evalExpression Test '%s'" input)
        Expecto.Tests.testList "evalExpressions Tests"
                [   //parseDCD

                    makeevalExpTest "Mult Only" "1*2*3*4" 24u
                    makeevalExpTest "Add Only" "1+2" 3u
                    makeevalExpTest "Subtract Only" "3-1" 2u
                    makeevalExpTest "All" "1*2*3*4+5*3-2" 54u
                    makeevalExpTest "All2" "5-4*3-1*1+2*2*2" 24u
                    makeevalExpTest "Num Only" "3" 3u

                ]



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
                Expect.equal (parseMemIns root suffix ld) output (sprintf "Parse LDR/STR Tests '%s'" root)
        Expecto.Tests.testList "parseMemIns Tests"
                [   

                    makeTest "LDR" "" (ldFunc "R0, [R1]") "LDR1" {InstructionType= LDR;
                                                                    DestSourceReg= R0; AddressReg= R1;
                                                                    BytesNotWords= false; IncrementValue= 0;
                                                                    PreIndexRbBy= false; PostIndexRbBy= false; 
                                                                    ExtraAddressReg= None;
                                                                    ShiftExtraRegBy= None;}
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