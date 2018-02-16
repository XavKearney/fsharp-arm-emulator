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
    let tTest = 
        let x:SymbolTable = ["a",uint32 2] |> Map.ofList
        let ldFunc Lab Ops = 
                {LoadAddr= WA 100u; 
                    Label= Some Lab; 
                    SymTab= Some x;
                    OpCode= "";
                    Operands= Ops}
        let lineDataEQU = ldFunc "labelT" "4"
        let makeTest root ld name output =
            testCase name <| fun () ->
                Expect.equal (parseLabelIns root ld) output (sprintf "Identity Test '%s'" root)
        Expecto.Tests.testList "parseLabelIns Tests"
                [   

                    makeTest "EQU" lineDataEQU "EQU1" {InstructionType = EQU; Name = (StrLabel (Some "labelT")); 
                                                        EQUExpr = Some (Num (Some 4u)); DCDValueList = None;
                                                        FillN = None}
                    makeTest "EQU" (ldFunc "labelT" "2") "EQU2" {InstructionType = EQU; Name = (StrLabel (Some "labelT")); 
                                                                            EQUExpr = Some (Num (Some 2u)); DCDValueList = None;
                                                                            FillN = None}
                    makeTest "EQU" (ldFunc "labelT" "3*4") "EQU Mult" {InstructionType = EQU; Name = (StrLabel (Some "labelT")); 
                                                                                    EQUExpr = Some (Num (Some 12u)); DCDValueList = None;
                                                                                    FillN = None}
                    makeTest "EQU" (ldFunc "labelT" "1+2") "EQU Add" {InstructionType = EQU; Name = (StrLabel (Some "labelT")); 
                                                                                    EQUExpr = Some (Num (Some 3u)); DCDValueList = None;
                                                                                    FillN = None}
                    makeTest "EQU" (ldFunc "labelT" "3-2") "EQU Sub" {InstructionType = EQU; Name = (StrLabel (Some "labelT")); 
                                                                                    EQUExpr = Some (Num (Some 1u)); DCDValueList = None;
                                                                                    FillN = None}
                    makeTest "EQU" (ldFunc "labelT" "5-4*3-1*1+2*2*2") "EQU All" {InstructionType = EQU; Name = (StrLabel (Some "labelT"));
                                                                                    EQUExpr = Some (Num (Some 24u)); DCDValueList = None;
                                                                                    FillN = None}
                    // makeTest "DCD" (ldFunc 100u "labelT" x "" "1") "DCD 1" {InstructionType = DCD; Name = (StrLabel (Some "labelT"));
                    //                                                                 EQUExpr = None; DCDValueList = Some ["1"];
                    //                                                                 FillN = None}
                    // makeTest "DCD" (ldFunc 100u "labelT" x "" "1, 3, 5") "DCD 1,3,5" {InstructionType = DCD; Name = (StrLabel (Some "labelT"));
                    //                                                                 EQUExpr = None; DCDValueList = Some ["1"; "3"; "5"];
                    //                                                                 FillN = None}
                    // makeTest "DCD" lineData "DCD1" "test DCD 1,3,5" (Some (Ok "1,3,5"))
                    // parseLabelIns root ls

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