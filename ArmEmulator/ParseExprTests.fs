module ParseExprTests
    open Expecto
    open ParseExpr
    open MultMemTests

    [<Tests>]
    let testEvalExprUnit =
        let testSymTab = [("test1",1u);("test2",2u)] |> Map.ofList 
        makeUnitTestList (evalExpr testSymTab) "evalExpr Unit"
           [
                "5*5", 5u*5u
                "17+94", 17u+94u
                "97-6", 97u-6u
                "97-6+7", 97u-6u+7u
                "97-6+7-19", 97u-6u+7u-19u
                "0-1", 0u-1u
                "10*0", 10u*0u
                "1-0", 1u-0u
                "2*6+3", 2u*6u+3u
                "2*(6+3)", 2u*(6u+3u)
                "2*(6+3)*5", 2u*(6u+3u)*5u
                "2*(6+(3*4)-(6+3))*5", 2u*(6u+(3u*4u)-(6u+3u))*5u
                "(2*(6+3)*5)+(5*4)", (2u*(6u+3u)*5u)+(5u*4u)
            ]