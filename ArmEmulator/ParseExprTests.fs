module ParseExprTests
    open Expecto
    open ParseExpr
    open MultMemTests

    [<Tests>]
    let testEvalExprUnit =
        let testSymTab = [("test1",10u);("test2",23u)] |> Map.ofList 
        makeUnitTestList (evalExpr testSymTab) "evalExpr Unit"
           [
                // test individual operators
                "5*5", 5u*5u
                "12/4", 12u/4u
                "17+94", 17u+94u
                "97-6", 97u-6u
                // test combining multiple operators
                "97-6+7", 97u-6u+7u
                "97-6+7-19", 97u-6u+7u-19u
                "0-1", 0u-1u
                "10*0", 10u*0u
                "1-0", 1u-0u
                "2*6+3", 2u*6u+3u
                // test brackets
                "2*(6+3)", 2u*(6u+3u)
                "(8/2)*(6+3)", (8u/2u)*(6u+3u)
                "2*(6+3)*5", 2u*(6u+3u)*5u
                "2*(6+(3*4)-(6+3))*5", 2u*(6u+(3u*4u)-(6u+3u))*5u
                "(2*(6+3)*5)+(5*4)", (2u*(6u+3u)*5u)+(5u*4u)
                // test labels
                "test2*test1", 23u*10u
                "(test1*(6+3)*5)+(5*4)", (10u*(6u+3u)*5u)+(5u*4u)
                "(test1*test2)", 10u*23u
            ]