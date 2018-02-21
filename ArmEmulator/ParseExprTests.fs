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
                "5*63", Ok(5u*63u)
                "111*0", Ok(111u*0u)
                "1*000", Ok(1u*000u)
                "17+94", Ok(17u+94u)
                "97-6", Ok(97u-6u)
                "-97+1", Ok(0u-97u+1u)
                "+97+1", Ok(97u+1u)
                // test combining multiple operators
                "97-6+7", Ok(97u-6u+7u)
                "97-6+7-19", Ok(97u-6u+7u-19u)
                "0-1", Ok(0u-1u)
                "10*0", Ok(10u*0u)
                "1-0", Ok(1u-0u)
                "2*6+3", Ok(2u*6u+3u)
                // test brackets
                "2*(6+3)", Ok(2u*(6u+3u))
                "(8-2)*(6+3)", Ok((8u-2u)*(6u+3u))
                "2*(6+3)*5", Ok(2u*(6u+3u)*5u)
                "2*(6+(3*4)-(6+3))*5", Ok(2u*(6u+(3u*4u)-(6u+3u))*5u)
                "-2*(6+(3*4)-(6+3))*5", Ok(0u-2u*(6u+(3u*4u)-(6u+3u))*5u)
                "(2*(6+3)*5)+(5*4)", Ok((2u*(6u+3u)*5u)+(5u*4u))
                // test labels
                "test2*test1", Ok(23u*10u)
                "test2-test1", Ok(23u-10u)
                "test2+test1", Ok(23u+10u)
                // test bracketed expressions with labels
                "(test1*test2)", Ok(10u*23u)
                "(test1*(6+3)*5)+(5*4)", Ok((10u*(6u+3u)*5u)+(5u*4u))
                "(test1*(6+test2)*5)+(test1*4)-3", Ok((10u*(6u+23u)*5u)+(10u*4u)-3u)
                // test hex expressions
                "0x55*3-0xAF", Ok(0x55u*3u-0xAFu)
                "0x01-3*0xFFFFFFFF", Ok(0x01u-3u*0xFFFFFFFFu)
                "&D1+1*0xFFFFFFFF", Ok(0xD1u+1u*0xFFFFFFFFu)
                "&A4*0xABCDEF", Ok(0xA4u*0xABCDEFu)
                "&A4-&ABCDEF", Ok(0xA4u-0xABCDEFu)
                "0xFFFFFFFF+1", Ok(0xFFFFFFFFu+1u)
                // test binary expressions
                "0b1111100101+0b01010011", Ok(0b1111100101u+0b01010011u)
                "0b11101010101-0b0110010011", Ok(0b11101010101u-0b0110010011u)
                "0b10101010101*0b100010011", Ok(0b10101010101u*0b100010011u)
                "0b10101010101*19", Ok(0b10101010101u*19u)
                "0b10101010101*0xFFA9", Ok(0b10101010101u*0xFFA9u)
                // combine everything
                "-3*(test1+test2)*(0xFF-&A*0b110011*(test2-0xABC*test1))",
                    Ok(0u-3u*(10u+23u)*(0xFFu-0xAu*0b110011u*(23u-0xABCu*10u)))
                // test invalid input
                "2**1", Error "Invalid expression."
                "2*()", Error "Invalid expression."
                "2*(3*()+79)", Error "Invalid expression."
                "2*-4", Error "Invalid expression."
                "0xZG*12-(4+1)", Error "Invalid literal in expression."
                "&AM-4", Error "Invalid literal in expression."
                "-0b1021-test1", Error "Invalid literal in expression."
                "0xFG", Error "Invalid literal at end of expression."
                "&FG", Error "Invalid literal at end of expression."
                "0b112", Error "Invalid literal at end of expression."
            ]