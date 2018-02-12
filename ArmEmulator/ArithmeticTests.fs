module MultMemTests
    open CommonData
    open CommonLex
    open Arithmetic
    open Expecto
    open FsCheck

    /// take a function f, test name
    /// and list of (input, output) tuples
    /// create an Expecto testList
    /// with unit tests, testing each case
    let makeTestList testFun name testVals =
        let makeTest inp outp =
            let testName = (sprintf "%s: %A" name inp)
            testCase testName <| fun () ->
                Expect.equal (testFun inp) outp testName
        List.map (fun (input, output) -> makeTest input output) testVals
        |> testList (sprintf "%s Test List" name) 


    [<Tests>]
    let testParseOpsLine = 
        makeTestList parseOpsLine "parseOpsLine Unit Tests" 
            [
                ("R7,R3,R9", Ok (R7, R3, Target R9));
                ("R0, R12, R1", Ok (R0, R12, Target R1));
                ("R,R3,R9", Error "Destination register not valid");
                ("R7, R20, R9", Error "Op1 is not a valid register");
                ("R7, R3, R20", Error "Op2 is not a valid register");
            ]