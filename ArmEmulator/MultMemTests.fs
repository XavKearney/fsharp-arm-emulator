module MultMemTests
    open CommonData
    open CommonLex
    open MultMem
    open Expecto
    open FsCheck

    [<Tests>]
    let testParse =
        testCase "Test Parse" <| fun () ->
        let ls = {
            LoadAddr = WA 100u; 
            Label = None; 
            SymTab = None;
            OpCode = "LDM";
            Operands = "R4, {R5,R7,R12}";
        }
        let expected = Some (Ok {
                    PInstr =  { InsType = Some(LDM); Direction = Some(FD);
                    Target = R4; WriteBack = false; RegList = [R5;R7;R12]} ;
                    PLabel = None; PSize = 4u; PCond = Cal;
                })
        let res = parse ls
        Expect.equal res expected "parse1"