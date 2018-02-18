module ArmEmulator

open Expecto
open VisualTest

[<EntryPoint>]
let main argv =
    Visual.initCaches VTest.defaultParas |> ignore
    Tests.runTestsInAssembly defaultConfig argv |> ignore
    Visual.finaliseCaches VTest.defaultParas |> ignore
    0
    
