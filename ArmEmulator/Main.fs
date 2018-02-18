module ArmEmulator

open Expecto
open Expecto.ExpectoFsCheck
open System
open MemInstructions

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
    printfn "enter any key to finish"
    Console.ReadKey() |> ignore
    0