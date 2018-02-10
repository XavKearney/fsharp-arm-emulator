module ArmEmulator

open Expecto
open Expecto.ExpectoFsCheck
open System
open System.Xml.Linq
open System.Xml.Linq
// open memInstructions

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
    printfn "enter any key to finish"
    Console.ReadKey() |> ignore
    0