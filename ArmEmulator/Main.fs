module ArmEmulator

open Expecto
open VisualTest
open System.IO
open Emulator.TopLevel
open CommonData
open VisualTest

/// reads the lines of a file into a string
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

/// converts a DataPath object to valid JSON as a string
let cpuToJson cpu = 
    let regsJson = 
        cpu.Regs 
        |> Map.toList
        |> List.map snd
        |> fun x -> (sprintf "%A" x).Replace("u","").Replace(";",",")
        |> sprintf "'regs': %s"
    let flagsJson = 
        (cpu.Fl.N, cpu.Fl.Z, cpu.Fl.C, cpu.Fl.V)
        |> fun (n,z,c,v)-> 
            sprintf "{'N': %A, 'Z': %A, 'C': %A, 'V':%A}" n z c v
        |> sprintf "'flags': %s"
    let memJson = 
        match Map.toList cpu.MM with
        | [] -> "'mem': {}"
        | lst -> 
            lst
            |> List.map (
                fun (WA a, x) ->
                    match x with
                    | DataLoc v -> sprintf "'%A': %A, " a v
                    | Code _ -> sprintf "'%A': '<someCode>'" a)
            |> List.reduce (+)
            |> fun x -> x.Replace("u","")
            |> sprintf "'mem': {%s}"
            |> fun x -> x.Replace(", }","}")
    (regsJson, flagsJson, memJson)
    |||> sprintf "{%s, %s, %s}" 

/// runs the emulator based on a source file in filePath
/// optionally uses flags and regs to initialise DataPath
let runFile filePath flags regs = 
    // initialise an empty datapath
    let cpu = 
        match initDataPath flags regs None with
        | Ok x -> x
        | Error _ -> failwithf "Should never happen."

    readLines filePath
    |> List.ofSeq
    |> List.filter (fun x -> x <> "")
    |> fun lines -> lines, cpu, Map.empty
    |||> parseThenExecLines
    |> function
        | Ok (cpu, _) -> 
            cpuToJson cpu
        | Error s -> 
            sprintf "%A" s

/// gets a Flags object from a string of 4 binary digits
let getFlags flagStr =
    let nums = int ("0b" + flagStr)
    let flagBool n = (nums &&& (1 <<< n)) > 0
    { 
        N = flagBool 3
        Z = flagBool 2
        C = flagBool 1
        V = flagBool 0
    }

/// gets a register map from a list of 16 comma separated integers
let getRegs (regStr: string) = 
    let regVals =
        regStr.Split(',')
        |> Array.toList
        |> List.map uint32
    [0..15]
    |> List.map (fun i -> (inverseRegNums.[i], regVals.[i]))
    |> Map.ofList

/// runs the emulator based on command line params
/// writes resulting JSON to file and returns 0
let emulate infile outfile flags regs =
    runFile infile flags regs
    |> fun result -> File.WriteAllText(outfile, result) 
    |> ignore
    printfn "Written results to %s" outfile
    0

let runTests argv = 
    Visual.initCaches VTest.defaultParas |> ignore
    Tests.runTestsInAssembly defaultConfig argv |> ignore
    Visual.finaliseCaches VTest.defaultParas |> ignore


/// with no command-line params, all tests are run
/// otherwise, give two parameters to emulate:
/// inputFilePath (path to a valid ARM source file)
/// outputFilePath (path to the output where JSON DataPath is written)
/// optional parameters can initialise flags and registers:
/// e.g. --flags 0101, sets NZCV to (false, true, false, true)
/// --regs takes a list of 16 comma separated integers
[<EntryPoint>]
let main argv =
    match Array.toList argv with
    /// if arguments given, must be input + output filepaths
    | [inFilePath; outFilePath] -> 
        emulate inFilePath outFilePath None None
    /// use flags as initial cpu state if given
    /// flags are given as 4 bits of binary: 0000 -> NZCV
    | [inFilePath; outFilePath; "--flags"; flagStr] -> 
        let flags = Some(getFlags flagStr)
        emulate inFilePath outFilePath flags None
    /// use regs as initial cpu state if given
    /// regs are given as 16 comma separated integers
    | [inFilePath; outFilePath; "--regs"; regStr] -> 
        let regs = Some(getRegs regStr)
        emulate inFilePath outFilePath None regs
    /// both flags and regs initialised (order matters)
    | [inFilePath; outFilePath; "--flags"; flagStr; "--regs"; regStr] -> 
        let flags = Some(getFlags flagStr)
        let regs = Some(getRegs regStr)
        emulate inFilePath outFilePath flags regs
    /// if no arguments given, run tests
    | [] -> 
        runTests argv |> ignore
        0
    | _ -> 
        printfn """
    Invalid usage. 
    Options are:
    1. No parameters 
        -> runs tests.
    2. [source file path] [output file path] 
        -> runs emulator on source file and outputs JSON.
    3. [source file path] [output file path] --flags [flags as binary e.g. 0101 (NZCV)] 
        -> runs emulator with initialised flags.
    4. [source file path] [output file path] --regs [list of 16 comma-separated integers] 
        -> runs emulator with initialised registers.
    5. [source file path] [output file path] --flags [binary digits] --regs [comma-separated integers]
    """
        1