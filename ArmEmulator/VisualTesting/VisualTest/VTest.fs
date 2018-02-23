namespace VisualTest
/// common types and low-level functions
/// provided for VisualTest Testbench
module VCommon =

    open System
    open System.Threading

    /// ARM Flags output from Visual run using postlude code via a register
    type Flags = {
        FN: bool
        FZ: bool
        FC: bool
        FV: bool
    }
   
    type TestSetup = { Asm: string }

    /// register name used for visual output
    type Out = 
       | R of int


    /// configuration data for running VisUAL
    /// 'D is DataPath type - used to make initial Datapth value equal to
    /// the initial machine state set up for VisUAL
    /// 'R is type of register names
    type Params = {
        Parallel: bool // if true run tests in parallel.
        MaxConcurrentVisualDirs: int // should only need same number as of cores
        Cached: bool  // If true use a cache to deliver instant results on repeat simulations
        VisualPath: string // the directory in which the downloaded VisUAL.exe can be found
        WorkFileDir: string // the directory in which both temporary files and the persistent cache file are put
        CacheFileName: string // The global cache name
        CacheLimit: int      // the max number of worker cache entries before the global cache is updated
        /// instructions to control inputs and outputs
        InitRegs: uint32 list
        InitFlags: Flags
        InitMem: Map<uint32, uint32> // Initial map of memory addr: data
        MemReadBase: uint32 //Read locations MemReadBase...MemReadBase+14 into registers R0
        MemReadDirection: string //which direction to read memory from, into registers, e.g. "IA"
        /// temporary data used to construct assembler
        Prelude: string
        Postlude: string

    }

   

    /// additional info extracted from Visual outputs by postlude.
    type VisState = {
        VFlags: Flags
        VMemData: uint32 list
        }

    /// info output by Visual
    type VisOutput = {
        Regs: (Out * int) list // registers after provided assembler
        RegsAfterPostlude: (Out * int) list // for processing postlude info
        State: VisState
        }

    type CacheT = { Dat: Map<string, ((Out*int) list * (Out*int) list)>; Limit: int}

    let mutable gLock: Semaphore array = [||]
    let mutable cLock: Semaphore = null
    let mutable dirUsedHint: bool array = [||]
    let mutable workerCache:  CacheT array = [||]
    let mutable GlobalCache: CacheT = { Dat = Map.empty ; Limit = 0}


module VData =

    open VCommon
    /// Generates assembler to set Reg n to value u.
    let SETREG (n:int) (u:uint32) =
        let setRegByte n b (u:uint32) =
            sprintf "MOV R%d, #0x%x\r\n" n (u <<< b*8)

        let addRegByte n b u =
            sprintf "ADD R%d, R%d, #0x%x\r\n" n n (u <<< b*8)

        if n < 0 || n >= 16 then 
            failwithf "Can't set register %d to %x because %d is out of range 0..15" n u n
        [0..3]
        |> List.collect (function | b when u &&& (0xFFu <<< b*8) <> 0u -> [(n , b,  (u >>> b*8) &&& 0xFFu)] | _ -> []
                                    | _ -> [])
        |> function
            | [] -> setRegByte n 0 0u
            | (n,b,u) :: rest -> 
                setRegByte n b u + (
                    rest 
                    |> List.map (fun (n,b,u) -> addRegByte n b u)
                    |> String.concat ""
                )

    /// Generates assembler which sets flags to value given by fl.        
    /// Uses R0 as temporary register
    let SETFLAGS (fl: Flags) = 

        let initNZ (fl:Flags) =     
            match fl.FN, fl.FZ with
            | false, false -> "MOVS R0, #1\r\n"
            | false, true -> "MOVS R0, #0\r\n"
            | true, false -> "MOVS R0, #-1\r\n"
            | true, true -> failwithf "What? This is an impossible flag combination: N=1, Z=1"

        let initCV (fl:Flags) =
            (match fl.FC, fl.FV with
            | true, true ->  "MOV R0, #0x80000000\r\n"
            | true, false -> "MOV R0, #0xFFFFFFFF\r\n"
            | false, true -> "MOV R0, #0x7FFFFFFF\r\n"
            | false, false -> "MOV R0, #0\r\n") +
            "ADDS R0, R0, R0\r\n"
            

        initCV fl + initNZ fl

    /// Generates assembler which
    /// loads memory location mAddr into register reg
    let LOADLOC (reg:int) (mAddr:uint32) =
        if mAddr < 0x100u then 
            failwithf "LOADMEM called with code section address: %x" mAddr
        SETREG reg mAddr + (sprintf "LDR R%d, [R%d]\r\n" reg mAddr)

    /// Generates assembler to store mDat in memory location mAddr
    /// Uses R0 and R2 as temporary registers
    let STORELOC (mDat:uint32) (mAddr: uint32) =
        // uses R2 and R3
        SETREG 0 mDat +
        SETREG 2 mAddr +
        "STR R0, [R2]\r\n"

    let SETMEM (initMem: Map<uint32, uint32>) =
        initMem
        |> Map.toList
        |> List.map (fun (m,x) -> STORELOC x m)
        |> String.concat "\r\n"
    
    /// Generates assembler which Sets registers R0-R14
    /// from the supplied list of values
    /// RegVals must be of length 15
    let SETALLREGS (regVals: uint32 list) =
        if regVals.Length <> 15 then
            failwithf "Error in SETALLREGS. regVals = %A is not length 15." regVals
        regVals
        |> List.indexed
        |> List.map (fun (n,v) -> SETREG n v)
        |> String.concat ""
    
    /// Read NZCV into bits 3210 of R0
    let READFLAGSINTOR0 =
    // use R0 as for output
           """MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
"""
    /// Read memBase..memBase+12 into R1..R12
    /// uses R13 as temporary register
    let READMEMORY (memBase:uint32) memDir =
        SETREG 13 memBase +
        sprintf "LDM%s R13, {R1-R12}\n" memDir

    /// Construct postlude assembly code
    /// memBase: base addr of memory locs read by postlude
    let POSTLUDE memBase memDir =
        READMEMORY memBase memDir + READFLAGSINTOR0


    /// Construct wrapper code for simulation
    /// regs, flags: init values of regs, flags
    ///    regs must be length 15 sets R0..R15
    /// memBase: location of block of memory words to read
    /// asm: assembly code to test
    /// returns (n, maincode, postlude)
    /// n is length of postlude
    let GETWRAPPER regs flags memBase initMem memDir =
        let main =
            SETMEM initMem +
            SETFLAGS flags +
            SETALLREGS regs +
            "\r\n"
        let post = POSTLUDE memBase memDir
        main, post
            
    /// processes after-postlude registers to extract additional state info
    /// as determined by postlude. E.g. flags from the standard postlude
    let decodeStateFromRegs outsAfter =
        /// Rn (n > 0) represents mem locations
        let memLocs = 
            outsAfter
            |> List.filter (fun (R n,_) -> n >= 0 && n < 13)
            |> List.map (fun (R _, v) -> uint32 v)
        let flagsInt = List.find (function | (R 0, _) -> true | _ -> false) outsAfter |> snd
        let flagBool n = (flagsInt &&& (1 <<< n)) > 0
        let flags =
            { 
                FN = flagBool 3
                FZ = flagBool 2
                FC = flagBool 1
                FV = flagBool 0
            }
        { VFlags = flags ; VMemData = memLocs}
            

/// Code to read/write structured data from Visual log file (XML) and Cache files
/// This does not use existing F# serialisers or type providers (that would do this nicely)
/// because that would introduce dependencies which at current state of F# ecosystem
/// reduce portability.
/// As a result, the code here is a one-off hack
/// no effort has been put into making this code general or maintainable

module VLog =

    open VCommon
    open VData
    open System
    open System.IO

    let sysNewline = System.Environment.NewLine

    /// used to parse log files using regex strings (primitive)
    let regexMatch (regex:string) (str:string) =
        let m = Text.RegularExpressions.Regex(regex).Match(str)
        if m.Success
        then
            let mLst = [ for x in m.Groups -> x.Value ]
            Some (List.head mLst, List.tail mLst)
        else None

    /// To do regex matching via pattern
    let (|MATCH|_|) = regexMatch

    
    /// detect register output from Visual log line
    let rgxReg = """<register name="R([([0-9]+)">(0x[0-9A-F]+)</register>"""

    /// Detect syntax error from Visual log line
    let rgxSyntaxError = """<syntax-error([^<]*)</syntax-error>"""

    /// detect runtime error from visual log line
    let rgxRuntimeError = """<runtime-error([^<]*)</runtime-error>"""

    /// Read an entire VisUAL log parsing it.
    /// Todo: reimplement using sequences and array reader for higher efficiency
    /// The cache size could be very large
    let readVisualLog n vLog =        
        let matches = 
            vLog
            |> Array.rev
            |> Array.collect (function
                | MATCH rgxSyntaxError (_, [e]) -> [|Error (e + sprintf "syntax error in\n:%A\n" vLog)|]
                | MATCH rgxRuntimeError (_, [e]) -> [|Error (e+ sprintf "runtime error in\n%A\n" vLog)|]
                | MATCH rgxReg (_,[rNum;rVal]) -> [|Ok (R (int rNum), int rVal)|]
                | _ -> [||])

        let errors = 
            matches
            |> Array.collect (function | Error e -> [|e|] |_ -> [||])
            |> Array.toList

        let regsAll =
            matches
            |> Array.collect (function | Ok x -> [|x|] | _ -> [||])
            |> Array.toList
        
        
        match errors with
        | [] when regsAll.Length >= 16*(1+n) -> 
            let regsReal = List.take 16 (List.skip (16*n) regsAll)
            let regsAfter = List.take 16 regsAll
            //printfn "Log:%A" regsReal
            Ok { 
            Regs = regsReal
            RegsAfterPostlude = regsAfter
            State =  decodeStateFromRegs regsAfter
            }
        | [] -> failwithf "No registers found in non-Error Visual log: %A" vLog
        | e -> Error e

    /// translate a text string for a custom log file escaping @ and '\n'
    /// those characters are used to delimit log entries
    let writeEscaped (txt:string) =
        txt.Replace("@","@@")
        |> (fun txt -> txt.Replace ("\n", "@n"))
        |> (fun txt -> txt.Replace ("\r","@r"))

    /// inverse of writeEscaped
    let readEscaped (line: string):string =
        let newl = sysNewline |> Seq.toList
        let rec rd s res =
            match s with
            | '@' :: '@' :: rest -> rd rest ('@' :: res)
            | '@' :: 'n' :: rest -> rd rest ('\n' :: res)
            | '@' :: 'r' :: rest -> rd rest ('\r' :: res)
            | '@' :: _ -> failwithf "What? unexpected '@' in coded string"
            | c :: rest -> rd rest (c :: res)
            | [] -> List.rev res
        rd (Seq.toList line) []
        |> List.toArray
        |> System.String

    /// write a specified set of output registers (not all) to a log file
    /// flags can be written as a pseudo-register (see Out type defn)
    /// in which case they are written as register no -1
    let writeRegs (outs:(Out*int) list) =
        outs
        |> List.map (function | R n,m -> sprintf "%d %d" n m )
        |> String.concat " "
    
    /// inverse of writeRegs
    let readRegs (str:string) =
        let s = str.Split([|' ';'\n';'\r';'\t'|],System.StringSplitOptions.RemoveEmptyEntries)
        [0..s.Length/2-1]
        |> List.map (fun n -> R (int s.[2*n]), int s.[2*n+1] )

    let writeVisOut (vso: VisOutput) =
        sprintf "%s,%s" (writeRegs vso.Regs) (writeRegs vso.RegsAfterPostlude)
  
    let readFileCacheItem ( srcS:string, regsS:string) =
        let txt,regs = readEscaped srcS, regsS.Split([|','|])
        let outs = regs |> Array.map readRegs
        let state = decodeStateFromRegs outs.[1]
        txt, {Regs = outs.[0] ; RegsAfterPostlude = outs.[1]; State = state}
        
    let writeFileCacheItem ( src:string, vso: VisOutput) =
        [| writeEscaped src ; writeVisOut vso |]
       
        


    /// read the entire cache from disk into a Map
    /// coalesce multiple identical entries
    /// cacheF: path of cache file on disk
    let readCache cacheF =
        let dat = File.ReadAllLines cacheF
        let n = dat.Length
        if n % 3 <> 0 then 
            failwithf "------\n%A\number of lines (%d) in cache not divisible by 3\n" dat n
        [|0..(n/3)-1|]
        |> Array.map (fun n -> readEscaped dat.[3*n], (readRegs dat.[3*n+1], readRegs dat.[3*n+2]))
        |> Array.distinct
        |> Map.ofArray


        
    /// add a single new item to the result cache
    /// scr: assembly source run
    /// outs: visual result info
    let appendCacheItem (cacheF:string) src (vso: VisOutput) =
        let outs = vso.Regs
        let outsAfterPostlude = vso.RegsAfterPostlude
        try
            File.AppendAllLines (cacheF, [| writeEscaped src ; writeRegs outs; writeRegs outsAfterPostlude|])
        with
            | e -> printfn "Append failed: %A" e; failwithf "What?"

    let appendMapToCache (cacheF:string) cacheToAppend =
        cacheToAppend
        |> Map.toSeq 
        |> Seq.collect (fun (src, (outs, outsAfter)) -> [ writeEscaped src ; writeRegs outs ; writeRegs outsAfter ])
        |> (fun s ->
                try
                    match File.Exists cacheF with
                    | true -> File.AppendAllLines (cacheF, s)
                    | false -> File.WriteAllLines(cacheF, s)
                with
                    | e -> printfn "Append failed: %A" e; failwithf "What?"
           )
        GlobalCache <- {Dat = readCache cacheF ; Limit = 0}

/// Code to run Visual from parallel expecto tests
/// Ccaheing is also provided so that second time round tests run much faster
module Visual = 
    open VCommon
    open VLog
    open VData

    open System.Threading
    open System.IO 
    open System
    
   
    let  VisualOpts = "--cycles --regs --syntax --runtime --mode:all "
    
    let VisualOptsMem memsize locs =
        let locshex = String.concat "," (List.map (sprintf "0x%x") locs)
        VisualOpts + (sprintf " --meminstsize:0x%x --custom:%s "  memsize locshex)
    
        
    /// run VisUAL via a separate command line process on source src
    /// returns output info as a list of registers and flags
    /// also returns register values after postlude
    let RunVisualBase (paras: Params) (src: string) : Result<VisOutput, string list>  = 
        let postludeLength = 
            paras.Postlude
            |> Seq.filter ((=) '\n') 
            |> Seq.length
        let visualJarExec vp = vp + @"jre\bin\java -jar "
        //printfn "\n\nVisual Temp files:%s\n" tempFilePath
        Directory.CreateDirectory paras.WorkFileDir |> ignore
        let runOpts = VisualOpts + sprintf " --meminstsize:0x%x " 0x1000
        let visualHeadlessExec srcFile outputFile opts = 
            (visualJarExec paras.VisualPath) + paras.VisualPath + @"content\visual_headless.jar --headless " + srcFile + " " + outputFile 
            + " " + opts
        let addQuotes s = sprintf "\"%s\"" s
        //printfn "Paths are: %s" visDir
        let srcF = Path.Combine(paras.WorkFileDir, "source.s")
        File.WriteAllText(srcF, src )
        //printfn "srcF=%s" srcF
        let outputF = Path.Combine(paras.WorkFileDir, "visoutput.log")
        let cmdArgs = "/C " + (addQuotes <| visualHeadlessExec srcF outputF runOpts)
        //printfn "%s" cmdArgs
        File.WriteAllText(paras.WorkFileDir + "comstr.txt", cmdArgs)
        try 
            let proc = System.Diagnostics.Process.Start("cmd", cmdArgs)
            proc.WaitForExit()
        with e -> ()//printfn "%s" e.Message
        let visLog = File.ReadAllLines outputF
        let visOutput = readVisualLog postludeLength visLog
        let recordError e = 
            let mess = sprintf "\n>>---SOURCE:\n%s\n-------\n\n>>>>>>>>>>>>>>>>>>>>>>\n\n%A\n----------------------\n" src "none"
            printfn "%s" mess
            File.AppendAllText( paras.WorkFileDir+"\\VisualErrors", String.concat "\n" e)
        match visOutput with
        | Error e -> recordError e
        | _ -> ()
        visOutput

    /// update global cacahe on disk with items in worker-local cache in memory
    let UpdateCache n (paras:Params) (src:string) (visResults: Result<VisOutput, string list>) = 
        match visResults with
        | Error e -> ()
        | Ok vOkRes ->
            let cacheFName = paras.CacheFileName
            let cache = workerCache
            cache.[n] <- {cache.[n] with Dat = cache.[n].Dat.Add(src, (vOkRes.Regs, vOkRes.RegsAfterPostlude))}
            if cache.[n].Dat.Count > cache.[n].Limit then
                cLock.WaitOne() |> ignore
                appendMapToCache cacheFName cache.[n].Dat
                cache.[n] <- { Dat = Map.empty; Limit = paras.CacheLimit}
                cLock.Release() |> ignore

    /// all-purpose Visual test run, with parallel execution and cacheing supported
    let RunVisualBaseWithLocksCached (paras: Params) src =
        let srcWithWrapper = paras.Prelude + "\r\n" + src + "\r\n" + paras.Postlude
                
        let rec prepDir' n =
            let dir = sprintf "%sd%d/" paras.WorkFileDir n
            let workerParas = {paras with WorkFileDir = dir}
            gLock.[n].WaitOne() |> ignore
            dirUsedHint.[n] <- true
            Directory.CreateDirectory dir |> ignore
            let res =
                match paras.Cached && GlobalCache.Dat.ContainsKey srcWithWrapper with
                | true  -> 
                    let co = GlobalCache.Dat.[srcWithWrapper]
                    Ok { Regs = fst co; RegsAfterPostlude = snd co; State = decodeStateFromRegs (snd co)}
                | false ->                     
                    let visRes = RunVisualBase workerParas srcWithWrapper
                    if paras.Cached then UpdateCache n workerParas srcWithWrapper visRes
                    visRes
                  
            dirUsedHint.[n] <- false
            gLock.[n].Release() |> ignore
            res

        let possN = 
            [0..paras.MaxConcurrentVisualDirs-1]
            |> List.filter (fun n -> dirUsedHint.[n] = false)

        match possN with
        | n :: _ -> prepDir' n
        | [] -> printfn "WARNING: all Visual directories are in use: waiting for directory"
                prepDir' 0
    /// initialise caches and semaphores for correct cached parallel operation
    /// called before any VisuAL process is started
    let initCaches (paras:Params) = 
        cLock <- new Semaphore(1,1,"visualCache")
        gLock <- 
            [|0..paras.MaxConcurrentVisualDirs-1|]
            |> Array.map (fun n -> new Semaphore(1,1,sprintf "visualDir%d" n))
        dirUsedHint <- Array.replicate paras.MaxConcurrentVisualDirs false
        workerCache <- Array.replicate paras.MaxConcurrentVisualDirs { Dat = Map.empty; Limit = paras.CacheLimit }
        if File.Exists paras.CacheFileName then 
            GlobalCache <- {Dat = readCache  paras.CacheFileName; Limit = 0}
            File.Delete paras.CacheFileName
            appendMapToCache paras.CacheFileName GlobalCache.Dat
    /// top-level run Visual function
    let RunVisual (paras: Params) src =
        Directory.CreateDirectory paras.WorkFileDir |> ignore
        RunVisualBaseWithLocksCached paras src
           
    /// Adds postlude to assembly code to detect flags values.
    /// Returns flags , registers (before flag detection code)
    let RunVisualWithFlagsOut paras src =
        let main, post = 
            VData.GETWRAPPER 
                paras.InitRegs paras.InitFlags paras.MemReadBase paras.InitMem paras.MemReadDirection
        let res = RunVisual {paras with Prelude=main; Postlude=post} src
        match res with
        | Error e -> failwithf "Error reading Visual Log %A" e
        | Ok ({ Regs=_; State={VFlags=fl; VMemData=mem}} as vso) -> fl, vso, mem
 
    /// convenience function, convert 4 char string to NZCV status flag record
    let strToFlags s =
        let toBool = function | '0' -> false | '1' -> true | s -> failwithf "Bad character in flag specification '%c'" s
        match s |> Seq.toList |> List.map toBool with
        | [ a ; b ; c ; d] -> { FN=a; FZ=b;FC=c;FV=d}
        | _ -> failwithf "Wrong number of characters (should be 4) in flag specification %s" s


                
    /// flush caches writing all info to global cache
    /// called after all VisUAL processes have terminated
    let finaliseCaches (paras:Params) =
        let cMap mArr =
            Array.collect (fun {Dat=m} -> Map.toArray m) mArr
            |> Array.distinct
            |> Map.ofArray
        workerCache
        |> cMap
        |> appendMapToCache paras.CacheFileName



/// top-level code demonstrating how to run tests

module VTest =

    open Expecto
    open VCommon
    open VLog
    open Visual
    open System.Threading
    open System.IO

    /// parameters setting up the testing framework
    /// WARNING: PostludeLength must be changed if Postlude is changed
    /// WARNING: global cache (CacheFileName) must be deleted if Postlude is changed
    /// Postlude can contain instructions to move CPU state (flags, memory locations) into rgeisters
    /// standard Postlude moves flags into R1
    /// Simulation reads VisUAL log and returns registers after test assembly code, and also after postlude
    let defaultParas = {
        Parallel = true              // parallel testing is now supported!
        MaxConcurrentVisualDirs = 10 // should only need the same number as of cores
        Cached = true                // true if results are stored in a cache on disk and reused to speed 
                                     // up future repeat simulations
        VisualPath =  
            @"\Users\dirkj\Desktop\fsharp-arm-emulator\ArmEmulator\VisualTesting\visualapp\visual\"  // the directory in which the downloaded VisUAL.exe can be found
        WorkFileDir = 
            @"\Users\dirkj\Desktop\fsharp-arm-emulator\ArmEmulator\VisualTesting\visualWork\"        // the directory in which both temporary files and the persistent cache file are put
        CacheFileName = 
            @"\Users\dirkj\Desktop\fsharp-arm-emulator\ArmEmulator\VisualTesting\visualWork\Cache"   // the file name of the global cache
        CacheLimit = 10               // the number of results before adding to global cache
        InitFlags = {FN=false;FZ=false; FC=false;FV=false}
        InitRegs = [0u..10u..140u]          // initial values of registers R0..R14
        InitMem = Map.empty // initial values of memory
        MemReadBase = 0x1000u          // locations read from memory (currently 13 consecutive words are read)
        MemReadDirection = "IA"
        Postlude = ""                 // this is overwritten by code
        Prelude = ""                  // this is overwritten by code
    } 
    
    
    /// run an expecto test of VisUAL
    /// name - name of test
    ///
    let VisualUnitTest paras name src (flagsExpected:string) (outExpected: (Out * int) list) =
        testCase name <| fun () ->
            let flagsActual, outActual, memActual = RunVisualWithFlagsOut paras src
            (printfn "%A" memActual)
            Expecto.Expect.equal flagsActual (flagsExpected |> strToFlags) "Status flags don't match"
            let outRegsNoted = 
                outExpected 
                |> List.map fst
            let outActualNoted = 
                outActual.Regs 
                |> List.filter (fun (r,_) -> List.contains r outRegsNoted)
                |> List.sort
            Expecto.Expect.equal outActualNoted (outExpected |> List.sort) <|
                sprintf "Register outputs>\n%A\n<don't match expected outputs, src=%s" outActual.Regs src

    let VisualFrameworkTest paras =
        testCase "Framework test failed" <| fun () ->
            let parasExpected = 
                paras.InitRegs
                |> List.indexed
                |> List.map (fun (n,v) -> R n, int v)

            let flagsActual, outActual, memActual = RunVisualWithFlagsOut paras ""
            let outSorted = 
                outActual.Regs
                |> List.sort
                |> List.take 15
            Expecto.Expect.equal flagsActual  paras.InitFlags "Status flags don't match"
            Expecto.Expect.equal outSorted parasExpected <|
                sprintf "Register outputs>\n%A\n<don't match expected outputs" outActual.Regs


    type rType = {
        R0:int;R1:int;R2:int;R3:int;R4:int;R5:int;R6:int;R7:int;
        R8:int;R9:int;R10:int;R11:int;R12:int;R13:int;R14:int
    }

    let rType2List (r:rType)=
        [r.R0;r.R1;r.R2;r.R3;r.R4; r.R5;r.R6;r.R7;
         r.R8;r.R9;r.R10;r.R11;r.R12;r.R13;r.R14]
      

    let VisualFrameworkRun (regs: rType,flags:Flags) =
        let performTest() =
            let initRegs = 
                rType2List regs
                |> List.map uint32
        
            let expectedRegs =
                initRegs
                |> List.indexed
                |> List.map (fun (n,v) -> R n, int v)

            let flagsActual, outActual, memActual = 
                    RunVisualWithFlagsOut { 
                        defaultParas with 
                            InitFlags=flags;
                            InitRegs=initRegs
                        } ""
            let actualRegs = 
                outActual.Regs
                |> List.sort
                |> List.take 15
            let flagsOK = flagsActual = flags
            let regsOK = actualRegs = expectedRegs 
            if not flagsOK then 
                printfn "Framework error: Bad flags: %A" flagsActual
                System.Console.ReadKey() |> ignore
            if not regsOK then 
                printfn "Framework error: Bad registers %A" actualRegs
                System.Console.ReadKey() |> ignore
            flagsOK && regsOK
        match flags with
        | {FN=true;FZ=true} -> true // prevent test with imposisble input
        | _ -> performTest()
            
    let testParas = defaultParas
 

    let vTest = VisualUnitTest testParas



    /// to test the testbench, create many tests with assembler
    /// this is enough for each test to need being run separately
    
    let manyTests n = 
        [0..n] 
        |> List.map (fun n -> 
            let n' = 1 + (n % 254)
            vTest (sprintf "SUBS%d test" n') (sprintf "SUBS R0, R0, #%d" n') "1000" [R 0, -n'])

    let many = testList "Many pointless tests" (manyTests 10)

    /// implements random property-based tests of the framework
    /// tests that read/write of registers and flags is consistent for random
    /// input values
    let frametests =        
        let fsConfig = {
                FsCheckConfig.defaultConfig with
                    replay = Some (0,0) // seed for RNG. Means that the same tests are done each run
                                        // replace by None for a random time-based seed and therefore
                                        // new tests each time that will not cache
                    maxTest = 100       // number of random tests
                }
        testPropertyWithConfig fsConfig "Flags and registers are preserved" VisualFrameworkRun


    let tests = 
        testList "Minimal Visual Unit Tests"
            [
            VisualFrameworkTest defaultParas
            vTest "SUB test" "SUB R0, R0, #1" "0000" [R 0, -1]
            vTest "SUBS test" "SUBS R0, R0, #0" "0110" [R 0, 0]
            // vTest "This ADDS test should fail" "ADDS R0, R0, #4" "0000" [R 0, 4; R 1, 0] 
            // R1 should be 10 but is specified here as 0
            ]

