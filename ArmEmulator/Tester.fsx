open System.Text.RegularExpressions

//module CommonData
//////////////////////////////////////////////////////////////////////////////////////
//                   Common types and code used by all modules
//////////////////////////////////////////////////////////////////////////////////////

/// ARM Status bits
type Flags = { N: bool; C:bool; Z: bool; V:bool}


////////////////////////ARM register names and operations/////////////////////////////


/// ARM register names
/// NB R15 is the program counter as read
/// This could be a [<Struct>] but causes build errors under Linux
type RName = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 
             | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15




/// Map used to convert strings into RName values, 
/// includes register aliasses PC, LR, SP
let regNames = 
    Map.ofList [ 
        "R0",R0 ; "R1",R1 ; "R2",R2 ; "R3",R3 ; "R4",R4 ; "R5",R5
        "R6",R6 ; "R7",R7 ; "R8",R8 ; "R9", R9 ; "R10",R10 ; "R11",R11 ; 
        "R12",R12 ; "R13",R13 ; "R14",R14 ; "R15",R15 ; 
        "PC",R15 ; "LR",R14 ; "SP",R13 
    ] 

// various functions used to convert between string, RName, and register number

/// Inverse of regNames, used to convert RName values to strings
/// NB The string chosen will always be the register (not alias)
let regStrings = 
    regNames
    |> Map.toList
    |> List.map (fun (s,rn)-> (rn,s)) 
    |> List.filter (fun (_,s:string) -> s.StartsWith "R")
    |> Map.ofList

/// Map converts RName into register number (no aliasses)
let regNums = Map.map (fun _ (s:string) -> int (s.[1..])) regStrings

/// Map converts register number into RName (no aliasses)
let inverseRegNums = 
    regNums |> Map.toList 
    |> List.map (fun (rn,n)->(n,rn)) |> Map.ofList

/// Property on RName to return register number, for convenience
/// Aliasses not included, since they are not RNames
type RName with
    /// Return the number of a register as an integer
    member r.RegNum = regNums.[r]

/// Return a register name from an integer
let register n = 
    if 0 <= n && n < 16 
    then inverseRegNums.[n] 
    else (failwithf "Register %d does not exist!" n)

/// Type to represent the contents of one memory location
/// 'INS is a parameter set to the type of an instruction
/// needed because instruction type is only defined
/// at top level.
type MemLoc<'INS> =
    | DataLoc of uint32
    | Code of 'INS

/// type to represent a (word) address
/// there is some ambiguity. Does this contain the real address
/// which is always divisible by 4
/// or does it contain the word number (real address dvided by 4)
/// either way multiply/divide by 4 will cause problems!
/// document this well and be consistent.
type WAddr = | WA of uint32

/// type to represent memory
type MachineMemory<'INS> = Map<WAddr,MemLoc<'INS>>

/// ARM state as values of all registers and status bits
/// NB PC can be found as R15 - 8. (Pipelining)
type DataPath<'INS> = {
    Fl: Flags; // Flags
    Regs:Map<RName,uint32> // map representing registers. 
                           // Must be correctly initialised
    MM: MachineMemory<'INS> // map showing the contents of all memory
    }






//module CommonLex

/// ARM execution conditions
type Condition =

    | Ceq
    | Cne
    | Cmi
    | Cpl
    | Chi
    | Chs
    | Clo
    | Cls
    | Cge
    | Cgt
    | Cle
    | Clt
    | Cvs
    | Cvc
    | Cnv // the "never executed" condition NV - not often used!
    | Cal // the "always executed condition "AL". Used by default on no condition

/// classes of instructions (example, add/change this is needed)
// TODO: sort these out.
type InstrClass = | DP | MEM | ARITH | BITARITH | COMP | ADR | LABEL | MISC


/// specification of set of instructions
type OpSpec = {
    InstrC: InstrClass
    Roots: string list
    Suffixes: string list
}

type SymbolTable = Map<string,uint32>

/// result returned from instruction-specific module parsing
/// an instruction class. If symbol definitions are found in a 
/// symbol table then a complete parse will be output
/// otherwise some fields will be None
type Parse<'INS> = {
        /// value representing instruction. NB type varies with instruction class
        PInstr: 'INS 
        /// name and value of label defined on this line, if one is.
        PLabel: (string * uint32) option 
        /// number of bytes in memory taken up by this instruction
        PSize: uint32 
        /// execution condition for instruction
        PCond: Condition
    }

/// data given to instruction-specific parse function
type LineData = {
    /// memory address this instruction is loaded. Must be word address
    LoadAddr: WAddr 
    /// name of label defined on this line, if one exists
    Label: string option 
    /// table of symbols with defined values. 
    /// if this is given we are phase 2 and all symbols should be defined
    /// if this is not given we are phase 1 and no symbols are defined
    SymTab: SymbolTable option
    /// opcode string
    OpCode: string
    /// string of all the operands
    Operands: string
}


/// Strings with corresponding execution condition
/// Note some conditions have multiple strings
/// Note "" is a valid condition string (always execute condition)
let condMap = [ "EQ",Ceq ; "NE",Cne ; "MI",Cmi ; "PL",Cpl ; "HI", Chi ; 
                "HS",Chs ; "LO",Clo ; "LS",Cls ; "GE",Cge ; "GT", Cgt ; 
                "LE", Cle ; "LT", Clt ; "VS",Cvs ;  "VC",Cvc ;
                "NV",Cnv ; "AL",Cal ; "",Cal; "",Cal] |> Map.ofList

/// list of all strings representing execution conditions
/// includes ""
let condStrings = 
    condMap
    |> Map.toList
    |> List.map fst
    |> List.distinct    

/// generate all possible opcode strings for given specification
/// each string is paired with info about instruction
/// and the three parts of the opcode
let opCodeExpand (spec: OpSpec) 
    //    opcode    class        root    suffix   instr cond
    : Map<string, InstrClass * (string * string * Condition)> =
    spec.Roots
    |> List.collect (fun r -> 
        spec.Suffixes
        |> List.collect (fun s -> 
            condStrings
            |> List.map (fun c -> r+s+c, (spec.InstrC,(r,s, condMap.[c])))))
            |> Map.ofList

/// function used to change PInstr field of a Result<Parse<'INS>,'E>
/// the output has this field mapped with fMap
/// or if Error has this value chnaged by fMapE
let pResultInstrMap fMap fMapE paRes =
    match paRes with
    | Ok ({PInstr=ins} as pr) -> 
        // Note subtle point. {pr with Pinst = ...} will not work here
        // That is because applying fmap changes the type of PInstr
        // and therefore the type of the record.
        Ok {
        PInstr = fMap ins 
        PLabel = pr.PLabel
        PCond = pr.PCond
        PSize = pr.PSize
        }
    | Error e -> Error (fMapE e)






/// this module implements a parser for mathematical expressions
/// it supports multiply, add & subtract with 
/// literals in normal, binary (0b) or hex (0x/&) form
/// as well as labels, where the label is converted into its address
//module ParseExpr

/// ----------- ACTIVE PATTERNS ---------------
/// Some of these are used in this module, some are used elsewhere
/// As they are general purpose, this is where they live

/// takes a string and ensures it starts and ends with
/// a given prefix and suffix
/// if so, returns the string inside
let (|GetInside|_|) (prefix:string) (suffix: string) (str:string) =
    if (str.StartsWith(prefix) && str.EndsWith(suffix))
    then Some (str.[1..String.length str - 2])
    else None

/// checks if a string contains a character
/// if so, split at that character and return list
let (|SplitAt|_|) (c:char) (str:string) =
    if str.Contains(c.ToString()) 
    then Some(str.Split(c) |> Seq.toList)
    else None

/// matches a string with regex pattern
/// returns a list of all of the matches
let (|Matches|_|) (pat:string) (inp:string) =
    let m = Regex.Matches(inp, pat) in
    if m.Count > 0
    then Some ([ for g in m -> g.Value ])
    else None

/// matches a string with regex pattern
/// if there is a single match, it is returned
let (|Match1|_|) (pat:string) (inp:string) =
    let m = Regex.Matches(inp, pat) in
    if m.Count = 1
    then Some (m.[0].Value)
    else None

/// matches a string with regex pattern
/// returns list of the matched groups (excluding the whole match)
let (|MatchGroups|_|) (pat:string) (inp:string) =
    let m = Regex.Matches(inp, pat) in
    if m.Count > 0
    then 
        [ for x in m -> x.Groups ]
        |> List.collect (fun x -> [for y in x -> y.Value])
        |> List.tail // remove the whole matched string
        |> Some 
    else None

/// matches a literal in hex form (0x.. or &..),
/// in binary form (0b..) or as a standard number
/// if no match, try and find the string in the SymbolTable
let (|Literal|_|) (symTab:SymbolTable) (inp:string) =
    match inp with
    | Match1 @"^(0[xX][a-fA-F0-9]+)$" x -> uint32 x |> Some
    | Match1 @"^(&[a-fA-F0-9]+)$" x -> "0x"+x.[1..] |> uint32 |> Some
    | Match1 @"^(0b[0-1]+)$" x -> uint32 x |> Some
    | Match1 @"^([0-9]+)$" x -> uint32 x |> Some
    | label -> symTab.TryFind label

let (|Contains|_|) element lst =
    match List.contains element lst with
    | true -> Some lst
    | false -> None

/// ----------- END ACTIVE PATTERNS ---------------


type Operator = 
    | Add
    | Sub
    | Mul

type Token = 
    | Num of uint32
    | LBra
    | RBra
    | Op of Operator

let tokenize expr (symTab: SymbolTable) =
    let recIfOk a f x =
        match f x with
        | Ok y -> Ok (a :: y)
        | Error s -> Error s
    let rec tok' cLst = 
        match cLst with
        // if the charlist starts with an operator, match it
        | '+' :: rest -> recIfOk (Op Add) tok' rest
        | '-' :: rest -> recIfOk (Op Sub) tok' rest
        | '*' :: rest -> recIfOk (Op Mul) tok' rest
        | '(' :: rest -> recIfOk (LBra) tok' rest
        | ')' :: rest -> recIfOk (RBra) tok' rest
        | ' ' :: rest -> tok' rest
        // if empty, we're done
        | [] -> Ok []
        // otherwise, the next item must be a literal (number or label)
        | chrLst -> 
            // convert the remaining characters into a string
            chrLst |> List.toArray |> System.String |> 
            function
            // match if the only thing remaining is a literal
            | Match1 @"^([&a-zA-Z0-9]+)$" x -> 
                match x with
                | Literal symTab n -> Ok [Num n]
                | _ -> Error "Invalid literal at end of expression."
            // otherwise, match the label/number up to the next operator
            | MatchGroups @"^([&a-zA-Z0-9]+)(.+)$" (txt :: [rest]) ->
                match txt with
                | Literal symTab n -> recIfOk (Num n) tok' (Seq.toList rest)
                | _ -> Error "Invalid literal in expression."
            | _ -> Error "Invalid expression."
    tok' (Seq.toList expr)


let evalTokenized tokInput =
    let doOp op right left = 
        match op with
        | Op Add -> left + right
        | Op Sub -> left - right
        | Op Mul -> left * right
        | _ -> failwithf "Invalid operator."
    
    let first2 (lst: 'a list) = lst.Head, lst.Tail.Head, lst.Tail.Tail
    
    let rec eval' toks nums ops =
        match toks with
        // if a number, put in the number stack
        | Num n :: rest -> eval' rest (n::nums) ops
        | Op op :: rest -> 
            // comparison of ops determines which comes first
            // needed if multiple pending operations, to get order correct
            match ops <> [] && ops.Head > (Op op) with
            // operator should be applied now
            | true when nums.Length >= 2 ->
                let first, second, remaining = first2 nums
                doOp ops.Head first second
                |> fun res -> eval' rest (res::remaining) (Op op::ops.Tail)
            | false -> 
                match nums, ops with
                // if it starts with an operator, put in a 0 at the beginning
                | [], [] -> eval' rest (0u::nums) (Op op::ops)
                // otherwise just add the operator to the stack
                | _ -> eval' rest nums (Op op::ops)
            | _ -> Error "Invalid expression."
        // open a bracketed expression
        | LBra :: rest -> eval' rest nums (LBra::ops)
        // close a bracketed expression
        | RBra :: rest ->
            match ops with
            // evaluate what was in the brackets
            | Op op :: restOps ->
                let first, second, remaining = first2 nums
                doOp (Op op) first second
                |> fun res -> eval' toks (res::remaining) restOps
            // handle nested brackets
            | LBra :: restOps -> eval' rest nums restOps
            | _ -> Error "Invalid expression."
        // if nothing left
        | [] -> 
            match ops with
            // perform any remaining operations
            | Op op :: restOps when nums.Length >= 2->
                let first, second, remaining = first2 nums
                doOp (Op op) first second
                |> fun res -> eval' [] (res::remaining) restOps
            // otherwise return the result
            | [] -> Ok nums.Head
            | _ -> Error "Invalid expression."
    match tokInput with
    | [] -> Error "No input expression supplied."
    | _ -> eval' tokInput [] []

/// main function to interface with the parser
/// takes a symbol table and expression string
/// returns a Result which is either the evaluated
/// expression as a number, or an error string
let evalExpr symTab expr = 
    tokenize expr symTab
    |> Result.bind evalTokenized






// check litteral 
/// checks if an integer can be created by rotating an 8 bit number in a 32 bit word 
let allowedLiterals num =
    let valid =
        [0..2..30] 
        |> List.allPairs [(uint32 num)] 
        |> List.map (fun (n,r) -> (n >>> r) ||| (n <<< (32-r)))
        |> List.collect (fun n -> [n < 256u])
        |> List.contains true
    match num with
    | x when ((x <= 2147483647) && (x >= -2147483648)) ->
        match valid with 
        | true -> Ok (uint32 num)
        | false -> Error "Litteral can't be created by rotating an 8 bit number in a 32 bit word "
    | _ -> Error "Litteral is out of range"

/// converts string to some litteral or none
/// string number must start with #
let toLit (str : string) (symTab : SymbolTable) =  
    match str.Length=0 with 
    | false -> 
        match str.[0] with
        | '#' -> 
            match evalExpr symTab str.[1..] with
            | Ok num when (int num) >= 0 -> allowedLiterals (int num)
            | Ok num -> 
                match allowedLiterals (~~~ (int num)) with
                | Ok _ -> Ok num
                | _ -> Error "This litteral is not allowed"
            | _ -> Error "Invalid litteral or expression"
        | _ -> Error "No # preceeding expression/litteral"
    | true -> Error "No string passed to toLit function"

let testSymTab = [("testLab", 37u); ("otherLab", 94u)] |> Map.ofList

toLit "#4080" testSymTab
 