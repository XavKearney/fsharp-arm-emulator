/// this module implements a parser for mathematical expressions
/// it supports multiply, add & subtract with 
/// literals in normal, binary (0b) or hex (0x/&) form
/// as well as labels, where the label is converted into its address
module ParseExpr
    open CommonLex
    open System.Text.RegularExpressions

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
        then 
        [0..m.Count-1]
        |> List.map (fun i -> m.Item(i).Value)
        |> Some
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
            [0..m.Count-1]
            |> List.map (fun i -> m.[i].Groups)
            |> List.collect 
                (fun g -> 
                    [0..g.Count-1]
                    |> List.map (fun i -> g.[i].Value))
            |> List.tail // remove the whole matched string
            |> Some 
        else None

    // parses a string of hex characters 0-9A-F into a uint32
    let parseHex (s:string) =
        let charMap = 
            [('0',0u);('1',1u);('2',2u);('3',3u);('4',4u);('5',5u);('6',6u);('7',7u);('8',8u);
                ('9',9u);('A',10u);('B',11u);('C',12u);('D',13u);('E',14u);('F',15u)]
            |> Map.ofList
        let rec parseHex' charLst n i =
            match charLst with
            | [] -> n
            | c :: rest ->
                parseHex' rest (n + i * charMap.[c]) (i*16u)
        s
        |> Seq.map System.Char.ToUpper
        |> Seq.toList
        |> List.rev
        |> fun chars -> parseHex' chars 0u 1u

    // parses a string of binary characters 0-1 into a uint32
    let parseBin (s:string) =
        let charMap = 
            [('0',0u);('1',1u);]
            |> Map.ofList
        let rec parseBin' charLst n i =
            match charLst with
            | [] -> n
            | c :: rest ->
                parseBin' rest (n + i * charMap.[c]) (i*2u)
        s
        |> Seq.map System.Char.ToUpper
        |> Seq.toList
        |> List.rev
        |> fun chars -> parseBin' chars 0u 1u
    
    /// matches a literal in hex form (0x.. or &..),
    /// in binary form (0b..) or as a standard number
    /// if no match, try and find the string in the SymbolTable
    let (|Literal|_|) (symTab:SymbolTable) (inp:string) =
        match inp with
        | Match1 @"^(0[xX][a-fA-F0-9]+)$" x -> parseHex x.[2..] |> Some
        | Match1 @"^(&[a-fA-F0-9]+)$" x -> parseHex x.[1..] |> uint32 |> Some
        | Match1 @"^(0b[0-1]+)$" x -> parseBin x.[2..] |> Some
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
                match ops <> [] && ops.Head >= (Op op) with
                // operator should be applied now
                | true when nums.Length >= 2 ->
                    let first, second, remaining = first2 nums
                    doOp ops.Head first second
                    |> fun res -> eval' toks (res::remaining) (ops.Tail)
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