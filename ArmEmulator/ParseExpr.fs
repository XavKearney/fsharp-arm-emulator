/// this module implements a parser for mathematical expressions
/// it supports multiply, add & subtract with 
/// literals in normal, binary (0b) or hex (0x/&) form
/// as well as labels, where the label is converted into its address
module ParseExpr
    open CommonLex
    open System.Text.RegularExpressions

    let (|Match1|_|) (pat:string) (inp:string) =
        let m = Regex.Matches(inp, pat) in
        if m.Count = 1
        then Some (m.[0].Value)
        else None

    let (|MatchGroups|_|) (pat:string) (inp:string) =
        let m = Regex.Matches(inp, pat) in
        if m.Count > 0
        then 
            [ for x in m -> x.Groups ]
            |> List.collect (fun x -> [for y in x -> y.Value])
            |> List.tail // remove the whole matched string
            |> Some 
        else None

    let (|Literal|_|) (symTab:SymbolTable) (inp:string) =
        match inp with
        | Match1 @"^(0x[a-fA-F0-9]+)$" x -> uint32 x |> Some
        | Match1 @"^(&[a-fA-F0-9]+)$" x -> "0x"+x.[1..] |> uint32 |> Some
        | Match1 @"^(0b[0-1]+)$" x -> uint32 x |> Some
        | Match1 @"^([0-9]+)$" x -> uint32 x |> Some
        | label -> symTab.TryFind label

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
        let rec tok' s = 
            match s with
            // if the charlist starts with an operator, match it
            | '+' :: rest -> Op Add :: tok' rest
            | '-' :: rest -> Op Sub :: tok' rest
            | '*' :: rest -> Op Mul :: tok' rest
            | '(' :: rest -> LBra :: tok' rest
            | ')' :: rest -> RBra :: tok' rest
            | ' ' :: rest -> tok' rest
            // if empty, we're done
            | [] -> []
            // otherwise, it must be a literal (number or label)
            | chrLst -> 
                // convert the remaining characters into a string
                chrLst |> List.toArray |> System.String |> 
                function
                // match if the only thing remaining is a literal
                | Match1 @"^([&a-zA-Z0-9]+)$" x -> 
                    match x with
                    | Literal symTab n -> [Num n]
                    | _ -> failwithf "Incorrect literal input."
                // otherwise, match the label/number up to the next operator
                | MatchGroups @"^([&a-zA-Z0-9]+)(.+)$" (txt :: [rest]) ->
                    match txt with
                    | Literal symTab n -> Num n :: tok' (Seq.toList rest)
                    | _ -> failwithf "Incorrect pre-finish literal input."
                | _ -> failwithf "Incorrect input."

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
                | true ->
                    let first, second, remaining = first2 nums
                    doOp ops.Head first second
                    |> fun res -> eval' rest (res::remaining) (Op op::ops.Tail)
                | false -> 
                    match nums, ops with
                    // if it starts with an operator, put in a 0 at the beginning
                    | [], [] -> eval' rest (0u::nums) (Op op::ops)
                    // otherwise just add the operator to the stack
                    | _ -> eval' rest nums (Op op::ops)
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
                | _ -> failwithf "Invalid tokenized input."
            // if nothing left
            | [] -> 
                match ops with
                // perform any remaining operations
                | Op op :: restOps ->
                    let first, second, remaining = first2 nums
                    doOp (Op op) first second
                    |> fun res -> eval' [] (res::remaining) restOps
                // otherwise return the result
                | [] -> nums.Head
                | _ -> failwithf "Should never happen."
        eval' tokInput [] []

    let evalExpr symTab expr = 
        tokenize expr symTab
        |> evalTokenized