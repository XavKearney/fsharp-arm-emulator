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

    type Operator = 
        | Add
        | Sub
        | Mul
        | Div

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
            | '/' :: rest -> Op Div :: tok' rest
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
                // match if the only thing remaining is a number
                | Match1 @"^([0-9]+)$" x -> [Num (uint32 x)]
                // otherwise, match the label/number up to the next operator
                | MatchGroups @"^([a-zA-Z0-9]+)(.+)$" (txt :: [rest]) ->
                    match txt with
                    | Match1 @"^(0x[0-9]+)$" x -> uint32 x
                    | Match1 @"^(&[0-9]+)$" x -> "0x"+x.[1..(x.Length-1)] |> uint32
                    | Match1 @"^(0b[0-1]+)$" x -> uint32 x
                    | Match1 @"^([0-9]+)$" x -> uint32 x
                    | label -> symTab.[label]
                    |> fun n -> Num n :: tok' (Seq.toList rest)
                | _ -> failwithf "Incorrect input."

        tok' (Seq.toList expr)

    let evalTokenized tokInput =
        let doOp op right left = 
            match op with
            | Op Add -> left + right
            | Op Sub -> left - right
            | Op Mul -> left * right
            | Op Div -> left / right
            | _ -> failwithf "Invalid operator."
        
        let first2 (lst: 'a list) = lst.Head, lst.Tail.Head, lst.Tail.Tail
        
        let rec eval' toks nums ops =
            match toks with
            | Num n :: rest -> eval' rest (n::nums) ops
            | Op op :: rest -> 
                match ops <> [] && ops.Head > (Op op) with
                | true ->
                    let first, second, remaining = first2 nums
                    doOp ops.Head first second
                    |> fun res -> eval' rest (res::remaining) (Op op::ops.Tail)
                | false -> eval' rest nums (Op op::ops)
            | LBra :: rest -> eval' rest nums (LBra::ops)
            | RBra :: rest ->
                match ops with
                | Op op :: restOps ->
                    let first, second, remaining = first2 nums
                    doOp (Op op) first second
                    |> fun res -> eval' toks (res::remaining) restOps
                | LBra :: _ ->
                    eval' rest nums ops.Tail
                | _ -> failwithf "Invalid tokenized input."
            | [] -> 
                match ops with
                | Op op :: restOps ->
                    let first, second, remaining = first2 nums
                    doOp (Op op) first second
                    |> fun res -> eval' [] (res::remaining) restOps
                | [] -> nums.Head
                | _ -> failwithf "Should never happen."
        eval' tokInput [] []

    let evalExpr symTab expr = 
        tokenize expr symTab
        |> evalTokenized