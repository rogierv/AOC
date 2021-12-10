module AOC2021.Day10

module Part1 =
    let isOpenChunk = function
    | '{' | '(' | '[' | '<' -> true
    | _ -> false

    let isClosingChunk = function
    | '}' | ')' | ']' | '>' -> true
    | _ -> false

    let getClosingChunk = function
    | ')' -> '(' 
    | ']' -> '[' 
    | '}' -> '{' 
    | '>' -> '<' 

    let getOpeningChunk = function
    | '(' -> ')'  
    | '[' -> ']'
    | '{' -> '}' 
    | '<' -> '>'

    let findFirstIllegal line =
        let rec isCorrupt l stack =
            match l, stack with
            | [], _ -> None
            | h::t, _ when isOpenChunk h -> isCorrupt t (h::stack)
            | h::t, h1::t1 when isClosingChunk h -> if h1 <> getClosingChunk h then Some(h) else isCorrupt t t1
        isCorrupt line []

    let getScore = function
        | Some(')') -> 3
        | Some(']') -> 57
        | Some('}') -> 1197
        | Some('>') -> 25137
        | None -> 0

    let solution input = input |> Seq.toList |> findFirstIllegal |> getScore

module Part2 =
    open Part1

    let remainingIncomplete line =
        let rec isCorrupt l stack =
            match l, stack with
            | [], _ -> Some(stack)
            | h::t, _ when isOpenChunk h -> isCorrupt t (h::stack)
            | h::t, h1::t1 when isClosingChunk h -> if h1 <> getClosingChunk h then None else isCorrupt t t1
        isCorrupt line []
    
    let missingChunks remaining =
        match remaining with
        | Some r -> r |> List.map (fun x -> getOpeningChunk x)
        | None -> []

    let getScore missing: int64 =
        let folder (acc:int64) x =
            let acc = acc * 5L
            match x with
            | ')' -> acc + 1L
            | ']' -> acc + 2L
            | '}' -> acc + 3L
            | '>' -> acc + 4L
        missing |> List.fold folder 0
 
    let solution input = input |> Seq.toList |> remainingIncomplete |> missingChunks |> getScore