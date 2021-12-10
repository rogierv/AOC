#time

#r "nuget: FSharpPlus"
open FSharpPlus

open System.Collections.Generic


let corrupt1 = "{([(<{}[<>[]}>{[]{[(<()>" |> Seq.toList
let corrupt2 = "[[<[([]))<([[{}[[()]]]" |> Seq.toList
let corrupt3 = "[{[{({}]{}}([{[{{{}}([]" |> Seq.toList
let corrupt4 = "{([(<{}[<>[]}>{[]{[(<()>" |> Seq.toList
let corrupt5 = "{([(<{}[<>[]}>{[]{[(<()>" |> Seq.toList
let missing = "[({(<(())[]>[[{[]{<()<>>" |> Seq.toList

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
    

findFirstIllegal corrupt3
findFirstIllegal missing

remainingIncomplete missing |> missingChunks
