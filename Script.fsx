#time

#r "nuget: FSharpPlus"
open FSharpPlus

let o = ("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe").Split '|' |> Array.tail |> Array.head
let digits = o.Split ' ' |> Array.filter (fun x -> x <> "")
//digits |> Array.fold (fun acc x -> if x.Length = 2 || x.Length = 4 || x.Length = 3 || x.Length = 7 then acc + 1 else acc) 0

open System.Collections.Generic

//let dict = new Dictionary<char, char>()
//['a'..'g'] |> List.map (fun x -> dict.Add(x, '_'))

//let encode (word:string) (dict:Dictionary<char, char>) =
//    if word.Length = 2 then dict.['c'] <- word.[0]; dict.['f'] <- word.[1]
//    if word.Length = 3 then dict.['a'] <- word.[0]; dict.['c'] <- word.[1]; dict.['f'] <- word.[2]
//    if word.Length = 4 then dict.['b'] <- word.[0]; dict.['c'] <- word.[1]; dict.['d'] <- word.[2]; dict.['f'] <- word.[2]

//encode "ab" dict




let x = set ['a';'b']
"au" |> Set.ofSeq |> Set.union (set ['a';'b'])

let countDuplicates s1 s2 =
    s1 |> Set.fold (fun acc x -> if Set.contains x s2 then acc + 1 else acc) 0

//"aubc" |> countDuplicates "ab"

let encode (word:string) (d:Dictionary<int, char Set>) =
    let wordChars = word |> Set.ofSeq
    let value = match word.Length with
                | 2 -> d.[1] <- d.[1] |> Set.union wordChars; 1
                | 3 -> d.[7] <- d.[7] |> Set.union wordChars; 7
                | 4 -> d.[4] <- d.[4] |> Set.union wordChars; 4
                | 7 -> d.[8] <- d.[8] |> Set.union wordChars; 8
                | 5 -> if (d.[1] |> countDuplicates wordChars = 2) then 3 else
                       if (d.[4] |> countDuplicates wordChars = 3) then 5 else 6 
                | 6 -> if (d.[1] |> countDuplicates wordChars = 1) then 6 else
                       if (d.[4] |> countDuplicates wordChars = 4) then 9 else 0
    (d, value)

let dict = new Dictionary<int, char Set>()
[0; 1; 2; 3; 4; 5; 6; 7; 8; 9] |> List.map (fun x -> dict.Add(x, Set.empty))
let solve = new Dictionary<string, int>();

let words = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab cdfeb fcadb cdfeb cdbaf".Split " "




let encodeList (words:string[]) =
    let dict = new Dictionary<int, char Set>()
    let solve = new Dictionary<string, int>();
    [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] |> List.map (fun x -> dict.Add(x, Set.empty)) |> ignore
    [2; 3; 4; 7; 5; 6]
    |> List.map
        (fun step ->
            words
            |> Array.map
                (fun word ->
                    if word.Length = step && (fst (solve.TryGetValue word) = false) then
                        solve.Add(word, (snd (encode word dict))))) |> ignore
    solve

encodeList words
