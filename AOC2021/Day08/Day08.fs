module AOC2021.Day08

module Parser =
    let parseToDigits(input: string) = (input.Split '|' |> Array.tail |> Array.head).Split ' ' |> Array.filter (fun x -> x <> "")

    let parseAllValues(input: string) = input.Split ' ' |> Array.filter (fun x -> x <> "|")

module Part1 =
    open Parser
    let countDigits (digits:string[]) = digits |> Array.fold (fun acc x -> if x.Length = 2 || x.Length = 4 || x.Length = 3 || x.Length = 7 then acc + 1 else acc) 0
    let solution input = input |> parseToDigits |> countDigits

module Part2 =
    open Parser
    open System.Collections.Generic

    let countDuplicates s1 s2 =
        s1 |> Set.fold (fun acc x -> if Set.contains x s2 then acc + 1 else acc) 0

    let encode (word:string) (d:Dictionary<int, char Set>) =
        let wordChars = word |> Set.ofSeq
        let value = match word.Length with
                    | 2 -> d.[1] <- d.[1] |> Set.union wordChars; 1
                    | 3 -> d.[7] <- d.[7] |> Set.union wordChars; 7
                    | 4 -> d.[4] <- d.[4] |> Set.union wordChars; 4
                    | 7 -> d.[8] <- d.[8] |> Set.union wordChars; 8
                    | 5 -> if (d.[1] |> countDuplicates wordChars = 2) then 3 else
                           if (d.[4] |> countDuplicates wordChars = 3) then 5 else 2 
                    | 6 -> if (d.[1] |> countDuplicates wordChars = 1) then 6 else
                           if (d.[4] |> countDuplicates wordChars = 4) then 9 else 0
        (d, value)

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

    let getAllValues input = input |> parseAllValues |> encodeList

    let solution input = input |> parseToDigits |> Array.map (fun x -> (getAllValues input).[x]) |> Array.map string |> Array.fold (+) "" |> int