module AOC2021.Day14
open Utils

module Parser =
    let parse input = 
        input 
        |> List.map (fun (instruction:string) -> (instruction.Split " -> " |> fun a -> ((a.[0].[0], a.[0].[1]), a.[1] |> char))) |> Map.ofList

module Part1 = 
    let AddToMap (c:char) (countChar:Map<char, int64>) = 
        let count = match Map.tryFind c countChar with
                    | Some x -> x + 1L
                    | None -> 1L
        countChar.Add (c, count)
    
    let newPairs (ch:char) (pair:(char*char)) = [(pair |> fst, ch);(ch, pair |> snd)]
    
    let step (rules:Map<(char*char), char>) (startPairs:(char * char) list) (countChar:Map<char, int64>) =
        List.foldBack (fun p acc -> 
            if (rules.ContainsKey p) 
                then ((newPairs rules.[p] p)@(fst acc), AddToMap rules.[p] (snd acc))
                else (p::(fst acc), snd acc)) 
            startPairs 
            ([], countChar)
    
    let steps count template rules = 
        let startChars = template |> Seq.toList |> List.countBy (fun c -> c) |> List.map (fun (c,x) -> (c, x |> int64) ) |> Map.ofList 
        let startPairs = template |> Seq.pairwise |> Seq.toList
        let calc = [0..(count-1)] |> List.fold (fun acc _ -> step rules (fst acc) (snd acc)) (startPairs, startChars)
        calc |> snd
    
    let calculate count template rules =
        let allElements = steps count template rules |> Map.toList |> List.sortBy (fun (_,v) -> -v)
        (snd allElements.Head) - (snd (allElements |> List.rev).Head)
    
    let solution template rules = calculate 40 template rules
    