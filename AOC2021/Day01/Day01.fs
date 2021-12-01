module AOC2021.Day01

open System
open System.IO

module Parser =
    let parse input =
        input
        |> File.ReadAllLines
        |> Array.toList
        |> List.map Int32.Parse

module Part1 =
    let scanDepth measurements =
        measurements
        |> List.pairwise
        |> List.fold (fun x xs -> if (fst xs < snd xs) then x + 1 else x) 0

    let Solution input = input |> scanDepth

module Part2 =
    open Part1

    let Solution(input: int list) =
        input
        |> List.windowed 3
        |> List.map (fun x -> x |> List.sum)
        |> scanDepth
