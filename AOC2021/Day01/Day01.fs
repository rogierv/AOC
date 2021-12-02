module AOC2021.Day01

open Utils

module Part1 =
    let scanDepth measurements =
        measurements
        |> List.pairwise
        |> List.count (fun (a, b) -> b > a)

    let Solution input = input |> scanDepth

module Part2 =
    open Part1

    let Solution(input: int list) =
        input
        |> List.windowed 3
        |> List.map (fun x -> x |> List.sum)
        |> scanDepth
