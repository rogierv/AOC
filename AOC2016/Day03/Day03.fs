module AOC2016.Day03

open System
open System.IO

module Parser =
    let parse input =
        input
        |> File.ReadAllLines
        |> Array.map
            (fun line ->
                ((line).Split " "
                 |> Array.filter (fun x -> x <> "")
                 |> Array.toList
                 |> List.map Int32.Parse))

module Part1 =
    let IsValidTriangle (s1: int) (s2: int) (s3: int) = (s1 + s2 > s3) && (s1 + s3 > s2) && (s2 + s3 > s1)

    let Solution(input: int list []) =
        input
        |> Array.filter (fun x -> IsValidTriangle x.[0] x.[1] x.[2])
        |> Array.length

module Part2 =
    open Part1

    let Solution(input: int list []) =
        input
        |> Array.splitInto (input.Length / 3)
        |> Array.map (fun x -> List.transpose x)
        |> List.concat
        |> List.filter (fun x -> IsValidTriangle x.[0] x.[1] x.[2])
        |> List.length
