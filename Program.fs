open AOC2021.Day08
open Utils.IO
open Parser

let input = "AOC2021/Day08/Input.txt"

let result = input |> readAllLines |> List.map (fun x -> x |> Part2.solution) |> List.sum

printf $"{result}"