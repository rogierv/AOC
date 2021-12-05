open AOC2021.Day05
open Utils.IO
open Parser

let input = "AOC2021/Day05/Input.txt"

let result = input |> readAllLines |> parser |> Part2.solution

printf $"{result}"