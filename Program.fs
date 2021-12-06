open AOC2021.Day06
open Utils.IO
open Parser

let input = "AOC2021/Day06/Input.txt"

let result = input |> readLines |> parse |> Part2.solution

printf $"{result}"