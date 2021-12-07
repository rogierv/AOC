open AOC2021.Day07
open Utils.IO
open Parser

let input = "AOC2021/Day07/Input.txt"

let result = input |> readLines |> parse |> Part1.solution

printf $"{result}"