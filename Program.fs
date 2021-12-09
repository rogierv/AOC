open AOC2021.Day09
open Utils.IO
open Parser

let input = "AOC2021/Day09/Input.txt"

let result = input |> readAllLines |> Part2.solution

printf $"{result}"