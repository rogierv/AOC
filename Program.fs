open AOC2021.Day03
open Utils.IO

let input = "AOC2021/Day03/Input.txt"

let result = input |> readAllLines |> Part2.solution

printf $"{result}"