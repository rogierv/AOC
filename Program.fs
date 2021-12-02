open AOC2021.Day02
open Utils.IO

let input = "AOC2021/Day02/Input.txt"

let result = input |> readAllLines |> Part2.solution

printf $"{result}"