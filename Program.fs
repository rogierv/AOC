open AOC2021.Day01
open Utils.IO

let input = "AOC2021/Day01/Input.txt"

let result = input |> readAllLinesToInt |> Part2.Solution

printf $"{result}"