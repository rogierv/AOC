open AOC2021.Day14
open Utils.IO

let input = "AOC2021/Day14/Input.txt"

let result = input |> readAllLines |> Parser.parse |> Part1.solution "BNSOSBBKPCSCPKPOPNNK"

printf $"{result}"
