open AOC2021.Day04
open Utils.IO
open Parser

let input = "AOC2021/Day04/Input.txt"
let inputLines = input |> readAllLines

let result = Part2.solution (inputLines |> parseDrawnNumbers) (inputLines |> parseCards)

printf $"{result}"