open AOC2016.Day04

let input = "AOC2016/Day04/Input.txt"

let result = input |> Parser.parse |> Part2.Solution

printf $"{result}"

