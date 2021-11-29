//open Day04.Parser
open AOC2016.Day01

let input = "AOC2016/Day01/Input.txt"

let result = input |> Parser.parse |> Part2.Solution

printf $"{result}"
