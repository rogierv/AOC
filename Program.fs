//open Day04.Parser
open AOC2016.Day02

let input = "AOC2016/Day02/Input.txt"

let result = input |> Parser.parse |> Part1.Solution

printf $"{result}"
