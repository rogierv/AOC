open AOC2021.Day01

let input = "AOC2021/Day01/Input.txt"

let result = input |> Parser.parse |> Part2.Solution

printf $"{result}"

