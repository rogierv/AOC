﻿//open Day04.Parser
open AOC2016.Day03

let input = "AOC2016/Day03/Input.txt"

let result = input |> Parser.parse |> Part2.Solution

printf $"{result}"
