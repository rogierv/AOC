module AOC2021.Day03Tests

open FsUnit.Xunit
open Xunit

open Day03

[<Fact>]
let test1() = Part1.solution ["00100";"11110";"10110";"10111";"10101";"01111";"00111";"11100";"10000";"11001";"00010";"01010"] |> should equal 198


[<Fact>]
let test2() = Part2.solution ["00100";"11110";"10110";"10111";"10101";"01111";"00111";"11100";"10000";"11001";"00010";"01010"] |> should equal 230
