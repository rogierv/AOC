module AOC2021.Day09Tests

open FsUnit.Xunit
open Xunit

open Day09

[<Fact>]
let test1() = Part1.solution ["2199943210";"3987894921";"9856789892";"8767896789";"9899965678"]
            |> should equal 15

[<Fact>]
let test2() = Part2.solution ["2199943210";"3987894921";"9856789892";"8767896789";"9899965678"]
            |> should equal 1134
