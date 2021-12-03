module AOC2021.Day04Tests

open FsUnit.Xunit
open Xunit

open Day04

[<Fact>]
let test1() = Part1.solution [] |> should equal 0


[<Fact>]
let test2() = Part2.solution [] |> should equal 0