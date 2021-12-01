module AOC2021.Day02Tests

open FsUnit.Xunit
open Xunit

open Day02

[<Fact>]
let test1() = Part1.Solution [] |> should equal 0

[<Fact>]
let test2() = Part2.Solution [] |> should equal 0