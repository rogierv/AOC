module AOC2021.Day02Tests

open FsUnit.Xunit
open Xunit

open Day02

[<Fact>]
let test1() = Part1.solution ["forward 5";"down 5";"forward 8";"up 3";"down 8";"forward 2"] |> should equal 150

[<Fact>]
let test2() = Part2.solution ["forward 5";"down 5";"forward 8";"up 3";"down 8";"forward 2"] |> should equal 900