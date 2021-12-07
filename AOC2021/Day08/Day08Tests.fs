module AOC2021.Day08Tests

open FsUnit.Xunit
open Xunit

open Day08

[<Fact>]
let test1() = Part1.solution
            |> should equal 0

[<Fact>]
let test2() = Part2.solution
            |> should equal 0