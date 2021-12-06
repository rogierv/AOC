module AOC2021.Day06Tests

open FsUnit.Xunit
open Xunit

open Day06

[<Fact>]
let test1() = Part1.solution [3;4;3;1;2]
            |> should equal 5934L

[<Fact>]
let test2() = Part2.solution [3;4;3;1;2]
            |> should equal 26984457539L