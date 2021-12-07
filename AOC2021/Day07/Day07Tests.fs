module AOC2021.Day07Tests

open FsUnit.Xunit
open Xunit

open Day07

[<Fact>]
let test1() = Part1.solution [|16;1;2;0;4;2;7;1;2;14|]
            |> should equal 37

[<Fact>]
let test2() = Part2.solution [|16;1;2;0;4;2;7;1;2;14|]
            |> should equal 168