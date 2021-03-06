module AOC2016.Day02Tests

open FsUnit.Xunit
open Xunit

open AOC2016.Day02

[<Fact>]
let test1() = Part1.Solution ["ULL";"RRDDD";"LURDL";"UUUUD"] |> should equal "1985"

[<Fact>]
let test2() = Part2.Solution ["ULL";"RRDDD";"LURDL";"UUUUD"] |> should equal "5DB3"