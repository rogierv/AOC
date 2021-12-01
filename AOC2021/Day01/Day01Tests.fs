module AOC2021.Day01Tests

open FsUnit.Xunit
open Xunit

open Day01

[<Fact>]
let test1() = Part1.Solution [199;200;208;210;200;207;240;269;260;263] |> should equal 7

[<Fact>]
let test2() = Part2.Solution [199;200;208;210;200;207;240;269;260;263] |> should equal 5