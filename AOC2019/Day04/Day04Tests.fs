module AOC2019.Day04Tests

open FsUnit.Xunit
open Xunit

open Day04

[<Fact>]
let test1() =
    Part1.Solution [111111] |> should equal 1

[<Fact>]
let test2() =
    Part1.Solution [223450] |> should equal 0

[<Fact>]
let test3() =
    Part1.Solution [123789] |> should equal 0

[<Fact>]
let test4() =
    Part2.Solution [112233] |> should equal 1

[<Fact>]
let test5() =
    Part2.Solution [123444] |> should equal 0

[<Fact>]
let test6() =
    Part2.Solution [111122] |> should equal 1