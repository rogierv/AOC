module AOC2016.Day01Tests

open FsUnit.Xunit
open Xunit

open AOC2016.Day01

[<Fact>]
let test1() = Part1.Solution [|"R2"; "L3"|] |> should equal 5

[<Fact>]
let test2() = Part1.Solution [|"R2"; "R2"; "R2"|] |> should equal 2

[<Fact>]
let test3() = Part1.Solution [|"R5"; "L5"; "R5"; "R3"|] |> should equal 12

[<Fact>]
let test4() = Part2.Solution [|"R8"; "R4"; "R4"; "R8"|] |> should equal 4