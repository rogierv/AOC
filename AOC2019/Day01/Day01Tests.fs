module Day01Tests

open FsUnit.Xunit
open Xunit

open Day01

[<Fact>]
let test1() = Part1.Solution [|12|] |> should equal 2

[<Fact>]
let test2() = Part1.Solution [|14|] |> should equal 2

[<Fact>]
let test3() = Part1.Solution [|1969|] |> should equal 654

[<Fact>]
let test4() = Part1.Solution [|100756|] |> should equal 33583

[<Fact>]
let test5() = Part2.Solution [|14|] |> should equal 2

[<Fact>]
let test6() = Part2.Solution [|1969|] |> should equal 966

[<Fact>]
let test7() = Part2.Solution [|100756|] |> should equal 50346
