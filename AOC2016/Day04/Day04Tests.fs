module AOC2016.Day04Tests

open FsUnit.Xunit
open Xunit

open AOC2016.Day04

[<Fact>]
let test1() = Part1.Solution ["aaaaa-bbb-z-y-x-123[abxyz]"] |> should equal 123

[<Fact>]
let test2() = Part1.Solution ["a-b-c-d-e-f-g-h-987[abcde]"] |> should equal 987

[<Fact>]
let test3() = Part1.Solution ["not-a-real-room-404[oarel]"] |> should equal 404

[<Fact>]
let test4() = Part1.Solution ["totally-real-room-200[decoy]"] |> should equal 0
