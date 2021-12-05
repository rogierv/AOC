module AOC2021.Day05Tests

open FsUnit.Xunit
open Xunit

open Day05

[<Fact>]
let test1() = Part1.solution [
                (0,9),(5,9)
                (8,0),(0,8)
                (9,4),(3,4)
                (2,2),(2,1)
                (7,0),(7,4)
                (6,4),(2,0)
                (0,9),(2,9)
                (3,4),(1,4)
                (0,0),(8,8)
                (5,5),(8,2)]
            |> should equal 5

[<Fact>]
let test2() = Part2.solution [
                (0,9),(5,9)
                (8,0),(0,8)
                (9,4),(3,4)
                (2,2),(2,1)
                (7,0),(7,4)
                (6,4),(2,0)
                (0,9),(2,9)
                (3,4),(1,4)
                (0,0),(8,8)
                (5,5),(8,2)]
            |> should equal 12