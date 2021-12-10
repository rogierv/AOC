module AOC2021.Day10Tests

open FsUnit.Xunit
open Xunit

open Day10

[<Fact>]
let test1() = Part1.solution "[({(<(())[]>[[{[]{<()<>>"
            |> should equal 0

[<Fact>]
let test2() = Part1.solution "[(()[<>])]({[<{<<[]>>("
            |> should equal 0

[<Fact>]
let test3() = Part1.solution "{([(<{}[<>[]}>{[]{[(<()>"
            |> should equal 1197

[<Fact>]
let test4() = Part1.solution "[[<[([]))<([[{}[[()]]]"
            |> should equal 3

[<Fact>]
let test5() = Part1.solution "[{[{({}]{}}([{[{{{}}([]"
            |> should equal 57

[<Fact>]
let test6() = Part1.solution "[<(<(<(<{}))><([]([]()"
            |> should equal 3

[<Fact>]
let test7() = Part1.solution "<{([([[(<>()){}]>(<<{{"
            |> should equal 25137

[<Fact>]
let test8() = Part2.solution "[({(<(())[]>[[{[]{<()<>>"
            |> should equal 288957