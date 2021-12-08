module AOC2021.Day08Tests

open FsUnit.Xunit
open Xunit

open Day08

[<Fact>]
let test1() = Part1.solution "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
            |> should equal 2

[<Fact>]
let test2() = Part1.solution "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
            |> should equal 3

[<Fact>]
let test3() = Part2.solution "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
            |> should equal 5353

[<Fact>]
let test4() = Part2.solution "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
            |> should equal 8394


[<Fact>]
let test5() = Part2.solution "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
            |> should equal 9781

[<Fact>]
let test6() = Part2.solution "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
            |> should equal 1197

[<Fact>]
let test7() = Part2.solution "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
            |> should equal 9361

[<Fact>]
let test8() = Part2.solution "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
            |> should equal 4873


[<Fact>]
let test9() = Part2.solution "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
            |> should equal 8418


[<Fact>]
let test10() = Part2.solution "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
            |> should equal 4548


[<Fact>]
let test11() = Part2.solution "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
            |> should equal 1625


[<Fact>]
let test12() = Part2.solution "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
            |> should equal 8717


[<Fact>]
let test13() = Part2.solution "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
            |> should equal 4315


