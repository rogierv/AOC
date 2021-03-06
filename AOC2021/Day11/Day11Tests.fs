module AOC2021.Day11Tests

open FsUnit.Xunit
open Xunit

open Day11

[<Fact>]
let test1() = Part1.solution [|[|1;1;1;1;1|];[|1;9;9;9;1|];[|1;9;1;9;1|];[|1;9;9;9;1|];[|1;1;1;1;1|]|]|> should equal 259

[<Fact>]
let test2() = Part1.solution [|
                                [|5;4;8;3;1;4;3;2;2;3|];
                                [|2;7;4;5;8;5;4;7;1;1|];
                                [|5;2;6;4;5;5;6;1;7;3|];
                                [|6;1;4;1;3;3;6;1;4;6|];
                                [|6;3;5;7;3;8;5;4;7;8|];
                                [|4;1;6;7;5;2;4;6;4;5|];
                                [|2;1;7;6;8;4;1;7;2;1|];
                                [|6;8;8;2;8;8;1;1;3;4|];
                                [|4;8;4;6;8;4;8;5;5;4|];
                                [|5;2;8;3;7;5;1;5;2;6|]|] |> should equal 1656

[<Fact>]
let test3() = Part1.solution [|
                            [|2;4;7;8;6;6;8;3;2;4|];
                            [|4;2;8;3;4;7;4;1;2;5|];
                            [|1;6;6;3;4;6;3;3;7;4|];
                            [|1;7;3;8;2;7;1;3;2;3|];
                            [|4;2;8;5;7;4;4;8;6;1|];
                            [|3;5;5;1;3;1;1;5;1;5|];
                            [|8;5;7;4;3;3;5;4;3;8|];
                            [|7;8;4;3;5;2;5;8;2;6|];
                            [|1;3;6;6;2;3;7;5;7;7|];
                            [|3;5;5;4;6;8;7;2;2;6|]|]|> should equal 1700

[<Fact>]
let test4() = Part2.solution [|
                                [|5;4;8;3;1;4;3;2;2;3|];
                                [|2;7;4;5;8;5;4;7;1;1|];
                                [|5;2;6;4;5;5;6;1;7;3|];
                                [|6;1;4;1;3;3;6;1;4;6|];
                                [|6;3;5;7;3;8;5;4;7;8|];
                                [|4;1;6;7;5;2;4;6;4;5|];
                                [|2;1;7;6;8;4;1;7;2;1|];
                                [|6;8;8;2;8;8;1;1;3;4|];
                                [|4;8;4;6;8;4;8;5;5;4|];
                                [|5;2;8;3;7;5;1;5;2;6|]|] |> should equal 195

[<Fact>]
let test5() = Part2.solution [|
                            [|2;4;7;8;6;6;8;3;2;4|];
                            [|4;2;8;3;4;7;4;1;2;5|];
                            [|1;6;6;3;4;6;3;3;7;4|];
                            [|1;7;3;8;2;7;1;3;2;3|];
                            [|4;2;8;5;7;4;4;8;6;1|];
                            [|3;5;5;1;3;1;1;5;1;5|];
                            [|8;5;7;4;3;3;5;4;3;8|];
                            [|7;8;4;3;5;2;5;8;2;6|];
                            [|1;3;6;6;2;3;7;5;7;7|];
                            [|3;5;5;4;6;8;7;2;2;6|]|]|> should equal 273
