module AOC2019.Day02Tests

open FsUnit.Xunit
open Xunit

open Day02

[<Fact>]
let test1() = Part1.Solution [|1;9;10;3;2;3;11;0;99;30;40;50|] |> should equal [|3500;9;10;70;2;3;11;0;99;30;40;50|]

[<Fact>]
let test2() = Part1.Solution [|1;0;0;0;99|] |> should equal [|2;0;0;0;99|]

[<Fact>]
let test3() = Part1.Solution [|2;3;0;3;99|] |> should equal [|2;3;0;6;99|]

[<Fact>]
let test4() = Part1.Solution [|2;4;4;5;99;0|] |> should equal [|2;4;4;5;99;9801|]

[<Fact>]
let test5() = Part1.Solution [|1;1;1;4;99;5;6;0;99|] |> should equal [|30;1;1;4;2;5;6;0;99|] 
