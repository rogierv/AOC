module AOC2021.Day13Tests

open FsUnit.Xunit
open Xunit

open Day13

[<Fact>]
let test1() = Part1.solution [('y',7);('x',5)] [(6,10);(6,10);(0,14);(9,10);(0,3);(10,4);(4,11);(6,0);(6,12);(4,1);(0,13);(10,12);(3,4);(3,0);(8,4);(1,10);(2,14);(8,10);(9,0)] |> should equal 17