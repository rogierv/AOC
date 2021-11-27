module Day03Tests

open FsUnit.Xunit
open Xunit

open Day03

[<Fact>]
let test1() =
    Part1.Solution [|"R8";"U5";"L5";"D3"|] [|"U7";"R6";"D4";"L4"|]
    |> should equal 6

[<Fact>]
let test2() =
    Part1.Solution 
        [|"R75";"D30";"R83";"U83";"L12";"D49";"R71";"U7";"L72"|] 
        [|"U62";"R66";"U55";"R34";"D71";"R55";"D58";"R83"|]
    |> should equal 159

[<Fact>]
let test3() =
    Part1.Solution 
        [|"R98";"U47";"R26";"D63";"R33";"U87";"L62";"D20";"R33";"U53";"R51"|] 
        [|"U98";"R91";"D20";"R16";"D67";"R40";"U7";"R15";"U6";"R7"|]
    |> should equal 135

[<Fact>]
let test4() =
    Part2.Solution [|"R8";"U5";"L5";"D3"|] [|"U7";"R6";"D4";"L4"|]
    |> should equal 30

[<Fact>]
let test5() =
    Part2.Solution 
        [|"R75";"D30";"R83";"U83";"L12";"D49";"R71";"U7";"L72"|] 
        [|"U62";"R66";"U55";"R34";"D71";"R55";"D58";"R83"|]
    |> should equal 610

[<Fact>]
let test6() =
    Part2.Solution 
        [|"R98";"U47";"R26";"D63";"R33";"U87";"L62";"D20";"R33";"U53";"R51"|] 
        [|"U98";"R91";"D20";"R16";"D67";"R40";"U7";"R15";"U6";"R7"|]
    |> should equal 410