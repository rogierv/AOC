module AOC2021.Day17Tests

open FsUnit.Xunit
open Xunit

open Day17

[<Fact>]
let test1() = nextStep {Position = (0,0); Velocity = (7,2)} 
            |> should equal {Position = (7,2); Velocity = (6,1)} 

[<Fact>]
let test2() = nextStep {Position = (7,2); Velocity = (6,1)} 
            |> should equal {Position = (13,3); Velocity = (5,0)}
            
[<Fact>]
let test3() = nextStep {Position = (13,3); Velocity = (5,0)} 
            |> should equal {Position = (18,3); Velocity = (4,-1)}

[<Fact>]
let test4() = getProbeStatus {Position = (28,-7); Velocity = (5,0)} {X = (20, 30); Y = (-10,-5)}
            |> should equal InTarget

[<Fact>]
let test5() = getProbeStatus {Position = (31,-7); Velocity = (5,0)} {X = (20, 30); Y = (-10,-5)}
            |> should equal OverShoot

[<Fact>]
let test6() = getProbeStatus {Position = (28,-11); Velocity = (5,0)} {X = (20, 30); Y = (-10,-5)}
            |> should equal OverShoot

[<Fact>]
let test7() = getProbeStatus {Position = (25,7); Velocity = (5,0)} {X = (20, 30); Y = (5,10)}
            |> should equal InTarget

[<Fact>]
let test8() = getProbeStatus {Position = (15,7); Velocity = (5,0)} {X = (20, 30); Y = (5,10)}
            |> should equal ToTarget

[<Fact>]
let test9() = getProbeStatus {Position = (25,15); Velocity = (5,0)} {X = (20, 30); Y = (5,10)}
            |> should equal ToTarget

[<Fact>]
let test13() = canHitTarget {Position = (0,0); Velocity = (7,2)} {X = (20, 30); Y = (-10,-5)} |> fst
            |> should equal true
            
[<Fact>]
let test14() = canHitTarget {Position = (0,0); Velocity = (6,3)} {X = (20, 30); Y = (-10,-5)} |> fst
            |> should equal true

[<Fact>]
let test15() = canHitTarget {Position = (0,0); Velocity = (9,0)} {X = (20, 30); Y = (-10,-5)} |> fst
            |> should equal true

[<Fact>]
let test16() = canHitTarget {Position = (0,0); Velocity = (17,-4)} {X = (20, 30); Y = (-10,-5)} |> fst
            |> should equal false

[<Fact>]
let test17() = maximumHeightTargetHit {Position = (0,0); Velocity = (6,9)} {X = (20, 30); Y = (-10,-5)}
            |> should equal (Some 45)

[<Fact>]
let test18() = maximumHeightTargetHit {Position = (0,0); Velocity = (6,10)} {X = (20, 30); Y = (-10,-5)}
            |> should equal (None)

[<Fact>]
let test19() = findMaximumHeight {Position = (0,0); Velocity = (0,0)} {X = (20, 30); Y = (-10,-5)}
            |> should equal 45

[<Fact>]
let test20() = findMaximumHeight {Position = (0,0); Velocity = (0,0)} {X = (240, 292); Y = (-90,-57)}
            |> should equal 4005

[<Fact>]
let test21() = canHitTarget {Position = (0,0); Velocity = (6,10)} {X = (20, 30); Y = (-10,-5)} |> fst
            |> should equal false
        
[<Fact>]
let test22() = canHitTarget {Position = (0,0); Velocity = (6,2)} {X = (20, 30); Y = (-10,-5)} |> fst
            |> should equal true

[<Fact>]
let test23() = getLimits {X = (20, 30); Y = (-10,-5)} |> should equal {X = (6, 30); Y = (-10, 89)}

[<Fact>]
let test24() = findAllCombinations {X = (6, 30); Y = (-10, 9)} |> List.length |> should equal 500

[<Fact>]
let test25() = initialVelocityValues {Position = (0,0); Velocity = (0,0)} {X = (20, 30); Y = (-10,-5)} |> List.length
            |> should equal 112

[<Fact>]
let test26() = canHitTarget {Position = (0,0); Velocity = (7,-1)} {X = (20, 30); Y = (-10,-5)} |> fst
            |> should equal true

[<Fact>]
let test27() = initialVelocityValues {Position = (0,0); Velocity = (0,0)} {X = (240, 292); Y = (-90,-57)} |> List.length
            |> should equal 2953