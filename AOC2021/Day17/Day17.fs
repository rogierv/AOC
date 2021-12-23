module AOC2021.Day17

open System

type Probe = {Position: (int * int); Velocity: (int * int)}
type Target = {X: (int * int); Y: (int * int)}
type ProbeStatus = InTarget | OverShoot | ToTarget
type Limits = {X: (int * int); Y: (int * int)}

let nextStep (probe:Probe) : Probe =
    let (px, py) = probe.Position
    let (vx, vy) = probe.Velocity
    { Position = (px + vx, py + vy); 
      Velocity = ((if vx > 0 then vx-1 else vx), vy-1) }

let getProbeStatus (probe:Probe) (target:Target) : ProbeStatus =
    let (px, py) = probe.Position
    let (tx, tx') = target.X
    let (ty, ty') = target.Y
    if px >= tx && px <= tx' && 
       py >= ty && py <= ty' then InTarget else
    if px >= tx' || py <= ty then OverShoot
    else ToTarget

let canHitTarget (probe:Probe) (target:Target) : (Boolean * int) =
    let (_, py) = probe.Position
    let mutable maximumHeight = py
    let rec steps probe = 
        match getProbeStatus probe target with
        | InTarget -> (true, maximumHeight)
        | OverShoot -> (false, maximumHeight)
        | ToTarget -> 
            let step = nextStep probe
            let (_, ny) = step.Position
            if ny > maximumHeight then maximumHeight <- ny
            steps (nextStep probe)
    steps probe

let maximumHeightTargetHit (probe:Probe) (target:Target) : int option =
    match canHitTarget probe target with
    | (false, _) -> None
    | (true, maxHeigh) -> Some maxHeigh

let findMaximumHeight (probe:Probe) (target:Target) : int =
    let (tx, _) = target.X
    let rec findMinimumX x = 
        match x with
        | x when [1..x] |> List.sum >= tx -> x
        | _ -> findMinimumX (x+1)
    let minimumX = findMinimumX 0
    let rec findMaximum y =
        match (canHitTarget {probe with Velocity = (minimumX, y)} target, y) with
        | ((true,_), y) -> y
        | ((false,_), y) -> findMaximum (y-1)
    let maximumY = findMaximum 100
    maximumHeightTargetHit {Position = (0,0); Velocity = (minimumX, maximumY)} target 
    |> Option.defaultValue 0

let getLimits (target:Target) : Limits =
    let (tx, tx') = target.X
    let (ty, _) = target.Y
    let rec findMinimumX x = 
        match x with
        | x when [1..x] |> List.sum >= tx -> x
        | _ -> findMinimumX (x+1)
    let minimumX = findMinimumX 0
    let maximumX = tx'
    let minimumY = ty
    let maximumY = 89
    {X = (minimumX, maximumX); Y = (minimumY, maximumY)}

let findAllCombinations limits =
    let (x,x') = limits.X
    let (y,y') = limits.Y
    [for x in [x..x'] do for y in [y..y'] -> (x,y)]

let initialVelocityValues (probe:Probe) (target:Target) =
    let limits = getLimits target
    let allcombinations = findAllCombinations limits
    allcombinations |> List.filter (fun (x,y) -> canHitTarget {probe with Velocity = (x,y)} target |> fst)