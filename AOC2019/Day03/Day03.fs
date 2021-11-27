module Day03

open System

module Parser =
    let parse input = input |> IO.File.ReadAllLines |> Array.map (fun x -> x.Split ',')

module Part1 =
    type Wire = { 
        Coordinates: (int * int) list
        Steps: int }

    let folder (x: (int * int) list) (y: string) =
        let direction = y.[0]
        let d = y.[1..] |> int                  // distance
        let c = x |> List.last                  // current
        match direction with
        | 'R' -> x @ ([1..d] |> List.map (fun y -> (fst c, snd c + y)))
        | 'L' -> x @ ([1..d] |> List.map (fun y -> (fst c, snd c - y)))
        | 'U' -> x @ ([1..d] |> List.map (fun y -> (fst c + y, snd c)))
        | 'D' -> x @ ([1..d] |> List.map (fun y -> (fst c - y, snd c)))
        | _   -> x
    
    let getPath wire = wire |> Array.toList |> List.fold folder [(0, 0)]
    
    let getIntersections wire1 wire2 =
        let wire1Path = wire1 |> getPath
        let wire2Path = wire2 |> getPath
        Set.intersect (Set.ofList wire1Path) (Set.ofList wire2Path) |> Set.toList
        
    let findClosestIntersection wire1 wire2 =
        let discardHome coordinates = coordinates |> List.filter (fun coor -> (fst coor, snd coor) <> (0,0))
        let manhattenDistance (coordinates:(int*int)) = Math.Abs(fst coordinates) + Math.Abs(snd coordinates)
        
        getIntersections wire1 wire2 
        |> discardHome
        |> List.map (fun coor -> manhattenDistance coor) 
        |> List.min

    let Solution = findClosestIntersection

module Part2 =
    let findIndex wirePath position = wirePath |> List.findIndex (fun y -> y = position)

    let Solution wire1 wire2 =
        let wire1Path = wire1 |> Part1.getPath
        let wire2Path = wire2 |> Part1.getPath
        Set.intersect (Set.ofList wire1Path) (Set.ofList wire2Path) 
            |> Set.toList 
            |> List.map (fun pos -> findIndex wire1Path pos + findIndex wire2Path pos)
            |> List.filter (fun x -> x <> 0)
            |> List.min
    