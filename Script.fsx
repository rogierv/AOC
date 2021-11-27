type Direction =
    | R
    | L
    | U
    | D

let parseDirection d =
    match d with
    | 'R' -> R
    | 'L' -> L
    | 'U' -> U
    | 'D' -> D

let folder (x: (int * int) list) (y: string) =
    let direction = y.[0] |> parseDirection
    let d = y.[1..] |> int                  // distance
    let c = x |> List.last                  // current
    match direction with
    | R -> x @ ([0..d] |> List.map (fun y -> (fst c, snd c + y)))
    | L -> x @ ([0..d] |> List.map (fun y -> (fst c, snd c - y)))
    | U -> x @ ([0..d] |> List.map (fun y -> (fst c + y, snd c)))
    | D -> x @ ([0..d] |> List.map (fun y -> (fst c - y, snd c)))

let getPath wire = wire |> Array.toList |> List.fold folder [(0, 0)]

let getIntersections wire1 wire2 =
    let wire1Path = wire1 |> getPath
    let wire2Path = wire2 |> getPath
    Set.intersect (Set.ofList wire1Path) (Set.ofList wire2Path) |> Set.toList

let wire1 = [|"R8";"U5";"L5";"D3"|]
let wire2 = [|"U7";"R6";"D4";"L4"|]

wire1 |> getPath
wire2 |> getPath

getIntersections wire1 wire2 |> List.tail |> List.map (fun coor -> fst coor + snd coor) |> List.min
