module AOC2021.Day05

open FParsec

module Parser =
    let ws = spaces
    let str s = ws >>. pstring s .>> ws
    let tupleParser: Parser<(int * int), unit> = pint32 .>> pchar ',' .>>. pint32
    let lineParser = tupleParser .>> str "->" .>>. tupleParser

    let parse str =
        match run lineParser str with
        | Success(result, _, _) -> result
        | Failure(_) -> failwith "Not Parsed"

    let parser input = 
        input 
        |> List.map (fun x -> parse x)

module Part1 =
    type Direction = Horizontal | Vertical | Diagonal45Degrees | Diagonal
    
    let getDirection (pos1:int*int) (pos2:int*int) =
        match (pos1, pos2) with
        | (x1, y1), (x2, y2) when x1 <> x2 && y1 = y2 -> Horizontal
        | (x1, y1), (x2, y2) when x1 = x2 && y1 <> y2 -> Vertical
        | (x1, y1), (x2, y2) when abs (x1 - x2) = abs (y1 - y2) -> Diagonal45Degrees
        | _ -> Diagonal
    
    let getPoints (pos1:int*int) (pos2:int*int) =
        let (x1, y1) = pos1
        let (x2, y2) = pos2
        let direction = getDirection pos1 pos2
        match direction with
        | Horizontal -> [(min x1 x2)..(max x1 x2)] |> List.map (fun x -> (x, y1))
        | Vertical -> [(min y1 y2)..(max y1 y2)] |> List.map (fun y -> (x1, y))
        | Diagonal45Degrees -> 
            let first = if x2 > x1 then [x1..x2] else [x1..(-1)..x2]
            let sec = if y2 > y1 then [y1..y2] else [y1..(-1)..y2]
            first |> List.map2 (fun a b -> (b, a)) sec
        | Diagonal -> [(min y1 y2)..(max y1 y2)] |> List.map (fun y -> (x1, y))
    
    let getAllPoints positions = 
        positions |> List.fold (fun x y -> x @ (getPoints (fst y) (snd y))) []
    
    let findOverlap positions =
        positions
        |> List.filter (fun (pos1, pos2) -> (getDirection pos1 pos2) <> Diagonal && (getDirection pos1 pos2) <> Diagonal45Degrees)
        |> getAllPoints
        |> List.groupBy id
        |> List.filter( fun (_,set) -> set.Length > 1)
        |> List.map( fun (key,_) -> key )

    let solution input = input |> findOverlap |> List.length

module Part2 =
    open Part1

    let findOverlap positions =
        positions
        |> List.filter (fun (pos1, pos2) -> (getDirection pos1 pos2) <> Diagonal)
        |> getAllPoints
        |> List.groupBy id
        |> List.filter( fun (_,set) -> set.Length > 1)
        |> List.map( fun (key,_) -> key )

    let solution input = input |> findOverlap |> List.length