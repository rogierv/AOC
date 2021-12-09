#time

#r "nuget: FSharpPlus"
open FSharpPlus

open System.Collections.Generic

let inline charToInt c = int c - int '0'

let array2DFold folder (state:'State) (source:'T[,]) =
    source
    |> ( Seq.cast<'T> >> Seq.fold folder state ) 

let input = [|"2199943210";
            "3987894921";
            "9856789892";
            "8767896789";
            "9899965678"|]



let heightMap = input |> Array.map (fun x -> x |> Seq.toArray |> Array.map charToInt) |> array2D
heightMap[0,0]

let findAdjacent pos (map:int[,]) =
    let h = Array2D.length1 map - 1
    let v = Array2D.length2 map - 1
    let (x,y) = pos
    match (x,y) with
    | (0,0) -> [(x+1,y);(x,y+1)]
    | (x,y) when x = h && y = v -> [(x-1,y);(x,y-1)]
    | (0,y) when y = v ->  [(x+1,y);(x,y-1)]
    | (x,0) when x = h -> [(x-1,y);(x,y+1)]
    | (0,_) -> [(x+1,y);(x,y+1);(x,y-1)]
    | (_,0) -> [(x-1,y);(x+1,y);(x,y+1)] 
    | (x,_) when x = h -> [(x-1,y);(x,y+1);(x,y-1)]
    | (_,y) when y = v -> [(x+1,y);(x-1,y);(x,y-1)] 
    | _ -> [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]


let rec flood pos v (map:int[,]) =
    let getNeighbors = findAdjacent pos map
    if (map[fst pos, snd pos] < 9) then (Array2D.set map (fst pos) (snd pos) v)
    getNeighbors |> List.iter (fun p -> if map[fst p, snd p] < 9 then flood p v map)
    
flood (0,0) 11 heightMap
heightMap

let floodMap (map:int[,]) = 
    let mutable v = 10
    Array2D.iteri (fun ri ci x -> if x < 9 then flood (ri, ci) v map; v <- v + 1) map
    map

floodMap heightMap
    
let findSizeBasin (v:int) (map:int[,]) = array2DFold (fun acc x -> if x = v then acc + 1 else acc) 0 map


let findThreeLargestBasins (map:int[,]) =
    let maxValue = array2DFold (fun acc x -> if x > acc then x else acc) 10 map
    let l = [10..maxValue] |> List.map (fun x -> findSizeBasin x map)
    l |> List.sortDescending |> List.take 3

findThreeLargestBasins heightMap |> List.fold (*) 1

//heightMap

//let getLowestPositionsOfNeighbors (pos:(int*int) list) (map:int[,]) =
//    pos |> List.map (fun x -> map.[fst x, snd x]) |> List.min

//getLowestPositionsOfNeighbors [(3, 9); (1, 9); (2, 8)] heightMap

//heightMap[2,9]
//findAdjacent (2,9) heightMap



//let y = Array2D.mapi (fun ri ci x -> if (x < (getLowestPositionsOfNeighbors (findAdjacent (ri, ci) heightMap) heightMap)) then (x+1) else 0 ) heightMap |> array2dFold (fun acc x -> acc + x) 0



//Array2D.mapi (fun ri ci x -> (ri, ci)) heightMap
