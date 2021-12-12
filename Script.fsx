#time

#r "nuget: FSharpPlus"
open FSharpPlus

open System.Collections.Generic


type Octo = {
    mutable Value: int
    mutable IsFlashed: bool
    mutable FlashCount:int
}

let map = [|
    [|5;4;8;3;1;4;3;2;2;3|];
    [|2;7;4;5;8;5;4;7;1;1|];
    [|5;2;6;4;5;5;6;1;7;3|];
    [|6;1;4;1;3;3;6;1;4;6|];
    [|6;3;5;7;3;8;5;4;7;8|];
    [|4;1;6;7;5;2;4;6;4;5|];
    [|2;1;7;6;8;4;1;7;2;1|];
    [|6;8;8;2;8;8;1;1;3;4|];
    [|4;8;4;6;8;4;8;5;5;4|];
    [|5;2;8;3;7;5;1;5;2;6|]|]

let octos = map |> array2D |> Array2D.mapi (fun x y v -> {Value = v; IsFlashed = false; FlashCount = 0})

Array2D.map (fun (x:Octo) -> x.Value) octos

let getValues = Array2D.map (fun (x:Octo) -> x.Value)
let getIsFlashed = Array2D.map (fun (x:Octo) -> x.IsFlashed)

getValues octos

let findNeighbors pos (map:Octo[,]) =
    let h = Array2D.length1 map - 1
    let v = Array2D.length2 map - 1
    let (x,y) = pos
    match (x,y) with
    | (0,0) -> [(x+1,y);(x,y+1);(x+1,y+1)]
    | (x,y) when x = h && y = v -> [(x-1,y);(x,y-1);(x-1,y-1)]
    | (0,y) when y = v ->  [(x+1,y);(x,y-1);(x+1,y-1)]
    | (x,0) when x = h -> [(x-1,y);(x,y+1);(x-1,y+1)]
    | (0,_) -> [(x+1,y);(x,y+1);(x,y-1);(x+1,y-1);(x+1,y+1)]
    | (_,0) -> [(x-1,y);(x+1,y);(x,y+1);(x-1,y+1);(x+1,y+1)] 
    | (x,_) when x = h -> [(x-1,y);(x,y+1);(x,y-1);(x-1,y+1);(x-1,y-1)]
    | (_,y) when y = v -> [(x+1,y);(x-1,y);(x,y-1);(x+1,y-1);(x-1,y-1)]
    | _ -> [(x-1,y);(x+1,y);(x,y-1);(x,y+1);(x-1,y-1);(x+1,y-1);(x-1,y+1);(x+1,y+1)]

let array2dFold folder (state:'State) (source:'T[,]) =
    source
    |>   ( Seq.cast<'T> >> Seq.fold folder state ) 

let sum  (octos:Octo[,]) = (array2dFold (fun acc (o:Octo) -> acc + o.Value) 0 octos)

let rec flashOctos pos (octos:Octo[,]) =
    let (x,y) = pos
    if octos[x,y].Value < 9 && octos[x,y].IsFlashed = false 
    then octos[x,y].Value <- octos[x,y].Value + 1
    else if octos[x,y].IsFlashed = false 
        then octos[x,y].Value <- 0; 
             octos[x,y].IsFlashed <- true; 
             octos[x,y].FlashCount <- octos[x,y].FlashCount + 1
             findNeighbors (x,y) octos |> List.iter (fun p -> flashOctos p octos)

let flashAllOctos (octos:Octo[,]) =
    Array2D.iter (fun o -> o.IsFlashed <- false) octos
    Array2D.mapi (fun x y o -> flashOctos (x, y) octos) octos |> ignore
    octos

flashAllOctos octos

let result = [1..10] |> List.map (fun _ -> flashAllOctos octos)

let rec findSimul (octos:Octo[,]) count =
    printfn $"{sum octos}"
    let flashSimul = (sum octos) = 0
    match flashSimul with
    | true -> count
    | false -> findSimul (flashAllOctos octos) count+1

findSimul octos 0

getValues octos
sum octos

getIsFlashed octos

let xHigh = 5
let yHigh = 5
let (x,y) = (5,5)
let posx = [(if x > 0 then x-1 else x)..(if x < xHigh then x+1 else x)]
let posy = [(if y > 0 then y-1 else y)..(if y < yHigh then y+1 else y)]
List.allPairs posx posy 