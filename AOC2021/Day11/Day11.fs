module AOC2021.Day11
open Utils

module Part1 =
    type Octo = {
        mutable Value: int
        mutable IsFlashed: bool
        mutable FlashCount:int
    }

    let mapToOctos input = input |> array2D |> Array2D.mapi (fun x y v -> {Value = v; IsFlashed = false; FlashCount = 0})

    let getValues = Array2D.map (fun (x:Octo) -> x.Value)

    let findNeighbors pos (map:Octo[,]) =
        let h = Array2D.length1 map - 1
        let v = Array2D.length2 map - 1
        let (x,y) = pos
        let posx = [(if x > 0 then x-1 else x)..(if x < h then x+1 else x)]
        let posy = [(if y > 0 then y-1 else y)..(if y < v then y+1 else y)]
        List.allPairs posx posy 

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

    let solution input =  [1..100] |> List.fold (fun acc _ -> flashAllOctos acc) (input |> mapToOctos) |> Array2D.fold (fun acc x -> acc + x.FlashCount) 0

module Part2 =
    open Part1
    let sum (octos:Octo[,]) = (Array2D.fold (fun acc (o:Octo) -> acc + o.Value) 0 octos)
    
    let rec findSimul count (octos:Octo[,]) =
        let flashSimul = sum octos = 0
        match flashSimul with
        | true -> count
        | false -> findSimul (count+1) (flashAllOctos octos) 

    let solution input = input |> mapToOctos |> findSimul 0