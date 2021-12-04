module AOC2021.Day04

module Parser =
    let parseDrawnNumbers (input:string list) = (input |> List.head).Split ',' |> Array.toList
    let parseCards (input:string list) = 
        (input |> List.tail)
        |> List.map (fun x -> x.Split " " |> Array.filter (fun y -> y <> ""))
        |> List.filter (fun x -> x <> Array.empty) |> List.toArray
        |> Array.chunkBySize 5 
        |> Array.toList

module Part1 =
    let mark (card:string[,]) (value:string) = card |> Array2D.map (fun (v:string) -> if v = value then "x" else v)

    let isWinningCard (card:string[,]) =
        let check = Array.create 5 "x"
        [0..4] |> List.fold (fun x i ->  if card[i,*] = check || card[*,i] = check then true else x) false
    
    let sumOfRemainingValues (card:string[,]) =
        [0..4] |> List.fold (fun x i -> x + (card[i,*] |> Array.fold (fun x y -> if y <> "x" then x + (y |> int) else x ) 0)) 0

    let play card numbers =
        let letsPlay =
            numbers
            |> List.fold
                (fun a b ->
                    let (c, v, r, i) = a
                    if v <> "" then (c, v, r, i) else
                    if isWinningCard (mark c b) then
                        let newCard = mark c b
                        (newCard, b, sumOfRemainingValues newCard, i)
                    else
                        (mark c b, v, r, i + 1))
                (card, "", 0, 0)
        let (_, lastValue, sum, i) = letsPlay
        (lastValue, sum, i)

    let playCards numbers (cards:string[][] list) =
        cards 
        |> List.map (
            fun cardInput -> 
                let card = Array2D.init 5 5 (fun i j -> cardInput[i][j])
                numbers |> play card)

    let solution (numbers:string list) (cards:string[][] list) = 
        playCards numbers cards
        |> List.minBy (fun (_,_,c) -> c)
        |> fun (a,b,_) -> (a |> int) * b

module Part2 =
    open Part1
    let solution (numbers:string list) (cards:string[][] list) = 
        playCards numbers cards
        |> List.maxBy (fun (_,_,c) -> c)
        |> fun (a,b,_) -> (a |> int) * b