module AOC2021.Day06

module Parser =
    let parse (input: string) = (input).Split ',' |> Array.map int |> Array.toList

module Part1 =
    let createArray l =
        let arr = [|for i in [0 .. 8] -> 0L|]
        l |> List.iter (fun fish -> arr.[fish] <- arr.[fish] + 1L)
        arr

    let nextDay(arr: int64 []) =
        let newFish = arr.[0]
        let newArray = Array.append (arr |> Array.tail) [|0|]
        if newFish > 0 then
            newArray.[6] <- newArray.[6] + newFish
            newArray.[8] <- newFish
        newArray

    let getTotalFish days input =
        [1 .. days]
        |> List.fold (fun x _ -> x |> nextDay) (input |> createArray)
        |> Array.sum

    let solution = getTotalFish 80

module Part2 =
    open Part1
    let solution = getTotalFish 256
