module AOC2021.Day07

module Parser =
    let parse(input: string) = (input).Split ',' |> Array.map int

module Part1 =
    let constantRate list value =
        list
        |> Array.fold (fun x y -> abs (y - value) + x) 0

    type num = int // to get the max value

    let calculateCheapestOutcome f input =
        let rec calc prevCost y =
            let currentCost = f input y
            match prevCost - currentCost with
            | d when d > 0 -> calc currentCost (y + 1)
            | _ -> prevCost
        input |> Array.min |> calc num.MaxValue

    let solution = calculateCheapestOutcome constantRate

module Part2 =
    open Part1

    let progressiveRate list value =
        list
        |> Array.fold (fun x y -> ([1 .. (abs (y - value))] |> List.sum) + x) 0

    let solution = calculateCheapestOutcome progressiveRate
