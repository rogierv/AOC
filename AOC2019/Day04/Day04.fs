module Day04

module Part1 =
    let inline charToInt c = int c - int '0'

    let rec digitsNeverDecrease(i: int) : bool =
        let s = i |> string |> Seq.toList
        match s with
        | [_] -> true
        | h :: t ->
            if (h |> charToInt) > (t.[0] |> charToInt) then
                false
            else
                digitsNeverDecrease (new System.String(t |> List.toArray) |> int)

    let rec hasTwoAdjacentDigits(i: int) : bool =
        let s = i |> string |> Seq.toList
        match s with
        | [_] -> false
        | h :: t ->
            if (h |> charToInt) = (t.[0] |> charToInt) then
                true
            else
                hasTwoAdjacentDigits (new System.String(t |> List.toArray) |> int)

    let Solution input =
        input
        |> List.filter (fun x -> digitsNeverDecrease x && hasTwoAdjacentDigits x)
        |> List.length

module Part2 =
    let group xs =
        let folder x =
            function
            | [] -> [[x]]
            | (h :: t) :: ta when h = x -> (x :: h :: t) :: ta
            | acc -> [x] :: acc
        Seq.foldBack folder xs []

    let findAdjacentDigits(i: int) =
        let c =
            i
            |> string
            |> group
            |> List.filter (fun x -> x.Length <> 1)
            |> List.sortBy (fun x -> x.[0])
            |> List.last
        c.Length = 2

    let Solution input =
        input
        |> List.filter
            (fun x ->
                Part1.digitsNeverDecrease x
                && Part1.hasTwoAdjacentDigits x
                && findAdjacentDigits x)
        |> List.length
