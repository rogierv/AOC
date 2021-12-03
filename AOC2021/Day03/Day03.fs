module AOC2021.Day03

open Utils.Convert

module Part1 =
    let gammaRate list =
        list
        |> List.map (fun x -> x |> Seq.toList |> List.map charToInt)
        |> List.transpose
        |> List.map
            (fun x ->
                if (x |> List.sum) > (x.Length / 2) then 1
                else 0)
        |> List.map string
        |> List.fold (+) ""

    let findOpposite bit =
        bit
        |> String.map (fun x -> if x = '0' then '1' else '0')

    let epsilonRate gammaRate = gammaRate |> findOpposite

    let solution(input: string list) =
        (gammaRate input |> binaryToInt) * (epsilonRate (gammaRate input) |> binaryToInt)

module Part2 =
    let mostCommonValue list =
        list
        |> List.countBy id
        |> List.sortBy (fun (a, _) -> -a)
        |> List.maxBy (fun (_, b) -> b)
        |> fst
        |> intToChar

    let leastCommonValue list =
        list
        |> List.countBy id
        |> List.sortBy (fun (a, _) -> a)
        |> List.minBy (fun (_, b) -> b)
        |> fst
        |> intToChar

    let filterOnBit pos (l: string list) bit = l |> List.filter (fun x -> x.[pos] = bit)

    let transpose(l: string list) =
        l
        |> List.map (fun x -> x |> Seq.toList |> List.map charToInt)
        |> List.transpose

    let filterBy func i list = (transpose list).[i] |> func |> filterOnBit i list

    let calculateRating func (input: string list) =
        [0 .. (input.[0].Length - 1)]
        |> List.fold (fun x y -> func y x) input
        |> List.head
        |> binaryToInt

    let solution(input: string list) =
        let oxygenGeneratorRating = calculateRating (filterBy mostCommonValue) input
        let CO2ScrubberRating = calculateRating (filterBy leastCommonValue) input
        oxygenGeneratorRating * CO2ScrubberRating
