module AOC2021.Day03

open Utils.Convert

module Part1 =
    let gammaRate list = 
        list 
        |> List.map (fun x -> x |> Seq.toList |> List.map charToInt)
        |> List.transpose
        |> List.map (fun x -> if (x |> List.sum) > (x.Length / 2) then 1 else 0)
        |> List.fold (fun x y -> x + (y |> string)) ""

    let findOpposite (bit:string) =
        bit |> String.map (fun x -> if x = '0' then '1' else '0')

    let toInt binary = System.Convert.ToInt32(binary, 2)
    let epsilonRate gammaRate = gammaRate |> findOpposite
    let calc input = (gammaRate input |> toInt) * (epsilonRate (gammaRate input) |> toInt)

    let solution (input:string list) = input |> calc

module Part2 =
    open Part1
    let transpose (l:string list) = l |> List.map (fun x -> x |> Seq.toList |> List.map charToInt) |> List.transpose    
    let findMostBit x = x |> List.countBy id |> List.sortBy (fun (a, _) -> -a) |> List.maxBy (fun (_,b) -> b) |> fst |> intToChar
    let findLeastBit (x:int list) = x |> List.countBy id |> List.sortBy (fun (a, _) -> a) |> List.minBy (fun (_,b) -> b) |> fst |> intToChar
    
    let filterOnBit pos bit (l:string list) = l |> List.filter (fun x -> x.[pos] = bit)
    
    let filter i l = filterOnBit i ((transpose l).[i] |> findMostBit) l
    let filterLeast i l = filterOnBit i ((transpose l).[i] |> findLeastBit) l

    let solution (input:string list) =
        let mutable first = input
        let mutable second = input
        for i in [0..(input[0].Length-1)] do first <- filter i first
        for i in [0..(input[0].Length-1)] do second <- filterLeast i second
        (first.[0] |> toInt) * (second.[0] |> toInt)