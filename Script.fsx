#time

let inline charToInt c = int c - int '0'
let inline intToChar i = char i + char '0'

let example = ["00100";"11110";"10110";"10111";"10101";"01111";"00111";"11100";"10000";"11001";"00010";"01010"]

let transpose (l:string list) = l |> List.map (fun x -> x |> Seq.toList |> List.map charToInt) |> List.transpose

let findMostBit x = x |> List.countBy id |> List.sortBy (fun (a, _) -> a) |> List.maxBy (fun (_,b) -> b) |> fst |> intToChar
let findLeastBit (x:int list) = x |> List.countBy id |> List.sortBy (fun (a, _) -> a) |> List.minBy (fun (_,b) -> b) |> fst |> intToChar


[0;1] |> List.countBy id

let filterOnBit pos bit (l:string list) = l |> List.filter (fun x -> x.[pos] = bit)

let filter i l = filterOnBit i ((transpose l).[i] |> findMostBit) l
let filterLeast i l = filterOnBit i ((transpose l).[i] |> findLeastBit) l

0b00100 * 0b10100

let x = byte 10100  * byte 10100
10110
System.Convert.ToInt32(x)


let oxygenGeneratorRatingFold = [0..4] |> List.fold (fun x y -> filter y x) example