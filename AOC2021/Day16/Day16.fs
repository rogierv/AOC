module AOC2021.Day16

open System

type PacketType = Literal | Operator

type LengthType =
    | TotalLengthInBits of int
    | NumberOfSubPackets of int

let hexToBinary hex =
    let binary = Convert.ToString(Convert.ToInt32(hex, 16), 2)
    binary.PadLeft(4, '0')

let hexToBinaryString(hexString: String) =
    hexString
    |> Seq.fold (fun acc c -> acc + (c |> string |> hexToBinary)) String.Empty

let binaryToInt bin = Convert.ToInt32(bin, 2);

let packetVersion (bin:String) = bin.[0..2] |> binaryToInt

let packetType (bin:String) = 
    match bin.[3..5] |> binaryToInt with
        | 4 -> Literal
        | _ -> Operator

let operatorLengthType (bin:String) = 
    match bin.[6] with
        | '0' -> TotalLengthInBits (bin.[7..21] |> binaryToInt)
        | _   -> NumberOfSubPackets (bin.[7..17] |> binaryToInt)

let getSubPackets bin =
    match operatorLengthType bin with
        | TotalLengthInBits x -> [bin.[22..32];bin.[33..(22+(x-1))]] |> List.filter (fun x -> x.Length >= 11)
        | NumberOfSubPackets n -> 
            let binaryLength = bin.TrimEnd('0').Length / n
            [0..(n-1)] |> List.map (fun i -> bin.[18+(binaryLength*i)..18+(binaryLength*(i+1))]) |> List.filter (fun x -> x.Length >= 11)
        //| NumberOfSubPackets x -> [0..(x-1)] |> List.map (fun i -> bin.[18+(11*i)..28+(11*i)])

let solution (bin:String) =
    let mutable versionSum = 0
    let addToSum bin = printfn $"{packetVersion bin}" ; versionSum <- versionSum + packetVersion bin
    let rec loop bin = 
        addToSum bin
        match packetType bin with
            | Literal -> ()
            | Operator -> getSubPackets bin |> List.iter (fun sub -> (); loop sub)
    loop bin
    versionSum