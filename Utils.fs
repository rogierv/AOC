namespace Utils

open System
open System.IO

module IO =
    let readLines path = File.ReadLines(path) |> Seq.head
    let readAllLines path = File.ReadAllLines(path) |> Array.toList
    let readAllLinesToInt path = path |> readAllLines |> List.map int

module List =
    let count (f: 'a -> bool) (l: 'a list) = l |> List.filter f |> List.length

module Seq =
    let count (f: 'a -> bool) (l: 'a seq) = l |> Seq.filter f |> Seq.length

    let filteri f =
        Seq.indexed
        >> Seq.filter (fun (i, e) -> f i e)
        >> Seq.map snd

module Convert =
    let inline charToInt c = int c - int '0'
    let inline intToChar i = char i + char '0'
    let binaryToInt b = Convert.ToInt32(b, 2)

module Array2D =
    let inline fold folder (state:'State) (source:'T[,]) =
        source
        |> (Seq.cast<'T> >> Seq.fold folder state ) 

module String =
    let isUpper (str : string) =
        let rec strIter isUpper arr =
            match arr with
            | [] -> isUpper
            | _ -> 
                match Char.IsLower(arr.Head) with
                | true -> strIter false []
                | false -> strIter true arr.Tail
        strIter true (Array.toList <| str.ToCharArray())