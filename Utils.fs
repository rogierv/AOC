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