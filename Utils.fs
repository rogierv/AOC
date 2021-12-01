namespace Utils

open System
open System.IO

module IO =
    let readLines path = File.ReadLines(path) |> Seq.head

    let readAllLines path : string list = File.ReadAllLines(path) |> Array.toList

    let readAllLinesToInt path : int list = path |> readAllLines |> List.map Int32.Parse
