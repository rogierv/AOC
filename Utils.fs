namespace Utils

open System.IO

module IO =
    let readLines filePath = File.ReadLines(filePath) |> Seq.head

    let readAllLines filePath : string list =
        File.ReadAllLines(filePath) |> Array.toList

    let readAllLinesToInt (filePath: string) : int list =
        filePath
        |> readAllLines
        |> List.map System.Int32.Parse
