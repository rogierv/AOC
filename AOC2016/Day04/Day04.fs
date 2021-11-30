module AOC2016.Day04

open System
open System.IO

module Parser =
    let parse input =
        input
        |> File.ReadAllLines
        |> Array.toList

module Part1 =
    type Encoded =
        {Encrypted: string
         Name: string
         SectorId: string
         Checksum: string}

    let commonLetters input =
        input
        |> Seq.toList
        |> Seq.countBy id
        |> Seq.sortBy (fun x -> -snd x, fst x)
        |> Seq.take 5
        |> Seq.fold (fun x y -> x + (fst y |> string)) ""

    let mapTo(input: string) =
        let a1 = (input).Split([|']'; '['|])
        let a2 = a1.[0]
        let split = a2.LastIndexOf '-'
        let checksum = a1.[1]
        let encryptedName = a2.Substring(0, (split)).Replace("-", "")
        let sectorId = a2.Substring(split + 1)
        {Encrypted = input
         Name = encryptedName
         SectorId = sectorId
         Checksum = checksum}

    let Solution(input: string list) =
        input
        |> List.map (fun x -> mapTo x)
        |> List.filter (fun x -> commonLetters x.Name = x.Checksum)
        |> List.sumBy (fun x -> (x.SectorId |> int))

module Part2 =
    open Part1

    let isAlpha c = System.Char.IsLetter c
    
    let shift n c  = 
      let b = 
        match isAlpha c with
        | true  -> (( int c - 97 + n ) % 26 ) + 97
        | false -> 
          match c with 
          | '-' -> 32
          | _   -> int c
      char b
              
    let encode xs n = 
      xs |> Seq.map (shift n) 
         |> Seq.toArray
         |> fun e -> new string (e)
    
    let mapTo(input: string) =
        let a1 = (input).Split([|']'; '['|])
        let a2 = a1.[0]
        let split = a2.LastIndexOf '-'
        let checksum = a1.[1]
        let encryptedName = a2.Substring(0, (split))
        let sectorId = a2.Substring(split + 1)
        {Encrypted = input
         Name = encryptedName
         SectorId = sectorId
         Checksum = checksum}

    let Solution(input: string list) =
        input 
        |> List.map (fun x -> mapTo x)
        |> List.filter (fun x -> encode x.Name (x.SectorId |> int) = "northpole object storage")

