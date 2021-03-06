module AOC2016.Day02

open System
open System.IO

module Parser =
    let parse input = input |> IO.File.ReadAllLines |> Array.toList

module Part1 =
    type Instruction = L | R | U | D
    
    let ToInstruction c : Instruction = 
        match c with
        | 'L' -> L
        | 'R' -> R
        | 'U' -> U
        | 'D' -> D
    
    let GetNextValue (current:int) (instruction:Instruction): int =
        match (instruction, current) with
        | (L,1) | (L,2) | (U,1) | (U,4) -> 1
        | (L,3) | (R,1) | (U,5) | (U,2) -> 2
        | (R,3) | (R,2) | (U,3) | (U,6) -> 3
        | (L,5) | (L,4) | (U,7) | (D,1) -> 4
        | (L,6) | (R,4) | (U,8) | (D,2) -> 5
        | (R,5) | (R,6) | (U,9) | (D,3) -> 6
        | (L,8) | (L,7) | (D,4) | (D,7) -> 7
        | (L,9) | (R,7) | (D,8) | (D,5) -> 8
        | _                             -> 9

    let FindButton (start:int) (s: string) =
        s
        |> Seq.map (fun c -> ToInstruction c)
        |> Seq.toList
        |> List.fold (fun x y -> GetNextValue x y) start

    let Solution input =
        input
        |> List.fold (fun x y -> x @ [FindButton(List.last x) y]) [5]
        |> List.tail
        |> List.map string
        |> String.concat ""

module Part2 =
    open Part1

    let GetNextValue (current:char) (instruction:Instruction): char =
        match (instruction, current) with
        | (L,'1') | (U,'1') | (U,'3') | (R,'1') -> '1'
        | (L,'2') | (L,'3') | (U,'6') | (U,'2') -> '2'
        | (L,'4') | (D,'1') | (U,'7') | (R,'2') -> '3'
        | (R,'4') | (R,'3') | (U,'4') | (U,'8') -> '4'
        | (L,'5') | (L,'6') | (D,'5') | (U,'5') -> '5'
        | (L,'7') | (R,'5') | (D,'2') | (U,'A') -> '6'
        | (L,'8') | (R,'6') | (D,'3') | (U,'B') -> '7'
        | (L,'9') | (R,'7') | (D,'4') | (U,'C') -> '8'
        | (R,'9') | (R,'8') | (D,'9') | (U,'9') -> '9'
        | (D,'6') | (L,'B') | (D,'A') | (L,'A') -> 'A'
        | (D,'7') | (L,'C') | (R,'A') | (U,'D') -> 'B'
        | (D,'8') | (R,'B') | (R,'C') | (D,'C') -> 'C'
        | _                                     -> 'D'

    let FindButton (start:char) (s: string) =
        s
        |> Seq.map (fun c -> ToInstruction c)
        |> Seq.toList
        |> List.fold (fun x y -> GetNextValue x y) start

    let Solution input =
        input
        |> List.fold (fun x y -> x @ [FindButton(List.last x) y]) ['5']
        |> List.tail
        |> List.map string
        |> String.concat ""