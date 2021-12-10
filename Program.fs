open AOC2021.Day10
open Utils.IO

let input = "AOC2021/Day10/Input.txt"

let result = 
    input 
    |> readAllLines 
    |> List.map (fun x -> Part2.solution x)
    |> List.filter (fun x -> x <> 0)
    |> List.sort

let middle = result.Length / 2

printf $"{result[middle]}"