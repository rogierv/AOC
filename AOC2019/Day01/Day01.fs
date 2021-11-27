module Day01

open System
open System.IO

module Parser =
    let parse input =
        input
        |> File.ReadAllLines
        |> Array.map Int32.Parse

module Part1 =
    let divideBy3 mass = mass / 3
    let substractBy2 mass = mass - 2
    let fuelRequired mass = mass |> divideBy3 |> substractBy2

    let sumOfFuelRequired input =
        input
        |> Array.fold (fun x y -> x + fuelRequired y) 0

    let Solution input = input |> sumOfFuelRequired

module Part2 =
    let totalFuelRequired mass =
        let mass = Part1.fuelRequired mass
        let rec total mass =
            match mass with
            | mass when mass <= 0 -> 0
            | _ -> mass + total (Part1.fuelRequired mass)
        total mass

    let sumOfTotalFuelRequired input =
        input
        |> Array.fold (fun x y -> x + totalFuelRequired y) 0

    let Solution input = input |> sumOfTotalFuelRequired
