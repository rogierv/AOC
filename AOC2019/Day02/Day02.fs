module Day02

open System
open System.IO

module Parser =
    let parse input =
        (input |> File.ReadLines |> Seq.head).Split ','
        |> Array.map Int32.Parse

module Part1 =
    let execute1202Program verb noun program : int [] =
        Array.set program 1 noun
        Array.set program 2 verb
        program

    let getParameters (program: int []) pos : (int * int * int) =
        let value1 = program.[program.[pos + 1]]
        let value2 = program.[program.[pos + 2]]
        let newPos = program.[pos + 3]
        (value1, value2, newPos)

    let executeProcess (program: int []) newPos newValue =
        Array.set program newPos newValue
        program

    let runOpsCode (program: int []) pos =
        match program.[pos] with
        | 1 ->
            (let (val1, val2, newPos) = getParameters program pos
             executeProcess program newPos (val1 + val2))
        | 2 ->
            (let (val1, val2, newPos) = getParameters program pos
             executeProcess program newPos (val1 * val2))
        | _ -> program

    let mapping(program: int []) =
        [0 .. 4 .. program.Length - 1]
        |> List.iter (fun x -> runOpsCode program x |> ignore)
        program

    let Solution input = input |> mapping

module Part2 =
    let verbs = [0 .. 99]
    let nouns = [0 .. 99]

    let findInput verb noun input =
        input
        |> Array.copy
        |> Part1.execute1202Program verb noun
        |> Part1.mapping
        |> Array.head

    let Solution input =
        for v in verbs do
            for n in nouns do
                if (findInput v n input = 19690720) then
                    printfn $"{v * 100 + n}"
                else
                    printf ""
