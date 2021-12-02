module AOC2021.Day02

module Parser =
    type Direction = Up | Down | Forward
    type Instruction = { Direction: Direction; Value: int }

    let parse(instruction: string) =
        let split = instruction.Split ' '
        let dir = 
            match split.[0] with
            | "down" -> Down
            | "up" -> Up
            | _ -> Forward
        { Direction = dir; Value = split.[1] |> int}

module Part1 =
    open Parser

    let nextPosition (pos: int * int) (instruction: string) =
        let i = parse instruction
        let (hor, ver) = pos
        match i.Direction with
        | Down -> (hor, ver + i.Value)
        | Up -> (hor, ver - i.Value)
        | Forward -> (fst pos + i.Value, ver)

    let destination instructions = instructions |> List.fold nextPosition (0, 0)

    let calc(pos: int * int) = fst pos * snd pos

    let solution input = input |> destination |> calc

module Part2 =
    open Parser

    let nextPosition (pos: int * int * int) (instruction: string) =
        let i = parse instruction
        let (hor, ver, aim) = pos
        match i.Direction with
        | Down -> (hor, ver, aim + i.Value)
        | Up -> (hor, ver, aim - i.Value)
        | Forward -> (hor + i.Value, ver + (i.Value * aim), aim)

    let destination instructions = instructions |> List.fold nextPosition (0, 0, 0)

    let calc(pos: int * int * int) =
        let (hor, ver, aim) = pos
        hor * ver

    let solution input = input |> destination |> calc
