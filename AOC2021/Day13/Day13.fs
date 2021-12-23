module AOC2021.Day13
open Utils
open FSharpPlus

module Parser =
    let parse input = 
        input 
        |> List.map (fun x -> 
            x 
            |> String.split [","] |> Seq.toList
            |> fun x -> (x.[0] |> int, x.[1] |> int))

module Part1 =
    let findPaperSize (dots:(int*int) list) = (dots |> List.map fst |> List.max, dots |> List.map snd |> List.max)

    let foldAtY (dot:(int*int)) (foldY:int) (size:(int*int)) =
        let x, y = dot
        let _, sy = size
        let y = if y >= foldY then sy - y 
                else y + (sy - foldY) - foldY
        (x, y)

    let foldAtX (dot:(int*int)) (foldX:int) (size:(int*int)) =
        let x, y = dot
        let sx, _ = size
        let x = if x >= foldX then sx - x 
                else x + (sx - foldX) - foldX
        (x, y)

    let fold (axis:char) (along:int) (dots:(int*int) list) =
        let paperSize = findPaperSize dots
        match axis with
        | 'x' -> dots |> List.map (fun dot -> foldAtX dot along paperSize)
        | 'y' -> dots |> List.map (fun dot -> foldAtY dot along paperSize)

    let solution (instructions:(char*int) list) dots = dots |> fold (fst (instructions.[0])) (snd (instructions.[0])) |> List.distinct |> List.length

module Part2 =
    open Part1

    let solution (instructions:(char*int) list) dots = 
        instructions 
        |> List.fold (fun acc instruction -> acc |> fold (fst (instruction)) (snd (instruction)) |> List.distinct) dots

    let arr = [(6,0);(10,7)]
    let ps = arr |> findPaperSize
    let createMap dots = Array2D.create (snd (findPaperSize dots) + 1) (fst (findPaperSize dots) + 1) '.'

    let print (map:char[,]) dots = dots |> List.iter (fun (x,y) -> map[y,x] <- 'x')
    