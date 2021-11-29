module AOC2016.Day01

open System
open System.IO

module Parser =
    let parse input = (input |> File.ReadLines |> Seq.head).Split ',' |> Array.map (fun x -> x.TrimStart())

module Part1 =
    type Position = {
        Coordinates: (int * int)
        Facing: char
    }

    let FindDestination (instructions:string[]) =
        let destination = 
            let folder x (y:string) = 
                let steps = y.[1..] |> int
                match (x.Facing, y.[0]) with
                | ('N', 'R') -> { Coordinates = (fst x.Coordinates, snd x.Coordinates + steps); Facing = 'E' }
                | ('N', _)   -> { Coordinates = (fst x.Coordinates, snd x.Coordinates - steps); Facing = 'W' }        
                | ('E', 'R') -> { Coordinates = (fst x.Coordinates - steps, snd x.Coordinates); Facing = 'S' }
                | ('E', _)   -> { Coordinates = (fst x.Coordinates + steps, snd x.Coordinates); Facing = 'N' }
                | ('S', 'R') -> { Coordinates = (fst x.Coordinates, snd x.Coordinates - steps); Facing = 'W' }
                | ('S', _)   -> { Coordinates = (fst x.Coordinates, snd x.Coordinates + steps); Facing = 'E' }
                | ('W', 'R') -> { Coordinates = (fst x.Coordinates + steps, snd x.Coordinates); Facing = 'N' }
                | _          -> { Coordinates = (fst x.Coordinates - steps, snd x.Coordinates); Facing = 'S' }
            instructions |> Array.fold folder { Coordinates = (0,0); Facing = 'N' }
        destination.Coordinates
    
    let CalculateManhatten (start:int * int) (destination: int * int) =
        Math.Abs(fst start - fst destination) + Math.Abs(snd start - snd destination)

    let Solution input = FindDestination input |> CalculateManhatten (0,0)

module Part2 =
    type Position = {
        Coordinates: (int * int)
        Facing: char
        PathTaken: (int * int) list
    }

    let WalkPath (position:int * int) (facing:char) (steps:int) : (int * int) list =
        match facing with
        | 'N' -> [1..steps] |> List.map (fun step -> (fst position + step, snd position))
        | 'E' -> [1..steps] |> List.map (fun step -> (fst position, snd position + step))
        | 'S' -> [1..steps] |> List.map (fun step -> (fst position - step, snd position))
        | 'W' -> [1..steps] |> List.map (fun step -> (fst position, snd position - step))

    let FindDestination (instructions:string[]) =
        let destination = 
            let folder x (y:string) = 
                let steps = y.[1..] |> int
                match (x.Facing, y.[0]) with
                | ('N', 'R') -> { Coordinates = (fst x.Coordinates, snd x.Coordinates + steps); Facing = 'E'; 
                                  PathTaken = x.PathTaken @ WalkPath (fst x.Coordinates, snd x.Coordinates) 'E' steps }
                | ('N', _)   -> { Coordinates = (fst x.Coordinates, snd x.Coordinates - steps); Facing = 'W'; 
                                  PathTaken = x.PathTaken @ WalkPath (fst x.Coordinates, snd x.Coordinates) 'W' steps  }        
                | ('E', 'R') -> { Coordinates = (fst x.Coordinates - steps, snd x.Coordinates); Facing = 'S'; 
                                  PathTaken = x.PathTaken @ WalkPath (fst x.Coordinates, snd x.Coordinates) 'S' steps  }
                | ('E', _)   -> { Coordinates = (fst x.Coordinates + steps, snd x.Coordinates); Facing = 'N'; 
                                  PathTaken = x.PathTaken @ WalkPath (fst x.Coordinates, snd x.Coordinates) 'N' steps  }
                | ('S', 'R') -> { Coordinates = (fst x.Coordinates, snd x.Coordinates - steps); Facing = 'W'; 
                                  PathTaken = x.PathTaken @ WalkPath (fst x.Coordinates, snd x.Coordinates) 'W' steps  }
                | ('S', _)   -> { Coordinates = (fst x.Coordinates, snd x.Coordinates + steps); Facing = 'E'; 
                                  PathTaken = x.PathTaken @ WalkPath (fst x.Coordinates, snd x.Coordinates) 'E' steps  }
                | ('W', 'R') -> { Coordinates = (fst x.Coordinates + steps, snd x.Coordinates); Facing = 'N'; 
                                  PathTaken = x.PathTaken @ WalkPath (fst x.Coordinates, snd x.Coordinates) 'N' steps  }
                | _          -> { Coordinates = (fst x.Coordinates - steps, snd x.Coordinates); Facing = 'S'; 
                                  PathTaken = x.PathTaken @ WalkPath (fst x.Coordinates, snd x.Coordinates) 'S' steps  }
            instructions |> Array.fold folder { Coordinates = (0,0); Facing = 'N'; PathTaken = [] }
        destination.PathTaken

    let FindFirstLocationVisitedTwice (locations:(int * int) list): (int * int) =
        let pos = 
            locations 
            |> List.groupBy id
            |> List.choose (fun (k,v) -> match v.Length with
                                         | x when x > 1 -> Some k
                                         | _            -> None)
        pos.[0]

    let Solution input = FindDestination input |> FindFirstLocationVisitedTwice |> Part1.CalculateManhatten (0,0)
