#time

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
    | (L,8) | (R,7) | (D,8) | (D,5) -> 8
    | _                             -> 9
    
"LLRLRUDD" |> Seq.map (fun c -> ToInstruction c) |> Seq.toList |> List.fold (fun x y -> GetNextValue x y) 5