#time
#r "nuget: morelinq"

let inline charToInt c = int c - int '0'

let rec digitsNeverDecrease (i:int) : bool =
    let s = i |> string |> Seq.toList
    match s with
    | [_] -> true
    | h :: t -> if (h |> charToInt) > (t.[0] |> charToInt) 
                then false 
                else digitsNeverDecrease (new System.String (t |> List.toArray) |> int)

let rec hasTwoAdjacentDigits (i:int) : bool =
    let s = i |> string |> Seq.toList
    match s with
    | [_] -> false
    | h :: t -> if (h |> charToInt) = (t.[0] |> charToInt) 
                then true 
                else hasTwoAdjacentDigits (new System.String (t |> List.toArray) |> int)

[246515..739105] |> List.filter (fun x -> digitsNeverDecrease x && hasTwoAdjacentDigits x) |> List.length

let group xs =
    let folder x = function
        | [] -> [[x]]
        | (h::t)::ta when h = x -> (x::h::t)::ta
        | acc -> [x]::acc
    Seq.foldBack folder xs []

let findAdjacentDigits (i:int) =
    let c = i |> string |> group |> List.filter (fun x -> x.Length <> 1) |> List.sortBy (fun x -> x.[0]) |> List.last
    c.Length = 2




3342114 |> findAdjacentDigits
