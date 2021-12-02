#time
#r "nuget: FSharpx.Collections"

open FSharpx.Collections

[0..10] |> List.mapIf (fun x -> x % 2 = 0) (fun x -> x + 1)

[0..10] |> List.skip 5

[0;5;70;6;7;8] |> List.split (fun x -> x = 7)

List.transpose [['a'..'d'];['a'..'d']]

[0..10] |> List.groupNeighboursBy (fun x -> x > 5 && x <= 7)

let x = DList.singleton 5 |> DList.conj 3

x |> DList.isEmpty

let t = (0, 1, 2)