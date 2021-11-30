#time

let a1 = ("not-a-real-room-404[oarel]").Split([|']';'['|], 4)

let a2 = a1.[0]
let split = a2.LastIndexOf '-'

let checksum = a1.[1]
let encryptedName = a2.Substring(0,(split)).Replace("-", "")
let sectorId = a2.Substring(split + 1)


let x =
    encryptedName
    |> Seq.toList
    |> Seq.countBy id
    |> Seq.sortBy (fun x -> -snd x, fst x)
    |> Seq.take 5
    |> Seq.fold (fun x y -> x + (fst y |> string)) ""

x