#time

let inline charToInt c = int c - int '0'
let inline intToChar i = char i + char '0'


//let card = array2D [[1;0;3];[4;5;8];[1;3;8]]
//card[0";"0] <- 4
//card

//let card2:int[";"] = Array2D.zeroCreate 5 5
let arrays = [|[|1;1;1;1;1|];[|1;1;1;1;1|];[|1;1;1;1;1|];[|1;1;1;1;1|];[|1;1;1;1;1|]|]
let card2 = Array2D.init 5 5 (fun i j -> arrays[i][j])


let input =
    [|[|"x"; "x"; "x"; "x"; "x"|]
      [|"10"; "16"; "15"; "9"; "19"|]
      [|"18"; "8"; "23"; "26"; "20"|]
      [|"22"; "11"; "13"; "6"; "5"|]
      [|"2"; "0"; "12"; "3"; "7"|]|]
let card = Array2D.init 5 5 (fun i j -> input[i][j])

let mark (card:string[,]) (value:string) =
    card |> Array2D.map (fun (v:string) -> if v = value then "x" else v)

let newCard = ["9";"12";"1";"7";"19";"5";"24";"1";"0"] |> List.fold (fun a b -> mark a b) card

card[4,*] |> Array.fold (fun x y -> if y <> "x" then x + (y |> int) else x ) 0

let isWinningCard (card:string[,]) =
    let check = Array.create 5 "x"
    [0..4] |> List.fold (fun x i ->  if card[i,*] = check || card[*,i] = check then true else x) false

let sumOfRemainingValues (card:string[,]) =
    [0..4] |> List.fold (fun x i -> x + (card[i,*] |> Array.fold (fun x y -> if y <> "x" then x + (y |> int) else x ) 0)) 0

sumOfRemainingValues card

let play card numbers =
    let letsPlay =
        numbers
        |> List.fold
            (fun a b ->
                let (c, i, r, winningCard) = a
                if winningCard then (c, i, r, winningCard) else
                if isWinningCard (mark c b) then
                    let newCard = mark c b
                    (newCard, b, sumOfRemainingValues newCard, true)       
                else
                    (mark c b, i, r, winningCard))
            (card, "", 0, false)
    let (_, lastValue, sum, _) = letsPlay
    (lastValue, sum)
    


["7";"4";"9";"5";"11";"17";"23";"2";"0";"14";"21";"24";"10";"16";"13";"6";"15";"25";"12";"22";"18";"20";"8";"19";"3";"26";"1"] 
|> play card