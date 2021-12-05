#time

#r "nuget: FParsec"

open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let ws = spaces
let str s = ws >>. pstring s .>> ws
let tuple: Parser<(int * int), unit> = pint32 .>> pchar ',' .>>. pint32
let parser = tuple .>> str "->" .>>. tuple

test parser "541,808 -> 108,808"

let parse str =
    match run parser str with
    | Success(result, _, _) -> result
    | Failure(_) -> failwith "Not Parsed"


parse "541,808 -> 108,808"

let number:Parser<int32, unit> = pint32 .>> ws
run (many number) "57  7  8 38 31"