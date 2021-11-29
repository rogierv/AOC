#time

let IsValidTriangle s1 s2 s3 = (s1 + s2 > s3) && (s1 + s3 > s2) && (s2 + s3 > s1)

IsValidTriangle 5 10 15

let sample = [[1;2;3];[3;4;5];[5;6;7];[1;2;3];[3;4;5];[5;6;7];[1;2;3];[3;4;5];[5;6;7]]

sample |> List.splitInto (sample.Length / 3) |> List.map (fun x -> List.transpose x) |> List.concat |> List.filter (fun x -> IsValidTriangle x.[0] x.[1] x.[2]) |> List.length