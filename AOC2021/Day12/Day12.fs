module AOC2021.Day12
open Utils

module Part1 =
    type 'a Edge = 'a * 'a
    type 'a Graph = 'a list * 'a Edge list
    type 'a Node = 'a * 'a list
    type 'a AdjacencyGraph = 'a Node list

    let g = (["DA";"xn";"KD";"ut";"gx"; "ll"; "dj"; "PW"; "dg"; "ak"; "start"; "YM"; "end"],
        [("DA","xn");("KD","ut");("gx","ll");("dj","PW");("xn","dj");("ll","ut");("xn","gx");
        ("dg","ak");("DA","start");("ut","gx");("YM","ll");("dj","DA");("ll","xn");("dj","YM");
        ("start","PW");("dj","start");("PW","gx");("YM","gx");("xn","ak");("PW","ak");("xn","PW");
        ("YM","end");("end","ll");("ak","end");("ak","DA")])

    let graph2AdjacencyGraph ((ns, es) : 'a Graph) : 'a AdjacencyGraph = 
        let nodeMap = ns |> List.map(fun n -> n, []) |> Map.ofList
        (nodeMap,es) 
        ||> List.fold(fun map (a,b) -> map |> Map.add a (b::map.[a]) |> Map.add b (a::map.[b]))
        |> Map.toList

    let paths start finish (g : AdjacencyGraph<string>) = 
        let map = g |> Map.ofList
        let rec loop route visited = [
            let current = List.head route
            if current = finish then
                yield List.rev route
            else
                for (next:string) in map.[current] do
                    if not (visited |> Set.contains next) || (String.isUpper next) then
                        yield! loop (next::route) (Set.add next visited) 
        ]
        loop [start] <| Set.singleton start

    let solution = g |> graph2AdjacencyGraph |> paths "start" "end" |> List.length

module Part2 =
    open Part1

    let pathsWithSmallCave start finish smallCave (g : AdjacencyGraph<string>) = 
        let map = g |> Map.ofList
        let rec loop route visited smallCaveVisited = [
            let current = List.head route
            if current = finish then
                yield List.rev route
            else
                for (next:string) in map.[current] do
                    if  (next = smallCave && smallCaveVisited < 2) || not (visited |> Set.contains next) || (String.isUpper next) then
                        let smallCaveVisited = if next = smallCave then smallCaveVisited + 1 else smallCaveVisited
                        yield! loop (next::route) (Set.add next visited) (smallCaveVisited) 
        ]
        loop [start] (Set.singleton start) 0
    
    let solution = ["xn";"ut";"gx"; "ll"; "dj"; "dg"; "ak"] 
                |> List.map (fun x -> g |> graph2AdjacencyGraph |> pathsWithSmallCave "start" "end" x) 
                |> List.concat 
                |> List.distinct 
                |> List.length
    