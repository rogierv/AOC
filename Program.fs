open Day03.Parser
open Day03

let result = 
    let wires = parse "AOC2019/Day03/Input.txt"
    Part2.Solution wires.[0] wires.[1]

printf $"{result}"
