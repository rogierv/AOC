module AOC2021.Day16Tests

open FsUnit.Xunit
open Xunit

open Day16

[<Fact>]
let test1() = hexToBinary "A" |> should equal "1010"

[<Fact>]
let test2() = hexToBinary "1" |> should equal "0001"

[<Fact>]
let test3() = hexToBinaryString "D2FE28" |> should equal "110100101111111000101000"

[<Fact>]
let test3a() = hexToBinaryString "8A004A801A8002F478"|> should equal "100010100000000001001010100000000001101010000000000000101111010001111000"

[<Fact>]
let test4() = packetVersion "110100101111111000101000" |> should equal 6

[<Fact>]
let test5() = packetType "110100101111111000101000" |> should equal Literal

[<Fact>]
let test6() = packetType "110101101111111000101000" |> should equal Operator

[<Fact>]
let test7() = operatorLengthType "11101110000000001101010000001100100000100011000001100000" |> should equal (NumberOfSubPackets 3)

[<Fact>]
let test8() = operatorLengthType "00111000000000000110111101000101001010010001001000000000" |> should equal (TotalLengthInBits 27)

[<Fact>]
let test9() = getSubPackets "00111000000000000110111101000101001010010001001000000000" |> should equal ["11010001010"; "0101001000100100"]

[<Fact>]
let test10() = getSubPackets "11101110000000001101010000001100100000100011000001100000" |> should equal ["010100000011001000"; "000100011000001100"]

[<Fact>]
let test11() = getSubPackets (hexToBinaryString "8A004A801A8002F478") |> should equal ["001010100000000001101010000000000000101111010001111000"]

[<Fact>]
let test12() = solution (hexToBinaryString "8A004A801A8002F478") |> should equal 16

[<Fact>]
let test13() = solution (hexToBinaryString "620080001611562C8802118E34") |> should equal 12

