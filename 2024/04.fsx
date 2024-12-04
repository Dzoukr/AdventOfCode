#load "../InputReader.fsx"

let input = InputReader.read "2024" "04"

open System

let exampleTxt = """
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"""

let example = exampleTxt.Split("\n", StringSplitOptions.RemoveEmptyEntries)

let toData (t:string array) = t |> Array.map _.ToCharArray()

let findInData (data:char [][]) (toFind:(int * int) []) =
    toFind |> Array.map (fun (y,x) ->
        if data.Length <= y || y < 0 || data.[y].Length <= x || x < 0 then '.'
        else data.[y].[x]
    )

module Part1 =
    let phrase = "XMAS" |> Seq.toArray
    let isPhrase (c:char []) = c = phrase
    let search (data:char [][]) y x =
        let axisDiffs = [| -1 .. 1 |] |> Array.collect (fun x -> [| -1 ..1 |] |> Array.map (fun y -> y, x))
        let coords =
            [|
                for (dy,dx) in axisDiffs do
                    [|
                        for l in 0 .. phrase.Length - 1 do
                            yield y + (dy * l), x + (dx * l)
                    |]
            |]
        [|
            for c in coords do
                let text = findInData data c
                if isPhrase text then
                    yield c
        |]
    let find (data:char [][]) =
        [|
            for i in 0 .. data.Length - 1 do
                for j in 0 .. data.[i].Length - 1 do
                    if data.[i].[j] = 'X' then
                        yield! search data i j
        |]
    let result =
        find (toData input)
        |> Array.map (fun x -> x |> Array.sort)
        |> Array.distinct
        |> Array.length

module Part2 =
    let search (data:char [][]) y x =
        let exp = "MS" |> Seq.toArray
        let op1 = [| y + 1, x + 1; y - 1, x - 1 |]
        let op2 = [| y + 1, x - 1; y - 1, x + 1 |]
        let chars1 = op1 |> findInData data |> Array.sort
        let chars2 = op2 |> findInData data |> Array.sort
        if chars1 = exp && chars2 = exp then [|y,x|]
        else [||]
    let find (data:char [][]) =
        [|
            for i in 0 .. data.Length - 1 do
                for j in 0 .. data.[i].Length - 1 do
                    if data.[i].[j] = 'A' then
                        yield! search data i j
        |]
    let result =
        find (toData input)
        |> Array.length