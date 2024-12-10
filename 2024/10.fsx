#load "../InputReader.fsx"

open System

let input = InputReader.read "2024" "10"

let exampleTxt = """
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
"""

let example = exampleTxt.Split("\n", StringSplitOptions.RemoveEmptyEntries)

let parse (input:string []) =
    input
    |> Array.toList
    |> List.map (fun x -> x.ToCharArray() |> Array.toList |> List.map (fun x -> int <| x.ToString()))

let rec findTrailheads (map: int list list) path (acc: (int * int) list list) (y,x)=
    let value = map.[y].[x]
    let path = (y,x) :: path
    if value = 9 then path :: acc
    else
        let neighbors = [ (y-1,x); (y,x-1); (y,x+1); (y+1,x) ]
        let nexts =
            neighbors
            |> List.filter (fun (y,x) -> y >= 0 && y < map.Length && x >= 0 && x < map.[0].Length)
            |> List.filter (fun (y,x) -> map.[y].[x] = value + 1)
            |> List.filter (fun (y,x) -> path |> List.exists (fun (y1,x1) -> y1 = y && x1 = x) |> not)
        [
            for n in nexts do
                yield! findTrailheads map path acc n
        ]

let allTrailheads (map: int list list) =
    [
        for y in 0 .. map.Length - 1 do
            for x in 0 .. map.[0].Length - 1 do
                if map.[y].[x] = 0 then
                    yield (findTrailheads map [] [] (y,x))
    ]

module Part1 =
    let result =
        input
        |> parse
        |> allTrailheads
        |> List.map (List.distinctBy _.Head >> List.length)
        |> List.sum

module Part2 =
    let result =
        input
        |> parse
        |> allTrailheads
        |> List.map _.Length
        |> List.sum