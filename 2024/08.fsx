#load "../InputReader.fsx"

open System.Collections.Generic

let input = InputReader.read "2024" "08"

open System

let exampleTxt = """
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"""

let example = exampleTxt.Split("\n", StringSplitOptions.RemoveEmptyEntries)

type Data = {
    Map : char [] []
    Antennas : Dictionary<char, (int * int) []>
}

let parse (input: string []) =
    let map = input |> Array.map (fun x -> x.ToCharArray())
    let antennas = Dictionary<char, (int * int) []>()
    for i in 0..map.Length - 1 do
        for j in 0..map.[i].Length - 1 do
            let key = map.[i].[j]
            if key <> '.' then
                if antennas.ContainsKey key then antennas.[key] <- ([|(i, j)|] |> Array.append antennas.[key])
                else antennas.Add(map.[i].[j], [|(i, j)|])
    { Map = map; Antennas = antennas }

let isInMap (map:char [] []) (y:int, x:int) =
    y >= 0 && y < map.Length
    && x >= 0 && x < map.[0].Length

module Part1 =
    let findAntinodes map (px:(int * int) []) =
        px |> Array.map (fun (y,x) ->
            let others = px |> Array.filter (fun (y1,x1) -> x <> x1 && y <> y1)
            others |> Array.collect (fun (y1,x1) ->
                let dx,dy = (x1 - x), (y1 - y)
                [| (y1 - dy, x1 - dx); (y1 + dy, x1 + dx) |]
            )
        )
        |> Array.concat
        |> Array.filter (fun (y,x) -> px |> Array.contains (y,x) |> not)
        |> Array.filter (isInMap map)
    let result =
        let data = parse input
        data.Antennas.Values
        |> Seq.map (findAntinodes data.Map)
        |> Array.concat
        |> Array.distinct
        |> Array.length

module Part2 =
    let rec findNext map op dy dx y x acc =
        let next = (op y dy, op x dx)
        if isInMap map next then
            findNext map op dy dx (fst next) (snd next) (acc |> Array.append [|next|])
        else acc
    let findAntinodes map (px:(int * int) []) =
        px |> Array.map (fun (y,x) ->
            let others = px |> Array.filter (fun (y1,x1) -> x <> x1 && y <> y1)
            others |> Array.collect (fun (y1,x1) ->
                let dx,dy = (x1 - x), (y1 - y)
                [|
                    yield! findNext map (+) dy dx y x [|(y, x)|]
                    yield! findNext map (-) dy dx y x [|(y, x)|]
                |]
            )
        )
        |> Array.concat
        |> Array.filter (isInMap map)
    let result =
        let data = parse input
        data.Antennas.Values
        |> Seq.map (findAntinodes data.Map)
        |> Array.concat
        |> Array.distinct
        |> Array.length