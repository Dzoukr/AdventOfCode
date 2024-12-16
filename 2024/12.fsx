#load "../InputReader.fsx"

open System
open System.Collections.Generic

let input = InputReader.read "2024" "12"

let exampleTxt = """
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
"""

let example = exampleTxt.Split("\n", StringSplitOptions.RemoveEmptyEntries)

let parse (input:string []) =
    input
    |> Array.toList
    |> List.map (fun x -> x.ToCharArray() |> Array.toList)

type Region = {
    Key : char
    Points : (int * int) list
}

let neighbors (y,x) = [ (y-1,x); (y,x-1); (y,x+1); (y+1,x) ]

let areNeighbors (y1,x1) (y2,x2) = (abs (y1 - y2) = 1 && (x1 = x2)) || ((y1 = y2) && abs (x1 - x2) = 1)

let isInMap (map: char list list) (y,x) =
    y >= 0 && y < map.Length && x >= 0 && x < map.[0].Length

let allRegions (map:char list list) =
    let d = Dictionary<char, (int * int) list>()
    for y in 0 .. map.Length - 1 do
        for x in 0 .. map.[0].Length - 1 do
            let value = map.[y].[x]
            if d.ContainsKey value then d.[value] <- (y,x) :: d.[value]
            else d.Add(value, [(y,x)])
    d
    |> Seq.map (fun (kv) -> { Key = kv.Key; Points = kv.Value })
    |> Seq.toList

let redistribute (r:Region) =
    let existingNeighbors (y,x) =
        (y,x)
        |> neighbors
        |> List.filter (fun x -> r.Points |> List.contains x)
    let rec findGroups (acc: (int * int) list) (toCheck: (int * int) list) =
        match toCheck with
        | [] -> acc
        | h :: xs ->
            let n = existingNeighbors h |> List.filter (fun x -> x <> h) |> List.filter (fun x -> acc |> List.contains x |> not)
            findGroups (h :: n @ acc) (n @ xs)
    let groups = ResizeArray<(int * int) list>()
    for (y,x) in r.Points do
        let ng = findGroups [] [(y,x)]
        if groups |> Seq.collect id |> Seq.exists (fun z -> z = (y,x)) |> not then
            groups.Add(ng |> List.distinct)
    groups
    |> Seq.toList
    |> List.map (fun x -> { Key = r.Key; Points = x })

module Part1 =
    let calculatePerimeter (map: char list list) (r:Region) =
        r.Points
        |> List.map (fun (y,x) ->
            neighbors (y,x)
            |> List.filter (fun (y,x) -> not <| isInMap map (y,x) || map.[y].[x] <> r.Key)
            |> List.length
        )
        |> List.sum
    let result =
        let map = input |> parse
        map
        |> allRegions
        |> List.collect redistribute
        |> List.map (fun x -> x.Points.Length * calculatePerimeter map x) |> List.sum

module Part2 =
    let calculatePerimeterDiscounted (map: char list list) (r:Region) =
        let isNotInMapOrSameGroup (y,x) = not <| isInMap map (y,x) || map.[y].[x] <> r.Key
        let edges = [
            for (y,x) in r.Points do
                if isNotInMapOrSameGroup (y-1,x) then yield (0,y,x)
                if isNotInMapOrSameGroup (y+1,x) then yield (1,y,x)
                if isNotInMapOrSameGroup (y,x+1) then yield (2,y,x)
                if isNotInMapOrSameGroup (y,x-1) then yield (3,y,x)
        ]
        let rec glue dirToCheck (acc:((int * int) list) list) toCheck =
            match toCheck with
            | [] -> acc
            | (dir,y,x) :: xs ->
                if dir = dirToCheck then
                    let applicable,others = acc |> List.partition (fun l -> l |> List.exists (fun (y1,x1) -> areNeighbors (y1,x1) (y,x)))
                    let newApp =
                        match applicable with
                        | [] -> [(y,x)]
                        | h :: t -> ((y,x) :: h) @ ( t |> List.concat)
                    glue dirToCheck (newApp :: others) xs
                else glue dirToCheck acc xs
        [0..3]
        |> List.map (fun i -> edges |> glue i [])
        |> List.map (fun x -> x.Length)
        |> List.sum
    let result =
        let map = input |> parse
        map
        |> allRegions
        |> List.collect redistribute
        |> List.map (fun x -> x.Points.Length * calculatePerimeterDiscounted map x)
        |> List.sum