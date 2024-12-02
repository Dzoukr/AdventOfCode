#load "../InputReader.fsx"

open System

let exampleInput = """
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"""

//let txtInput = exampleInput.Split("\n", StringSplitOptions.RemoveEmptyEntries)

let txtInput = InputReader.read "2024" "02"

let input =
    txtInput
    |> Array.map (fun x -> x.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int)

type Direction = Increasing | Decreasing

type Report = {
    Direction : Direction option
    PreviousValue : int option
    IsSafe : bool
}

let folder (acc:Report) (item:int) =
    match acc with
    | { IsSafe = false } -> acc
    | { PreviousValue = None } -> { acc with PreviousValue = Some item }
    | { PreviousValue = Some prev; Direction = prevDir } ->
        let diff = prev - item
        let dir = if diff > 0 then Decreasing else Increasing
        let absDiff = abs diff
        let isSafe = absDiff >= 1 && absDiff <= 3 && (prevDir.IsNone || prevDir = Some dir )
        { acc with PreviousValue = Some item; Direction = Some dir; IsSafe = isSafe }

module Part1 =
    let result =
        input
        |> Array.map (fun x -> x |> Array.fold folder { Direction = None; PreviousValue = None; IsSafe = true })
        |> Array.filter (fun x -> x.IsSafe)
        |> Array.length

module Part2 =
    let canBeSave (values:int array) =
        let tryReport (i:int) =
            let others = values |> Array.removeAt i
            others |> Array.fold folder { Direction = None; PreviousValue = None; IsSafe = true }
        [|0 .. values.Length - 1|]
        |> Array.map tryReport
        |> Array.exists (fun x -> x.IsSafe)
    
    let result =
        input
        |> Array.map canBeSave
        |> Array.filter id
        |> Array.length