#load "../InputReader.fsx"

let input = InputReader.read "2024" "07"

open System

let exampleTxt = """
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"""

let example = exampleTxt.Split("\n", StringSplitOptions.RemoveEmptyEntries)

type Row = {
    Result : int64
    Numbers : int64 array
}

let parse (t:string []) =
    t
    |> Array.map (fun (item:string) ->
        let parts = item.Split(": ")
        let result = int64 parts.[0]
        let numbers = parts.[1].Split(" ") |> Array.map int64
        { Result = result; Numbers = numbers }
    )

let canBeSolved ops (row:Row)  =
    let rec solver (numbers:int64 list) acc =
        match numbers with
        | [] -> acc = row.Result
        | h::t -> ops |> List.exists (fun op -> solver t (op acc h))
    solver (row.Numbers |> Array.toList) 0

module Part1 =
    let ops = [(+);(*)]
    let result =
        input
        |> parse
        |> Array.filter (canBeSolved ops)
        |> Array.map _.Result
        |> Array.sum

module Part2 =
    let (|^|) x y = $"{x}{y}" |> int64
    let ops = [(+);(*);(|^|)]
    let result =
        input
        |> parse
        |> Array.filter (canBeSolved ops)
        |> Array.map _.Result
        |> Array.sum