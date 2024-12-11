#load "../InputReader.fsx"

open System

let input = InputReader.read "2024" "11"

let example = "125 17" |> Array.singleton

let parse (input:string []) =
    input.[0].Split(" ", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int64
    |> Array.toList

let (|HasEvenDigits|_|) (i:int64) =
    let digits = (i |> string).ToCharArray()
    if digits.Length % 2 = 0 then
        let l = digits.[.. (digits.Length / 2) - 1 ] |> Array.map string |> String.concat "" |> int64
        let r = digits.[(digits.Length / 2) ..] |> Array.map string |> (fun x -> if (x |> Array.filter (fun y -> y <> "0") |> Array.length) = 0 then [|"0"|] else x) |> String.concat "" |> int64
        Some (l, r)
    else None

let blinkRule (i:int64) =
    match i with
    | 0L -> [1L]
    | HasEvenDigits (l,r) -> [l;r]
    | x -> [ x * 2024L ]

let blink (xs:int64 list) =
    xs
    |> List.collect blinkRule

let rec blinkNtimes n xs =
    if n = 0 then xs
    else
        let next = blink xs
        printfn "Cycle %A" n
        blinkNtimes (n-1) next

module Part1 =
    let result = input |> parse |> blinkNtimes 25 |> List.length

module Part2 =
    let result = input |> parse |> blinkNtimes 75 |> List.length
