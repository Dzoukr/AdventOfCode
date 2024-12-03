#load "../InputReader.fsx"

open System.Text
open System.Text.RegularExpressions

let input = InputReader.read "2024" "03"

let parse (input:string array) = input |> String.concat ""

let pattern = @"mul\((\d+),(\d+)\)"
let regex = new Regex(pattern)

module Part1 =
    let example = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" |> Array.singleton
    let matches = regex.Matches(parse input)
    let result =
        matches
        |> Seq.map (fun x -> x.Groups.[1].Value, x.Groups.[2].Value)
        |> Seq.sumBy (fun (x,y) -> int x * int y)

module Part2 =
    let example = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" |> Array.singleton
    let text = parse input
    let validText =
        let sb = new StringBuilder()
        let mutable read = true 
        text |> Seq.iteri (fun i x ->
            if read then
                sb.Append x |> ignore
            if text.[i+1..i+7] = "don't()" then
                read <- false
            if text.[i+1..i+4] = "do()" then
                read <- true
        )
        sb.ToString()
    let matches = regex.Matches(validText)
    let result =
        matches
        |> Seq.map (fun x -> x.Groups.[1].Value, x.Groups.[2].Value)
        |> Seq.sumBy (fun (x,y) -> int x * int y)