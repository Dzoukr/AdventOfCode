#load "../InputReader.fsx"

let txtInput =
    InputReader.read "2024" "01"
    |> Array.map (fun line -> line.Split "   " |> Array.map int)
    |> Array.map (fun arr -> (arr.[0], arr.[1]))

// let txtInput =
//     [|
//         3, 4
//         4, 3
//         2, 5
//         1, 3
//         3, 9
//         3, 3
//     |]

let example1 = txtInput |> Array.map fst
let example2 = txtInput |> Array.map snd

module Part1 =
    let sorted1 = example1 |> Array.sort
    let sorted2 = example2 |> Array.sort
    let absDiff x y = if x > y then x - y else y - x
    let result1 = Array.zip sorted1 sorted2 |> Array.map (fun (x, y) -> absDiff x y) |> Array.sum

module Part2 =
    let similarity x = example2 |> Array.filter (fun y -> y = x) |> Array.length
    let result2 = Array.zip example1 example2 |> Array.map (fun (x, _) -> x * (similarity x)) |> Array.sum