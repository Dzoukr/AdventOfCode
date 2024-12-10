#load "../InputReader.fsx"

open System

let input = InputReader.read "2024" "09"

let example = "2333133121414131402" |> Array.singleton

let parse (input:string []) = input.[0]

let empty = "."

let toBlock (s:string) =
    s.ToCharArray()
    |> Array.mapi (fun i v ->
        let value = v |> string |> int
        [| 0 .. value - 1 |] |> Array.map (fun _ ->
            let ind = i / 2
            if i % 2 = 0 then string ind else empty
        ) 
    )
    |> Array.filter (Array.isEmpty >> not)

module Part1 =
    let rec compress (s:string []) =
        let emptyIndex = s |> Array.tryFindIndex (fun x -> x = empty)
        match emptyIndex with
        | Some i ->
            let last = s |> Array.last
            let withoutLast = s.[..^1]
            withoutLast.[i] <- last
            compress withoutLast
        | None -> s
    let result =
        input
        |> parse
        |> toBlock
        |> Array.concat
        |> Array.filter (fun x -> String.IsNullOrEmpty x |> not)
        |> compress
        |> Array.mapi (fun i v -> int64 i * (int64 v))
        |> Array.sum

module Part2 =
    let reorder (sx:string [] []) =
        let rec foo ind (acc:string [] []) =
            if ind >= 0 then
                let value = acc.[ind]
                let possibleSlotIndex = acc |> Array.tryFindIndex (fun x ->
                    (x |> Array.filter ((=) empty) |> Array.length) >= value.Length && (value |> Array.contains empty |> not)
                )
                match possibleSlotIndex with
                | Some i when i < ind ->
                    let firstAvailable = acc.[i] |> Array.findIndex ((=) empty)
                    acc.[i].[firstAvailable .. (firstAvailable + value.Length - 1)] <- value
                    acc.[ind] <- Array.init value.Length (fun _ -> empty)
                    foo (ind - 1) acc
                | _ -> foo (ind - 1) acc
            else acc
        foo (sx.Length - 1) sx
    let result =
        input
        |> parse
        |> toBlock
        |> reorder
        |> Array.concat
        |> Array.mapi (fun i v -> if v <> empty then (int64 i) * (int64 v) else 0L)
        |> Array.sum