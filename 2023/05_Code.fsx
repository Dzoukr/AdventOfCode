#load "InputReader.fsx"

open System
open Microsoft.FSharp.Collections

let input = InputReader.read "05"

let parseMapping (src:string []) =
    let folder (acc:(string * (int64 * int64 * int64) list) list) (item:string) =
        if Char.IsNumber(item.[0]) then
            let lastName,lastNums = acc.Head
            let nums = item.Split(" ", StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) |> Array.map int64 |> Array.toList
            (lastName, lastNums @ [nums.[0], nums.[1], nums.[2]]) :: acc.Tail
        else
            (item, []) :: acc
    src.[1 .. src.Length - 1]
    |> Array.fold folder []
    |> List.rev

let parseSeed (src:string []) =
    src.[0].Replace("seeds:","").Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int64 |> Array.toList

let seed = input |> parseSeed
let mappings = input |> Array.filter (String.IsNullOrWhiteSpace >> not) |> parseMapping

let mappingFn (dest:int64,src:int64, range:int64) (n:int64) =
    if n >= src && n <= src + range - 1L then
        let diff = dest - src
        n + diff
    else n

let foldMappings maps =
    fun (num:int64) ->
        let f =
            maps
            |> List.fold (fun (acc:int64 -> int64) item ->
                let v = mappingFn item num
                if v <> num then mappingFn item else acc
            ) id
        f num

let num =
    mappings
    |> List.map snd
    |> List.fold (fun acc item -> acc >> foldMappings item) id

let part1 =
    seed
    |> List.map num
    |> List.min

let getNum (s:int64 list) =
    seq {
        s.[0] .. s.[0] + s.[1] - 1L
    }
    |> Seq.fold (fun acc item ->
        let v = item |> num
        if v < acc then v else acc
    ) Int64.MaxValue

open System.Linq

let part2 =
    seed
    |> List.chunkBySize 2
    |> (_.AsParallel().Select(getNum).Min())