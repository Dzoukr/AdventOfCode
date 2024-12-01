#load "InputReader.fsx"

open System

let input = InputReader.read "03"

let isSymbol (c:char) =
    Char.IsNumber(c) |> not
    && c <> '.'

type PartNumber = {
    StartIndex : int
    RowIndex : int
    Value : string
}

let parseRow (i:int) (s:string) =
    let folder (acc:bool * int * PartNumber list) (c:char) =
        let isReading, index, numbers = acc
        match isReading, Char.IsNumber(c) with
        | false, true -> true, index + 1, ({ StartIndex = index; RowIndex = i; Value = string c } :: numbers)
        | true, true -> true, index + 1, ({ numbers.Head with Value = numbers.Head.Value + (string c)} :: numbers.Tail)
        | _, false -> false, index + 1, numbers
    s
    |> Seq.fold folder (false,0,[])
    |> (fun (_,_,x) -> x)

let getSurroundings (pn:PartNumber) =
    let xs = [pn.StartIndex - 1 .. (pn.StartIndex) + pn.Value.Length]
    let ys = [pn.RowIndex - 1; pn.RowIndex; pn.RowIndex + 1]
    [
        for y in ys do
            for x in xs do
                yield y,x
    ]
    |> List.filter (fun (y,x) -> x >= 0 && y >= 0)

type Symbol = {
    Y : int
    X : int
    Value : char
}

let findSymbols (rows:string []) (pn:PartNumber) : Symbol list =
    let rowLength = rows |> Array.tryHead |> Option.map _.Length |> Option.defaultValue 0
    getSurroundings pn
    |> List.filter (fun (y,x) -> y < rows.Length && x < rowLength)
    |> List.collect (fun (y,x) ->
        let value = rows.[y].[x]
        if value |> isSymbol then [{ Y = y; X = x; Value = value }] else []
    )

let partNumbers =
    input
    |> Array.mapi parseRow
    |> Array.toList
    |> List.collect id

let numbers =
    partNumbers
    |> List.filter (findSymbols input >> List.isEmpty >> not)
    |> List.map (_.Value >> int)
    |> List.sum

let gears =
    partNumbers
    |> List.map (fun pn -> pn, findSymbols input pn)
    |> List.filter (snd >> List.isEmpty >> not)
    |> List.collect (fun (pn,symbols) -> symbols |> List.map (fun x -> x, pn))
    |> List.groupBy fst
    |> List.map (fun (s,x) -> x |> List.map snd |> List.map (_.Value >> int))
    |> List.filter (fun x -> List.length x = 2)
    |> List.map (List.fold (*) 1)
    |> List.sum