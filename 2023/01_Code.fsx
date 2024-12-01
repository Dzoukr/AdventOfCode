#load "InputReader.fsx"

open System

let input = InputReader.read "01"

let words = [
    "one"
    "two"
    "three"
    "four"
    "five"
    "six"
    "seven"
    "eight"
    "nine"
]

let wordsValues = words |> List.mapi (fun i w -> w, (i+1))

let tryFindNumber (s:string) =
    wordsValues
    |> List.choose (fun (v,i) -> if s.Contains(v) then Some i else None)
    |> List.tryHead

let asSingleNumber (xi:int list) =
    match xi |> List.tryHead, xi |> List.tryLast with
    | Some f, Some l -> int (string f +  string l)
    | Some f, None
    | None, Some f -> int (string f + string f)
    | _ -> 0

let extractNumbers (s:string) =
    let folder (acc:string * int list) (item:char) =
        if Char.IsNumber(item) then
            "", (snd acc @ [ int (string item) ])
        else
            let newWord = fst acc + string item
            match newWord |> tryFindNumber with
            | None -> newWord, (snd acc)
            | Some i -> string item, (snd acc @ [ i ])
    s
    |> Seq.fold folder ("",[])
    |> snd

input
|> Array.map (extractNumbers >> asSingleNumber)
|> Array.sum