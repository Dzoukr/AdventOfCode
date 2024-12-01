module InputReader

open System.IO

let read (day:string) =
    Path.Combine(__SOURCE_DIRECTORY__, "Inputs", $"{day}_Input.txt")
    |> File.ReadAllLines 