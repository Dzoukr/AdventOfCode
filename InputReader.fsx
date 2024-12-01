module InputReader

open System.IO

let read (year:string) (day:string) =
    Path.Combine(__SOURCE_DIRECTORY__, year, "Inputs", $"{day}.txt")
    |> File.ReadAllLines 