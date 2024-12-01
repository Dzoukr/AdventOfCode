#load "InputReader.fsx"

open System

let input = InputReader.read "04"

type Card = {
    Id : int
    Wins : int list
    Tries : int list
}

let parseCard (s:string) : Card =
    let parts = s.Split("|")
    let card = parts.[0].Split(":")
    let i = card.[0].Split(" ", StringSplitOptions.RemoveEmptyEntries).[1].Trim() |> int
    let wins = card.[1].Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map (_.Trim() >> int) |> Array.toList
    let tries = parts.[1].Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map (_.Trim() >> int) |> Array.toList
    { Id = i; Wins = wins; Tries = tries }

let getWinningNumbers (c:Card) =
    let w = Set.ofList c.Wins
    let t = Set.ofList c.Tries
    Set.intersect w t |> Set.toList

let calculatePoints (c:Card) =
    let wins = c |> getWinningNumbers
    Math.Pow(2., (float wins.Length) - 1.) |> int

let cards =
    input
    |> List.ofArray
    |> List.map parseCard

let folder (acc:int[]) (item:Card) =
    let currentIndex = item.Id - 1
    let wins =
        item
        |> getWinningNumbers
        |> List.length
    let multiplier = acc.[currentIndex]
    for i in [currentIndex + 1 .. currentIndex + wins] do
        acc.[i] <- acc.[i] + multiplier
    acc

let points =
    cards
    |> List.map calculatePoints
    |> List.sum

let getInitArray (cx:Card list) =
    Array.init cx.Length (fun _ -> 1)

let total =
    cards
    |> List.fold folder (getInitArray cards)
    |> Array.sum