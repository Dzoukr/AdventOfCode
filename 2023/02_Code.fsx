#load "InputReader.fsx"

let input = InputReader.read "02"

type Color =
    | Red
    | Green
    | Blue

type ColorValue = {
    Color : Color
    Value : int
}

module ColorValue =
    let create (c, v) = { Color = c; Value = v }

type Round = ColorValue list

type Game = {
    Id : int
    Rounds : Round list
}

type Bag = ColorValue list

let parseColorValue (c:string) =
    let parts = c.Trim().Split(" ")
    let value = int parts.[0]
    match parts.[1].ToUpperInvariant() with
    | "BLUE" -> { Color = Blue; Value = value }
    | "GREEN" -> { Color = Green; Value = value }
    | "RED" -> { Color = Red; Value = value }
    | x -> failwith $"Unrecognized color {x}! Bad Lef, bad!"

let parseRound (r:string) : Round =
    r.Split(",")
    |> Array.map parseColorValue
    |> Array.toList

let parseGame (s:string) =
    let parts = s.Split(":")
    let colors = parts.[1].Split(";")
    { Id = int (parts.[0].Split(" ").[1]); Rounds = colors |> Array.toList |> List.map parseRound }

let parseGames (s:string seq) =
    s
    |> Seq.map parseGame
    |> Seq.toList

let tryApplyRound (bag:Bag) (round:Round) : Bag option =
    let folder (bag:Bag option) (c:ColorValue) =
        bag
        |> Option.bind (List.tryFind (fun x -> x.Color = c.Color && x.Value >= c.Value))
        |> Option.bind (fun cv ->
            bag
            |> Option.map (fun b -> b |> List.filter (fun x -> x.Color <> cv.Color))
            |> Option.map (fun others -> { cv with Value = cv.Value - c.Value } :: others)
        )
    round |> List.fold folder (Some bag)

let isGamePossible (bag:Bag) (game:Game) =
    game.Rounds
    |> List.exists (fun x -> tryApplyRound bag x |> Option.isNone)
    |> not

let getPower (game:Game) =
    game.Rounds
    |> List.collect id
    |> List.groupBy _.Color
    |> List.map (fun (_,cvx) ->
        cvx |> List.max
    )
    |> List.map _.Value
    |> List.fold (*) 1

let initialBag : Bag = [ Red, 12; Green, 13; Blue, 14 ] |> List.map ColorValue.create
let games =
    input
    |> Array.toList
    |> List.map parseGame
let possibleGameIdsSum = games |> List.filter (isGamePossible initialBag) |> List.map _.Id |> List.sum
let sumOfPower = games |> List.map getPower |> List.sum