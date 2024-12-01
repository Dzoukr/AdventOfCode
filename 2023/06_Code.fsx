#load "InputReader.fsx"

open System

let input = InputReader.read "06"

type Race = {
    Time : int64
    Distance : int64
}

let parseRaces (xs:string []) =
    xs
    |> Array.map (_.Split(":").[1].Split(" ", StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries))
    |> (fun x ->
        let fstRow = x.[0]
        [0..fstRow.Length - 1]
        |> List.map (fun y ->
            { Time = int x.[0].[y]; Distance = int x.[1].[y] }
        )
    )
    
let races = input |> parseRaces

let asOne (rx:Race list) =
    rx
    |> List.fold (fun acc (item:Race) ->
        (fst acc + string item.Time), (snd acc + string item.Distance) 
    ) ("","")
    |> (fun (t,d) -> { Time = int64 t; Distance = int64 d })
    
let calculateWins (r:Race) =
    let tryTime (waitTime:int64) =
        let time = r.Time - waitTime
        let dist = time * waitTime
        if dist > r.Distance then Some (waitTime) else None 
    
    [0L..r.Time]
    |> List.choose tryTime

let part1 =
    races
    |> List.map (calculateWins >> _.Length)
    |> List.fold (*) 1

let part2 =
    races
    |> asOne
    |> calculateWins
    |> List.length