#load "../InputReader.fsx"

let input = InputReader.read "2024" "06"

open System

let exampleTxt = """
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"""

let example = exampleTxt.Split("\n", StringSplitOptions.RemoveEmptyEntries)

type Direction = Up | Down | Left | Right
    with
        member this.TurnRight = 
            match this with
            | Up -> Right
            | Right -> Down
            | Down -> Left
            | Left -> Up

type Game = {
    Map : char array array
    Position : (int * int)
    Obstacles : (int * int) array
    Direction : Direction
}

let parse (input:string []) =
    input
    |> Array.indexed
    |> Array.fold (fun (acc:Game) (i:int,item:string) ->
        let row = item.ToCharArray()
        let obstacles = row |> Array.indexed |> Array.filter (fun (_,c) -> c = '#') |> Array.map (fun (j,_) -> (i,j))
        let position =
            row
            |> Array.tryFindIndex (fun c -> c = '^')
            |> Option.map (fun x -> i,x)
            |> Option.defaultValue acc.Position
        
        { acc with Map = [|row|] |> Array.append acc.Map; Position = position; Obstacles = acc.Obstacles |> Array.append obstacles }
    ) { Map = [||]; Position = (0, 0); Direction = Up; Obstacles = [||] }

type Move = {
    Direction : Direction
    Position : (int * int)
}
with
    member this.MoveForward = 
        match this.Direction with
        | Up -> { this with Position = (fst this.Position - 1, snd this.Position) }
        | Down -> { this with Position = (fst this.Position + 1, snd this.Position) }
        | Left -> { this with Position = (fst this.Position, snd this.Position - 1) }
        | Right -> { this with Position = (fst this.Position, snd this.Position + 1) }
    member this.IsOutOfMap (game:Game) =
        let x,y = this.Position
        x >= 0 && x < game.Map.Length && y >= 0 && y < game.Map.[0].Length
    member this.IsObstacleForward (game:Game) =
        let newPos = this.MoveForward
        let x,y = newPos.Position
        game.Obstacles |> Array.exists (fun (i,j) -> i = x && j = y)

let rec nextMove (g:Game) (acc:Move []) (next:Move) : Move array option =
    if acc |> Array.contains next then None // loop detected
    else
        if next.IsOutOfMap g |> not then (Some acc)
        elif next.IsObstacleForward g then
            let newMove = { next with Direction = next.Direction.TurnRight }
            nextMove g (acc |> Array.append [|next|]) newMove
        else
            nextMove g (acc |> Array.append [| next |]) next.MoveForward

module Part1 =
    let result =
        let game = parse input
        let initMove = { Direction = game.Direction; Position = game.Position }
        nextMove game [| |] initMove
        |> Option.defaultValue [||]
        |> Array.distinctBy (fun m -> m.Position)
        |> Array.length

module Part2 =
    let generateGames (game:Game) =
        [|
            for y in 0 .. game.Map.Length - 1 do
                for x in 0 .. game.Map.[0].Length - 1 do
                    if game.Obstacles |> Array.contains (y,x) |> not then
                        yield { game with Obstacles = game.Obstacles |> Array.append [| (y,x) |] }
        |]
    let getWrongGames (games:Game []) =
        games |> Array.filter (fun g ->
            let initMove = { Direction = g.Direction; Position = g.Position }
            nextMove g [| |] initMove |> Option.isNone
        )
    let result =
        input
        |> parse
        |> generateGames
        |> Array.chunkBySize 1000
        |> Array.Parallel.map (fun games -> games |> getWrongGames |> Array.length)
        |> Array.sum