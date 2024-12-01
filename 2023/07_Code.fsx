#load "InputReader.fsx"

let input = InputReader.read "07"

type Card = string * (char * int) list
let values = ['A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2']
let jokerValues = ['A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J']

let toCard (s:string) : Card =
    s
    |> Seq.toList
    |> List.groupBy id
    |> List.map (fun (c,x) -> c, (x.Length))
    |> (fun x -> s, x)

let parseCardAndBid (c:string) =
    let parts = c.Split(" ")
    (toCard parts.[0]), (int parts.[1])

let cards = input |> Array.map parseCardAndBid |> Array.toList

let score (c:Card) =
    c
    |> snd
    |> List.sortByDescending snd
    |> List.map snd
    |> List.filter (fun x -> x >= 2)

let compare values (c1:Card) (c2:Card) =
    let lc1 = c1 |> score
    let lc2 = c2 |> score
    if lc1 > lc2 then -1
    elif lc1 < lc2 then 1
    else
        [0..4]
        |> List.fold (fun (acc:int) item -> 
            if acc = 0 then
                let i1 = (fst c1).[item] |> (fun x -> values |> List.findIndex (fun y -> y = x))
                let i2 = (fst c2).[item] |> (fun x -> values |> List.findIndex (fun y -> y = x))
                if i1 < i2 then -1 elif i1 > i2 then 1 else 0
            else acc
        ) 0

let part1 = 
    cards
    |> List.sortWith (fun x y -> compare values (fst x) (fst y))
    |> List.rev
    |> List.map snd
    |> List.mapi (fun i x -> x * (i + 1))
    |> List.sum

let applyJoker (card:Card) =
    let name,nums = card
    let jokers,others = nums |> List.partition (fun (x,_) -> x = 'J')
    if jokers.Length = 0 || others.Length = 0 then card
    else
        let maxChar,maxNum = others |> List.maxBy snd
        let toAppend = others |> List.filter (fun (x,_) -> x <> maxChar)
        name, (toAppend @ [maxChar, maxNum + snd jokers.[0]])
let part2 =
    cards
    |> List.map (fun (x,y) -> applyJoker x, y)
    |> List.sortWith (fun x y -> compare jokerValues (fst x) (fst y))
    |> List.rev
    |> List.map snd
    |> List.mapi (fun i x -> x * (i + 1))
    |> List.sum