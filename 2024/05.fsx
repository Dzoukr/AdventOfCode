#load "../InputReader.fsx"

let input = InputReader.read "2024" "05"

open System

let exampleTxt = """
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"""

let example = exampleTxt.Split("\n", StringSplitOptions.RemoveEmptyEntries)

type Printer = {
    Ordering : (int * int) array
    Updates : int array array
}

let parse (input:string []) =
    input |> Array.fold (fun (acc:Printer) (item:string) ->
        if item.Contains("|") then
            let parts = item.Split("|")
            let x = int parts.[0]
            let y = int parts.[1]
            { acc with Ordering = [|(x,y)|] |> Array.append acc.Ordering }
        elif item.Contains(",") then
            let parts = item.Split(",")
            let values = parts |> Array.map int
            { acc with Updates = [|values|] |> Array.append acc.Updates }
        else acc
    ) { Ordering = [||]; Updates = [||] }

let isOrderingValid (ordering:(int * int) []) (pages:int array) =
    let isValid (ind:int) (value:int) =
        let left = pages.[..ind - 1]
        let right = pages.[ind + 1..]
        let r,l = ordering |> Array.filter (fun (x,y) -> x = value || y = value)|> Array.partition (fun (x,_) -> x = value)
        let rr = r |> Array.map snd
        let ll = l |> Array.map fst
        left |> Array.exists (fun x -> rr |> Array.contains x) |> not
        &&
        right |> Array.exists (fun x -> ll |> Array.contains x) |> not
    pages
    |> Array.mapi isValid
    |> Array.forall id

let middleOfArray (arr:int array) = arr.[arr.Length / 2]

module Part1 = 
    let printer = parse input
    let result =
        printer.Updates
        |> Array.filter (isOrderingValid printer.Ordering)
        |> Array.map middleOfArray
        |> Array.sum
    
module Part2 =
    let printer = parse input
    let customSort (ordering:(int * int) []) (pages:int array) =
        let sort x y =
            let ox = ordering |> Array.filter (fun (a,b) -> (a = x && b = y) || (a = y && b = x))
            if ox |> Array.contains (x,y) then -1
            elif ox |> Array.contains (y,x) then 1
            else 0
        pages |> Array.sortWith sort
    let result =
        printer.Updates
        |> Array.filter (isOrderingValid printer.Ordering >> not)
        |> Array.map (customSort printer.Ordering >> middleOfArray)
        |> Array.sum