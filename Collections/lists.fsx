let items1 = [2;5;3;1;4]

let items2 = [1..5]

let items3 = [
    for x in 1..5 do
        yield x
]

let items4 = [ for x in 1..5 do x]

let extendedItems = 6::items1


let readList items = 
    match items with
    | [] -> "Empty list"
    | [head] -> $"Head: {head}"
    | head::tail -> sprintf "Head %A and Tail: %A" head tail

printf $"{readList items1}"

let getEvens items = items |> List.filter (fun x -> x % 2 = 0)

let sum (items: list<int>) = items |> List.sum

let triple items = items |> List.map (fun x -> x * 3)

let myTriples = triple [1..5]

// Like a forEach
let print items = items |> List.iter(fun x -> printfn $"Value {x}")

let quantity_and_price = [(1,0.25M);(5,0.25M);(1,2.25M);(1,125M);(7,10.9M)]
let sum_list_tuples (items: (int * decimal) list) = items |> List.map(fun (q, p) -> decimal q * p) |> List.sum
// A nice example of map-reduce paradigm.
let total1 = sum_list_tuples quantity_and_price
let total2 items = items |> List.sumBy(fun (q, p) -> decimal q * p)


let total3 = [1..10] |> List.fold (fun acc v ->  acc + v) 0
let total4 = [1..10] |> List.fold (+) 0

// Grouping data
let myList = [1;2;3;4;5;7;6;5;4;3]
let gbResult = myList |> List.groupBy (fun x -> x)
let unique items =
    items
    |> List.groupBy id
    |> List.map (fun (i, _) -> i)

let unResult = unique myList
let distinct = myList |> List.distinct

// List concatenation
let items6 = [1;2;3] @ [4;5;6;7]