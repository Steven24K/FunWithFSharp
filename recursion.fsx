// We are going to start with a naive implementation of the factorial function (!):
// 5! = 5 * 4 * 3 * 2 * 1 = 120

let rec fact n = 
    match n with
    | 1 -> 1 // <- base case
    | n -> n * fact(n-1) 


// With tail call optimisation (first step towards Fold)
let fact_tail n =
    let rec loop n acc = 
        match n with 
        | 1 -> acc
        | _ -> loop (n-1) (acc * n)
    loop n 1


let rec fold (f: 'b -> 'a -> 'b) (acc: 'b) (items: list<'a>): 'b = 
    match items with 
    | [] ->  acc
    | head::tail -> tail |> fold f (f acc head)

let sum = fun xs x -> xs + x
let print = fun (xs: string) (x: 'a) -> xs + x.ToString() + ", "

[1; 2; 3] |> fold sum 0 |> printfn "%A"
[1; 2; 3] |> fold print "" |> printfn "%A"


// Without tail call optimization
let rec fib (n: int64) = 
    match n with 
    | 0L -> 0L
    | 1L -> 1L
    | s -> fib (s-1L) + fib(s-2L)

// Better version optimized
let fib_tail (n:int64) =
    let rec loop n (a,b) =
        match n with
        | 0L -> a
        | 1L -> b
        | n -> loop (n-1L) (b, a+b)
    loop n (0L,1L)

let rec quick_sort input =
    match input with
    | [] -> []
    | head::tail ->
        let smaller, larger = List.partition (fun n -> head >= n) tail
        List.concat [quick_sort smaller; [head]; quick_sort larger]

[5;9;5;2;7;9;1;1;3;5] |> quick_sort |> printfn "%A"
