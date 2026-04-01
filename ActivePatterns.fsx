open System

// Active Pattern: |...|
// Partial Active Pattern: |...|_|
let (|ValidDate|_|) (input: string) =
    match DateTime.TryParse(input) with
    | true, value -> Some value
    | false, _ -> None

let parse input =
    match input with
    | ValidDate dt -> printfn "%A" dt
    | _ -> printfn $"'%s{input}' is not a valid date"

parse "2026-04-01"
parse "Hello"

let (|IsDivisibleBy|_|) divisor n = 
    // if n % divisor = 0 then Some () else None
    if divisor |> List.forall (fun div -> n % div = 0) then Some () else None

let (|NotDivisibleBy|_|) divisor n = 
    // if n % divisor = 0 then Some () else None
    if divisor |> List.forall (fun div -> n % div <> 0) then Some () else None

let isLeapYear year =
    // year % 400 = 0 || (year % 4 = 0 && year % 100 <> 0)
    match year with
    | IsDivisibleBy [400] -> true
    | IsDivisibleBy [4] & NotDivisibleBy [100] -> true
    | _ -> false

[2000;2001;2020] |> List.map isLeapYear = [true;false;true]

let calculate n =
    [(3, "Fizz"); (5, "Buzz"); (7, "Bazz")]
    |> List.map (fun (divisor, result) -> if n % divisor = 0 then result else "")
    |> List.reduce (+)
    |> fun input -> input
    // match (i) with 
    // | IsDivisibleBy [3;5;7] -> "FizzBuzzBazz"
    // | IsDivisibleBy [3;5] -> "FizzBuzz"
    // | IsDivisibleBy [3] -> "Fizz"
    // | IsDivisibleBy [5] -> "Buzz"
    // | IsDivisibleBy [7] -> "Bazz"
    // | _ -> i |> string

[1..15] |> List.map calculate

// Multicase active patterns
type Rank = Ace|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King
type Suit = Hearts|Clubs|Diamonds|Spades
type Card = Rank * Suit

let (|Red|Black|) (card: Card) = 
    match card with
    | (_, Diamonds) | (_, Hearts) -> Red 
    | (_, Clubs) | (_, Spades) -> Black

let describeColour card =
    match card with
    | Red -> "red"
    | Black -> "black"
    |> printfn "The card is %s"

describeColour (Two, Hearts)

// Single case active patterns
open System
let (|CharacterCount|) (input:string) =
    input.Length

let (|ContainsANumber|) (input:string) =
    input
    |> Seq.filter Char.IsDigit
    |> Seq.length > 0

let (|IsValidPassword|) input =
    match input with
    | CharacterCount len when len < 8 -> (false, "Password must be at least 8 characters.")
    | ContainsANumber false -> (false, "Password must contain at least 1 digit.")
    | _ -> (true, "")

let setPassword input =
    match input with
    | IsValidPassword (true, _) as pwd -> Ok pwd
    | IsValidPassword (false, failureReason) -> Error $"Password not set: %s{failureReason}"
    
let badPassword = setPassword "password"
let goodPassword = setPassword "passw0rd"


type Score = int * int

let (|CorrectScore|_|) (expected:Score, actual: Score) =
    if expected = actual then Some() else None

let (|Draw|HomeWin|AwayWin|) (score:Score) =
    match score with
    | (h, a) when h = a -> Draw
    | (h, a) when h > a -> HomeWin
    | _ -> AwayWin

let (|CorrectResult|_|) (expected:Score, actual:Score) =
    match (expected, actual) with
    | (Draw, Draw) -> Some ()
    | (HomeWin, HomeWin) -> Some ()
    | (AwayWin, AwayWin) -> Some ()
    | _ -> None

let goalsScore (expected:Score) (actual:Score) =
    let home = [ fst expected; fst actual ] |> List.min
    let away = [ snd expected; snd actual ] |> List.min
    (home * 15) + (away * 20)

let resultScore (expected:Score) (actual:Score) =
    match (expected, actual) with
    | CorrectScore -> 400
    | CorrectResult -> 100
    | _ -> 0

let calculatePoints (expected:Score) (actual:Score) =
    [ resultScore; goalsScore ]
    |> List.sumBy (fun f -> f expected actual)



let assertnoScoreDrawCorrect = calculatePoints (0, 0) (0, 0) = 400
let assertHomeWinExactMatch = calculatePoints (3, 2) (3, 2) = 485
let assertHomeWin = calculatePoints (5, 1) (4, 3) = 180
let assertIncorrect = calculatePoints (2, 1) (0, 7) = 20
let assertDraw = calculatePoints (2, 2) (3, 3) = 170
