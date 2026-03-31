type Sum<'a, 'b> = 
    | Left of 'a
    | Right of 'b

// Option allready exists, let's call it Maybe for those Haskel fan boys. 
type Maybe<'T> = 
    | Some of 'T 
    | None 

let map f maybe =
    match maybe with
    | Some x -> f x |> Some
    | None -> None
let bind f result = 
    match result with 
    | Some x -> f x
    | _ -> None

let _some a = Some a 
let _none = None

open System

let tryParseDateTime (input: string) =
    // let (success, value) = DateTime.TryParse input
    // if success then Some value else None
    match DateTime.TryParse input with 
    | true, result -> Some result
    | _ -> None

let tryDivide x y = 
    let result = 
        match y with 
        | 0 -> None
        | y -> x / y |> Some
    result
// Or use the Result type build in, Success or Failure

// Maybe a start on Fold logic, but lets wait until we move to recursion
let visit onSome onNone a = 
    match a with 
        | Some a -> onSome(a)
        | None -> onNone()

let toString = fun a -> a.ToString()
let Id =  fun () -> "<Empty>"
let q1 = tryDivide 4 2 |> visit toString Id
let q2 = tryDivide 4 0 |> visit toString Id
printfn $"{q1}"
printfn $"{q2}"

let month_progress = "2026-09-03" |> tryParseDateTime |> bind (fun d -> tryDivide d.Month d.Day) |> visit toString Id

printfn $"Month Progress: {month_progress}"

let isDate = tryParseDateTime "2019-08-01" |> visit toString Id
let isNotDate = tryParseDateTime "Hello" |> visit toString Id

printfn $"{isDate}"
printfn $"{isNotDate}"

type Person = {
    FirstName: string 
    MiddleName: Maybe<string> // Or use the build in: string option
    LastName: string
}

let person1 = { FirstName = "Ian"; MiddleName = None; LastName = "Russell"}
let person2 = { person1 with MiddleName = Some "????" }


// Evil null pointers
// Simulating values comming from .NET C#
let nullObj: string = null
let nullPri = Nullable<int>()

let fromNullObj = Option.ofObj nullObj
let fromNullPri = Option.ofNullable nullPri

let toNullObj = Option.toObj fromNullObj
let toNullPri = Option.toNullable fromNullPri



let resultFP = fromNullObj |> Option.defaultValue "------"
let setUnknownAsDefault = Option.defaultValue "????"
let nullSafety = fromNullObj |> setUnknownAsDefault

