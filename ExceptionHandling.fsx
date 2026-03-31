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
let extractMaybe a = 
    match a with 
        | Some a -> a.ToString()
        | None -> "None"

let q1 = tryDivide 4 2 |> extractMaybe
let q2 = tryDivide 4 0 |> extractMaybe
printfn $"{q1}"
printfn $"{q2}"

let isDate = tryParseDateTime "2019-08-01" |> extractMaybe
let isNotDate = tryParseDateTime "Hello" |> extractMaybe

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

