// Feature: Applying a discount
// Scenario: Eligible Registered Customers get 10% discount when they spend £100 or more

// Tuples/pairs are represented as a product type
type MyTuple = string * bool * bool
let customer: MyTuple = "Fred", true, true


// type RegisteredCustomer = {
//     Id: string
// }

// type UnregisteredCustomer = {
//     Id: string
// }

type Customer =
    | Eligible of Id:string
    | Registered of Id: string
    | Guest of Id: string
    with
        member this.Discount =
            match this with
            | Eligible _ -> 0.1M
            | _ -> 0.0M


type ValidationError =
    | InputOutOfRange of string

type Spend = private Spend of decimal
    with
        member this.Value = this |> fun (Spend value) -> value
        static member Create input =
            if input >= 0.0M && input <= 1000.0M then
                Ok (Spend input)
            else
                Error (InputOutOfRange "You can only spend between 0 and 1000")

type Total = decimal
type CalculateTotal = Customer -> Spend -> Total


let fred = Eligible "Fred"
let john = Registered "John"
let sarah = Guest "Sarah"

let calculateTotal (customer:Customer) (spend:Spend) =
    let discount =
        if spend.Value >= 100.0M then spend.Value * customer.Discount
        else 0.0M
    spend.Value - discount




let isEqualTo expected actual = 
    actual =expected

let assertFred1 = calculateTotal fred (Spend 100.0M) |> isEqualTo 90.0M
let assertJohn1 = calculateTotal john (Spend 99.0M) |> isEqualTo 99.0M
let assertSarah1 = calculateTotal sarah (Spend 100.0M) |> isEqualTo 100.0M

printfn $"Fred: {assertFred1}"
printfn $"John: {assertJohn1}"
printfn $"Sarah: {assertSarah1}"

let assertEqual customer spent expected =
    Spend.Create spent
    |> Result.map (fun spend -> calculateTotal customer spend)
    |> isEqualTo (Ok expected)

let assertJohn2 = assertEqual john 100.0M 90.0M
let assertSarah2 = assertEqual sarah 100.0M 100.0M

type Latitude = decimal
type Longitude = decimal

type GpsCoordinate = { Latitude: Latitude; Longitude: Longitude }

let badGps : GpsCoordinate = { Latitude = 1000M; Longitude = -345M }
let latitude = 46M
let longitude = 15M
let badGps2 : GpsCoordinate = { Latitude = longitude; Longitude = latitude }
