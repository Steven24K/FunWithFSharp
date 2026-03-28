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

let fred = Eligible "Fred"
let john = Registered "John"
let sarah = Guest "Sarah"

let calculateTotal customer spend: decimal =
    let discount = 
        match customer with 
        | Eligible _ when spend >= 100.0M -> spend * 0.1M
        // | Registered _ -> 0.0M
        // | Guest _ -> 0.0M
        | _ -> 0.0M
    spend - discount

let assertFred = calculateTotal fred 100.0M = 90.0M
let assertJohn = calculateTotal john 99.0M = 99.0M
let assertSarah = calculateTotal sarah 100.0M = 100.0M

printfn $"Fred: {assertFred}"
printfn $"John: {assertJohn}"
printfn $"Sarah: {assertSarah}"
