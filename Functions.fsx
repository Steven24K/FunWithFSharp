//Rule: One input -> one output

// Function composition:
// f1: 'a -> 'b
// f2: 'b -> 'c

// f3: f1 >> f2 // 'a -> 'c

type Customer = {
    Id: int 
    IsVip: bool
    Credit: decimal
}

let getPurchases customer = 
    let purchases = if customer.Id % 2 = 0 then 120M else 80M
    customer, purchases

let tryPromoteToVip purchases =
    let customer, amount = purchases
    if amount > 100M then { customer with IsVip = true } 
    else customer

let increaseCreditIfVip customer = 
    let increase = if customer.IsVip then 100M else 50M 
    { customer with Credit = customer.Credit + increase }


// Compositions, functional style
let upgradeCustomerComposed = 
    getPurchases >> tryPromoteToVip >> increaseCreditIfVip

let upgradeCustomerNested customer =
    increaseCreditIfVip(tryPromoteToVip(getPurchases customer))


// Procedural (don't do this in F#)
let upgradeCustomerProcedural customer = 
    let customerWithPurchases = getPurchases customer
    let promotedCustomer = tryPromoteToVip customerWithPurchases
    let increasedCreditCustomer = increaseCreditIfVip promotedCustomer
    increasedCreditCustomer


// Even more elegant
let upgradeCustomerPiped customer =
    customer 
    |> getPurchases
    |> tryPromoteToVip
    |> increaseCreditIfVip


let customerVIP = { Id = 1; IsVip = true; Credit = 0.0M }
let customerSTD = { Id = 2; IsVip = false; Credit = 100.0M }
let assertVIP =
    upgradeCustomerPiped customerVIP = { Id = 1; IsVip = true; Credit = 100.0M }
let assertSTDtoVIP =
    upgradeCustomerPiped customerSTD = { Id = 2; IsVip = true; Credit = 200.0M }
let assertSTD =
    upgradeCustomerPiped { customerSTD with Id = 3; Credit = 50.0M } = { Id = 3; IsVip = false; Credit = 100.0M }


printfn $"assetVIP: {assertVIP}"
printfn $"assetSTDtoVIP: {assertSTDtoVIP}"
printfn $"assetSTD: {assertSTD}"

// Unit

open System

// Oh no a side effect!
let now () = DateTime.UtcNow

let log msg = ()

let fixedNow = DateTime.UtcNow

// The binding will not change
let theTimeIs = fixedNow


// Some lambda's 

let add = fun x y -> x + y

let apply f x y = f x y

let sum_five = apply(fun x y -> x + y)

let rnd () = 
    let rand = Random()
    rand.Next(100)

// Create a list of 50 random integers
List.init 50 (fun _ -> rnd())


let calculateTotal customer = 
    fun spend -> 
        let discount = 
            if customer.IsVip && spend >= 100.0M then spend * 0.1M 
            else 0.0M
        spend - discount


let john: Customer = {Id = 1; Credit = 100.0M; IsVip = true}

let partial = calculateTotal john
let complete = 100.0M |> partial // Using forward pipe operator

let areEqual expected actual = 
    expected = actual

let isEqualTo expected actual = 
    expected = actual

let assertJohn1 = areEqual 90.0M (calculateTotal john 100.0M)

let assertJohn2 = calculateTotal john 100.0M |> isEqualTo 90.0M



// Partial application - logging

type LogLevel = 
    | Error
    | Warning 
    | Info

let _log (level: LogLevel) (message: string) = 
    printfn $"[{level}]: {message}"

let _logError = _log Error
let _logWarning = _log Warning
let _logInfo = _log Info


_logError "Some error message"
_logWarning "Warnings are ment to be ingored"
_logInfo "Just for your information"
