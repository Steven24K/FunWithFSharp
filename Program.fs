let mutable myInt = 0 // Assignment call
// myInt = 1 // Equality check
myInt <- 1
let res = if myInt = 1 then "Equals to 1" else  "Not equals to 1"


let areEqual expected actual = 
    actual = expected

// Define a new function to print a name.
let printGreeting name =
    printfn $"Hello {name} from F#!"

// Call your new function!
printGreeting "Steven"

