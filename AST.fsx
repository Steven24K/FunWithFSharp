type Primitive = 
    | INT of int
    | STR of string
    | BOOL of bool

// TODO: Catch this with Either as well
let add (t: Primitive*Primitive) = 
    match t with
    | INT a, INT b -> INT(a + b)
    | STR a, STR b -> STR $"{a}{b}"
    | BOOL a, BOOL b -> BOOL (a && b)
    | a, b -> STR $"Cannot apply + on {a} and {b}" 

let sub (t: Primitive*Primitive) = 
    match t with
    | INT a, INT b -> INT(a - b)
    | STR a, STR b -> STR (a.Replace(b, ""))
    | BOOL a, BOOL b -> BOOL (a || b)
    | a, b -> STR $"Cannot apply - on {a} and {b}" 

let mul (t: Primitive*Primitive) = 
    match t with
    | INT a, INT b -> INT(a * b)
    | a, b -> STR $"Cannot apply * on {a} and {b}" 

let div (t: Primitive*Primitive) = 
    match t with
    | INT a, INT b -> INT(a / b)
    | a, b -> STR $"Cannot apply / on {a} and {b}" 

type Expression =
    | VAL of Primitive
    | SET of string * Primitive
    | VAR of string
    | ADD of Expression * Expression
    | SUB of Expression * Expression
    | MUL of Expression * Expression
    | DIV of Expression * Expression
    | PRINT of string

// TODO: Add expressions to this type to support lazy evaluation
type ValueResult = Primitive
// TODO: Add more errors for better exception handling
type Error = string 

type Stack = Map<string, ValueResult>

type Program = list<Expression>
type Either<'a, 'b> = 
    | Left of 'a
    | Right of 'b

// Product of Stack with Sum of Error and Result = [Stack, |Error|Result|]
type ExecutionStack =  Stack * Either<Error, ValueResult>

let mapFst f (a, b) = (f a, b)
let mapSnd f (a, b) = (a, f b)
let mapFull f1 f2 (a, b) = (f1 a, f2 b)

let inL<'a, 'b>(v: 'a): Either<'a, 'b> = Left v  
let inR<'a, 'b>(v: 'b): Either<'a, 'b> = Right v  

let bindR f result = 
    match result with 
    | Right x -> f x
    | Left x -> inL x 

let bindL f result = 
    match result with 
    | Left x -> f x
    | Right x -> inR x

let printOut (r: ExecutionStack): ExecutionStack = 
    let pretty_string = 
        let stack, result  = r
        match result with 
        | Right output -> $"{output}" 
        | Left e -> $"ERROR: {e} Stack: {stack}"
    pretty_string |> printfn "%A"
    r

let rec exec (stack: Stack)(exp: Expression): ExecutionStack = 
    match exp with 
    | VAL v 
        -> stack, v |> inR
    | SET (k, v) -> 
        stack.Add(k, v), v |> inR
    | VAR k -> 
        stack, if stack.ContainsKey k then stack.[k] |> inR else $"var {k} does not exists" |> inL
    | ADD (e1, e2) -> 
        let s1, res1 = exec stack e1
        let s2, res2 = exec s1 e2
        s2, res1 |> bindR (fun v1 -> res2 |> bindR (fun v2 -> add(v1, v2) |> inR))
    | SUB (e1, e2) -> 
        let s1, res1 = exec stack e1
        let s2, res2 = exec s1 e2
        s2, res1 |> bindR (fun v1 -> res2 |> bindR (fun v2 -> sub(v1, v2) |> inR))
    | MUL (e1, e2) -> 
        let s1, res1 = exec stack e1
        let s2, res2 = exec s1 e2
        s2, res1 |> bindR (fun v1 -> res2 |> bindR (fun v2 -> mul(v1, v2) |> inR))
    | DIV (e1, e2) -> 
        let s1, res1 = exec stack e1
        let s2, res2 = exec s1 e2
        s2, res1 |> bindR (fun v1 -> res2 |> bindR (fun v2 -> div(v1, v2) |> inR))
    | PRINT k -> 
        exec stack (VAR k) |> printOut

let run (program: Program): ExecutionStack =
    program 
    |> List.fold(fun (s, res) exp -> exec s exp )
        (Map.empty, INT 0 |> inR)
        

let program: Program = [
    SET ("msg", STR "Hello World")
    SET ("x", INT 4);
    SET ("y", INT 5);
    // SET ("q", DIV (VAL 8, VAL 2));

    PRINT "msg"
    PRINT "x"
    PRINT "y"
    PRINT "q"

    // something = 8(4 + 6) - (4 / 2) = 78
    // SET("something", SUB(MUL(ADD (VAL 4, VAL 6), VAL 8), DIV (VAL 4, VAL 2)))
    // PRINT "something"
]

run program
