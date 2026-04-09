// TODO: Make support for strings and booleans

type Expression =
    | SET of string * int
    | VAL of int
    | VAR of string
    | ASSIGN of string * Expression
    | ADD of Expression * Expression
    | SUB of Expression * Expression
    | MUL of Expression * Expression
    | DIV of Expression * Expression
    | PRINT of string

type Error = string 
type Return = int

type Program = list<Expression>

type Stack = Map<string, int>

type Either<'a, 'b> = 
    | Left of 'a
    | Right of 'b

type ExecutionResult =  Stack * Either<Error, Return>

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

let printOut (r: ExecutionResult): ExecutionResult = 
    let pretty_string = 
        let stack, result  = r
        match result with 
        | Right output -> $"{output}" 
        | Left e -> $"ERROR: {e} Stack: {stack}"
    pretty_string |> printfn "%A"
    r

let rec exec (stack: Stack)(exp: Expression): ExecutionResult = 
    match exp with 
    | SET (k, v) -> 
        stack.Add(k, v), v |> inR
    | VAL (v: int) -> stack, v |> inR
    | VAR k -> 
        stack, if stack.ContainsKey k then stack.[k] |> inR else $"var {k} does not exists" |> inL
    | ASSIGN (k, e) -> 
        exec stack e // TODO: Store Assignments in different stack for expressions and call by need
    | PRINT k -> 
        exec stack (VAR k) |> printOut
    | ADD (e1, e2) -> 
        let s1, res1 = exec stack e1
        let s2, res2 = exec s1 e2
        s2, res1 |> bindR (fun v1 -> res2 |> bindR (fun v2 ->  v1 + v2 |> inR))
    | SUB (e1, e2) -> 
        let s1, res1 = exec stack e1
        let s2, res2 = exec s1 e2
        s2, res1 |> bindR (fun v1 -> res2 |> bindR (fun v2 ->  v1 - v2 |> inR))
    | MUL (e1, e2) -> 
        let s1, res1 = exec stack e1
        let s2, res2 = exec s1 e2
        s2, res1 |> bindR (fun v1 -> res2 |> bindR (fun v2 ->  v1 * v2 |> inR))
    | DIV (e1, e2) -> 
        let s1, res1 = exec stack e1
        let s2, res2 = exec s1 e2
        s2, res1 |> bindR (fun v1 -> res2 |> bindR (fun v2 ->  v1 / v2 |> inR))

let run (program: Program): ExecutionResult =
    program 
    |> List.fold(fun (s, res) exp -> exec s exp )
        (Map.empty, 0 |> inR)
        

let program: Program = [
    SET ("x", 4);
    SET ("y", 5);
    ASSIGN ("result", ADD (VAR "x", VAR "y"));
    ASSIGN ("q", DIV (VAL 8, VAL 2));
    PRINT "x"
    PRINT "y"
    PRINT "result"
    PRINT "q"

    // something = 8(4 + 6) - (4 / 2) = 78
    ASSIGN("something", SUB(MUL(ADD (VAL 4, VAL 6), VAL 8), DIV (VAL 4, VAL 2)))
    PRINT "something"
]

run program
