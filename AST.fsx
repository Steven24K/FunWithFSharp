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

type Program = list<Expression>

type Stack = Map<string, int>

type Either<'a, 'b> = 
    | Left of 'a
    | Right of 'b

// TODO: Also store Stack together with error
// or Type ExecutionResult = Stack * Either<Error, int>
type ExecutionResult = Either<Error, Stack * int>

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
        match r with 
        | Right (_, output) -> $"{output}" 
        | Left e -> $"ERROR: {e}"
    pretty_string |> printfn "%A"
    r

let rec exec (stack: Stack)(exp: Expression): ExecutionResult = 
    match exp with 
    | SET (k, v) -> 
        (stack.Add(k, v), v) |> inR
    | VAL (v: int) -> (stack, v) |> inR
    | VAR k -> 
        if stack.ContainsKey k then (stack, stack.[k]) |> inR else $"var {k} does not exists" |> inL
    | ASSIGN (k, e) -> 
        exec stack e |> bindR (fun (s1, v) -> (s1.Add(k, v), v) |> inR)
    | PRINT k -> 
        exec stack (VAR k) |> printOut |> bindR (fun (s1, v) -> (s1, v) |> inR)
    | ADD (e1, e2) -> 
        exec stack e1 |> bindR (fun (s1, v1) -> exec s1 e2 |> bindR (fun (s2, v2) ->  (s2, v1 + v2) |> inR))
    | SUB (e1, e2) -> 
        exec stack e1 |> bindR (fun (s1, v1) -> exec s1 e2 |> bindR (fun (s2, v2) ->  (s2, v1 - v2) |> inR))
    | MUL (e1, e2) -> 
        exec stack e1 |> bindR (fun (s1, v1) -> exec s1 e2 |> bindR (fun (s2, v2) ->  (s2, v1 * v2) |> inR))
    | DIV (e1, e2) -> 
        exec stack e1 |> bindR (fun (s1, v1) -> exec s1 e2 |> bindR (fun (s2, v2) ->  if v2 = 0 then $"Cannot divide {v1} by 0" |> inL |> printOut else (s2, v1 / v2) |> inR))

let run (program: Program): ExecutionResult =
    program 
    |> List.fold (fun (s1: ExecutionResult) exp -> 
        s1 |> bindR (fun (s2, _) -> exec s2 exp)) 
        ((Map.empty, 0) |> inR)
        

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
