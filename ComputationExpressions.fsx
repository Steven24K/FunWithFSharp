namespace ComputationExpression

module OptionDemo = 

    let multiply x y =
        x * y 

    let divide x y = 
        if y = 0 then None 
        else Some (x / y)

    // f(x,y) = (( x / y ) * x) / y
    let calculate x y = 
        divide x y 
        |> Option.map (fun v -> multiply v x)
        |> Option.bind (fun t -> divide t y)
        // |> Option.map (fun t -> divide t y) |> Option.flatten

        // Too much code below
        // |> fun v -> 
        //     match v with
        //     | Some v -> multiply v x |> Some
        //     | None -> None
        // |> fun t -> 
        //     match t with
        //     | Some t -> divide t y
        //     | None -> None
