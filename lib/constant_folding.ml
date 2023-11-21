open Ast

let rec fold : expr -> expr = function 
    | Prim1 (Add1, e) -> (
        let e = fold e in 
        match e with Num n -> Num (n+1) | _ -> Prim1 (Add1, e)
    )
    | Prim1 (Sub1, e) -> (
        let e = fold e in 
        match e with Num n -> Num (n-1) | _ -> Prim1 (Sub1, e)
    )
    | Prim1 (p, e) -> 
        Prim1 (p, fold e)
    | Prim2 (Plus, e1, e2) -> (
        let e1 = fold e1 in 
        let e2 = fold e2 in 
        match (e1, e2) with 
        | Num x, Num y -> 
            Num (x + y)
        | _ ->
            Prim2 (Plus, e1, e2)
    )
    | Prim2 (p, e1, e2) ->
        Prim2 (p, fold e1, fold e2)
    | If (e1, e2, e3) -> 
        If (fold e1, fold e2, fold e3)
    | Let (v, e, b) ->
        Let (v, fold e, fold b)
    | e -> e
    

let fold_program (prog: program) = 
    {body = fold prog.body;
    defns = 
        List.map 
            (fun {name; args; body; toplevel; offset} -> {name; args; body = fold body; toplevel; offset})
            prog.defns
    }