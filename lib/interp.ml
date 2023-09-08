open S_exp

exception BadExpression of s_exp

type value = Number of int | Boolean of bool

let string_of_value (v: value) : string =
    match v with
    | Number n -> string_of_int n
    | Boolean b -> if b then "true" else "false"

let rec interp_exp (program: s_exp): value =
    match program with
    | Num n ->
        Number n
    | Sym "true" ->
        Boolean true
    | Sym "false" ->
        Boolean false
    | Lst [Sym "add1"; arg] as e -> (
        match interp_exp arg with 
        | Number n -> Number (n + 1)
        | _ -> raise (BadExpression e)
    )
    | Lst [Sym "sub1"; arg] as e -> (
        match interp_exp arg with 
        | Number n -> Number (n - 1)
        | _ -> raise (BadExpression e)
    )
    | Lst [Sym "not"; arg] ->
        if interp_exp arg = Boolean false then Boolean true else Boolean false
    | Lst [Sym "zero?"; arg] ->
        if interp_exp arg = (Number 0) then Boolean true else Boolean false
    | Lst [Sym "num?"; arg] -> (
        match interp_exp arg with
        | Number _ -> Boolean true
        | _ -> Boolean false
    )
    | e -> raise (BadExpression e)

let interp (program: string) : string =
    string_of_value (interp_exp (parse program))