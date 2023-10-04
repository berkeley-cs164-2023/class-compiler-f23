open S_exp
open Util

exception BadExpression of s_exp

type value = Number of int | Boolean of bool | Pair of (value * value)

let rec string_of_value (v: value) : string =
    match v with
    | Number n -> string_of_int n
    | Boolean b -> if b then "true" else "false"
    | Pair (v1, v2) ->
        Printf.sprintf "(pair %s %s)" (string_of_value v1) (string_of_value v2)

let input_channel = ref stdin

let rec interp_exp env (exp: s_exp): value =
    match exp with
    | Num n ->
        Number n
    | Sym "true" ->
        Boolean true
    | Sym "false" ->
        Boolean false
    | Lst [Sym "pair"; e1; e2] ->
        let l = interp_exp env e1 in 
        let r = interp_exp env e2 in 
        Pair (l, r)
    | Lst [Sym "read-num"] ->
        Number (input_line !input_channel |> int_of_string)
    | Lst [Sym "left"; e] -> (
        match interp_exp env e with
        | Pair (v, _) -> v
        | _ -> raise (BadExpression exp)
    )
    | Lst [Sym "right"; e] -> (
        match interp_exp env e with
        | Pair (_, v) -> v
        | _ -> raise (BadExpression exp)
    )
    | Sym var when Symtab.mem var env->
        Symtab.find var env
    | Lst [Sym "let"; Lst [Lst [Sym var; e]]; body] ->
        let e_value = interp_exp env e in 
        interp_exp (Symtab.add var e_value env) body
    | Lst [Sym "add1"; arg] as e -> (
        match interp_exp env arg with 
        | Number n -> Number (n + 1)
        | _ -> raise (BadExpression e)
    )
    | Lst [Sym "sub1"; arg] as e -> (
        match interp_exp env arg with 
        | Number n -> Number (n - 1)
        | _ -> raise (BadExpression e)
    )
    | Lst [Sym "not"; arg] ->
        if interp_exp env arg = Boolean false then Boolean true else Boolean false
    | Lst [Sym "zero?"; arg] ->
        if interp_exp env arg = (Number 0) then Boolean true else Boolean false
    | Lst [Sym "num?"; arg] -> (
        match interp_exp env arg with
        | Number _ -> Boolean true
        | _ -> Boolean false
    )
    | Lst [Sym "+"; e1; e2] -> (
        match (interp_exp env e1, interp_exp env e2) with 
        | Number n1, Number n2 -> Number (n1 + n2)
        | _ -> raise (BadExpression exp)
    )
    | Lst [Sym "-"; e1; e2] -> (
        match (interp_exp env e1, interp_exp env e2) with 
        | Number n1, Number n2 -> Number (n1 - n2)
        | _ -> raise (BadExpression exp)
    )
    | Lst [Sym "="; e1; e2] -> (
        match (interp_exp env e1, interp_exp env e2) with
        | Number n1, Number n2 -> Boolean (n1 = n2)
        | _ -> raise (BadExpression exp)
    )
    | Lst [Sym "<"; e1; e2] -> (
        match (interp_exp env e1, interp_exp env e2) with 
        | Number n1, Number n2 -> Boolean (n1 < n2)
        | _ -> raise (BadExpression exp)
    )
    | Lst [Sym "if"; test_exp; then_exp; else_exp] ->
        if interp_exp env test_exp = Boolean false then interp_exp env else_exp else interp_exp env then_exp
    | e -> raise (BadExpression e)

let interp (program: string) : string =
    string_of_value (interp_exp Symtab.empty (parse program))

let interp_err (program : string) : string =
    try interp program with BadExpression _ -> "ERROR"