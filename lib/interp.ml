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
let output_channel = ref stdout

let rec interp_exp (defns : defn list) env (exp: s_exp): value =
    match exp with
    | Num n ->
        Number n
    | Sym "true" ->
        Boolean true
    | Sym "false" ->
        Boolean false
    | Lst (Sym f :: args) when is_defn defns f ->
        let defn = get_defn defns f in 
        if List.length args = List.length defn.args then 
            let vals = List.map (interp_exp defns env) args in 
            let fenv = (List.combine defn.args vals) |> Symtab.of_list in
            interp_exp defns fenv defn.body
        else raise (BadExpression exp)
    | Lst [Sym "pair"; e1; e2] ->
        let l = interp_exp defns env e1 in 
        let r = interp_exp defns env e2 in 
        Pair (l, r)
    | Lst (Sym "do" :: exps) when List.length exps > 0 ->
        exps |> List.rev_map (interp_exp defns env) |> List.hd
    | Lst [Sym "print"; e] ->
        interp_exp defns env e |> string_of_value |> output_string !output_channel ;
        Boolean true
    | Lst [Sym "newline"] ->
        output_string !output_channel "\n";
        Boolean true
    | Lst [Sym "read-num"] ->
        Number (input_line !input_channel |> int_of_string)
    | Lst [Sym "left"; e] -> (
        match interp_exp defns env e with
        | Pair (v, _) -> v
        | _ -> raise (BadExpression exp)
    )
    | Lst [Sym "right"; e] -> (
        match interp_exp defns env e with
        | Pair (_, v) -> v
        | _ -> raise (BadExpression exp)
    )
    | Sym var when Symtab.mem var env->
        Symtab.find var env
    | Lst [Sym "let"; Lst [Lst [Sym var; e]]; body] ->
        let e_value = interp_exp defns env e in 
        interp_exp defns (Symtab.add var e_value env) body
    | Lst [Sym "add1"; arg] as e -> (
        match interp_exp defns env arg with 
        | Number n -> Number (n + 1)
        | _ -> raise (BadExpression e)
    )
    | Lst [Sym "sub1"; arg] as e -> (
        match interp_exp defns env arg with 
        | Number n -> Number (n - 1)
        | _ -> raise (BadExpression e)
    )
    | Lst [Sym "not"; arg] ->
        if interp_exp defns env arg = Boolean false then Boolean true else Boolean false
    | Lst [Sym "zero?"; arg] ->
        if interp_exp defns env arg = (Number 0) then Boolean true else Boolean false
    | Lst [Sym "num?"; arg] -> (
        match interp_exp defns env arg with
        | Number _ -> Boolean true
        | _ -> Boolean false
    )
    | Lst [Sym "+"; e1; e2] -> (
        match (interp_exp defns env e1, interp_exp defns env e2) with 
        | Number n1, Number n2 -> Number (n1 + n2)
        | _ -> raise (BadExpression exp)
    )
    | Lst [Sym "-"; e1; e2] -> (
        match (interp_exp defns env e1, interp_exp defns env e2) with 
        | Number n1, Number n2 -> Number (n1 - n2)
        | _ -> raise (BadExpression exp)
    )
    | Lst [Sym "="; e1; e2] -> (
        match (interp_exp defns env e1, interp_exp defns env e2) with
        | Number n1, Number n2 -> Boolean (n1 = n2)
        | _ -> raise (BadExpression exp)
    )
    | Lst [Sym "<"; e1; e2] -> (
        match (interp_exp defns env e1, interp_exp defns env e2) with 
        | Number n1, Number n2 -> Boolean (n1 < n2)
        | _ -> raise (BadExpression exp)
    )
    | Lst [Sym "if"; test_exp; then_exp; else_exp] ->
        if interp_exp defns env test_exp = Boolean false then interp_exp defns env else_exp else interp_exp defns env then_exp
    | e -> raise (BadExpression e)

let interp (program: string) : unit =
    let defns, body = parse_many program |> defns_and_body in
     interp_exp defns Symtab.empty body |> ignore

let interp_io (program : string) (input : string) =
    let input_pipe_ex, input_pipe_en = Unix.pipe () in
    let output_pipe_ex, output_pipe_en = Unix.pipe () in
    input_channel := Unix.in_channel_of_descr input_pipe_ex ;
    set_binary_mode_in !input_channel false ;
    output_channel := Unix.out_channel_of_descr output_pipe_en ;
    set_binary_mode_out !output_channel false ;
    let write_input_channel = Unix.out_channel_of_descr input_pipe_en in
    set_binary_mode_out write_input_channel false ;
    let read_output_channel = Unix.in_channel_of_descr output_pipe_ex in
    set_binary_mode_in read_output_channel false ;
    output_string write_input_channel input ;
    close_out write_input_channel ;
    interp program ;
    close_out !output_channel ;
    let r = input_all read_output_channel in
    input_channel := stdin ;
    output_channel := stdout ;
    r

let interp_err (program : string) (input : string) : string =
    try interp_io program input with BadExpression _ -> "ERROR"