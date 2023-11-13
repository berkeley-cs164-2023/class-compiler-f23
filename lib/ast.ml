open S_exp
open Util

type prim0 = ReadNum | Newline

let prim0_of_string = function
  | "read-num" ->
      Some ReadNum
  | "newline" ->
      Some Newline
  | _ ->
      None

type prim1 = Add1 | Sub1 | ZeroP | NumP | Not | Left | Right | Print

let prim1_of_string = function
  | "add1" ->
      Some Add1
  | "sub1" ->
      Some Sub1
  | "zero?" ->
      Some ZeroP
  | "num?" ->
      Some NumP
  | "not" ->
      Some Not
  | "left" ->
      Some Left
  | "right" ->
      Some Right
  | "print" ->
      Some Print
  | _ ->
      None

type prim2 = Plus | Minus | Eq | Lt | Pair

let prim2_of_string = function
  | "+" ->
      Some Plus
  | "-" ->
      Some Minus
  | "=" ->
      Some Eq
  | "<" ->
      Some Lt
  | "pair" ->
      Some Pair
  | _ ->
      None

type expr =
  | Prim0 of prim0
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Do of expr list
  | Num of int
  | Var of string
  | Call of expr * expr list
  | True
  | False
  | Closure of string

type expr_lam =
  | Prim0 of prim0
  | Prim1 of prim1 * expr_lam
  | Prim2 of prim2 * expr_lam * expr_lam
  | Let of string * expr_lam * expr_lam
  | If of expr_lam * expr_lam * expr_lam
  | Do of expr_lam list
  | Num of int
  | Var of string
  | Call of expr_lam * expr_lam list
  | True
  | False
  | Lambda of string list * expr_lam

type defn = {name: string; args: string list; body: expr}

type program = {defns: defn list; body: expr}

let is_defn defns name = List.exists (fun d -> d.name = name) defns

let get_defn defns name = List.find (fun d -> d.name = name) defns

let is_sym e = match e with Sym _ -> true | _ -> false

let as_sym e = match e with Sym s -> s | _ -> raise Not_found

let rec expr_lam_of_s_exp : s_exp -> expr_lam = function
  | Num x ->
      Num x
  | Sym "true" ->
      True
  | Sym "false" ->
      False
  | Sym var ->
      Var var
  | Lst [Sym "let"; Lst [Lst [Sym var; exp]]; body] ->
      Let (var, expr_lam_of_s_exp exp, expr_lam_of_s_exp body)
  | Lst (Sym "do" :: exps) when List.length exps > 0 ->
      Do (List.map expr_lam_of_s_exp exps)
  | Lst [Sym "if"; test_s; then_s; else_s] ->
      If
        ( expr_lam_of_s_exp test_s
        , expr_lam_of_s_exp then_s
        , expr_lam_of_s_exp else_s )
  | Lst [Sym "lambda"; Lst args; body] when List.for_all is_sym args ->
      Lambda (List.map as_sym args, expr_lam_of_s_exp body)
  | Lst [Sym prim] when Option.is_some (prim0_of_string prim) ->
      Prim0 (Option.get (prim0_of_string prim))
  | Lst [Sym prim; arg] when Option.is_some (prim1_of_string prim) ->
      Prim1 (Option.get (prim1_of_string prim), expr_lam_of_s_exp arg)
  | Lst [Sym prim; arg1; arg2] when Option.is_some (prim2_of_string prim) ->
      Prim2
        ( Option.get (prim2_of_string prim)
        , expr_lam_of_s_exp arg1
        , expr_lam_of_s_exp arg2 )
  | Lst (f :: args) ->
      Call (expr_lam_of_s_exp f, List.map expr_lam_of_s_exp args)
  | e ->
      raise (BadSExpression e)

let rec expr_of_expr_lam (defns : defn list ref) : expr_lam -> expr = function
  | Num x ->
      Num x
  | Var s ->
      Var s
  | True ->
      True
  | False ->
      False
  | If (test_exp, then_exp, else_exp) ->
      If
        ( expr_of_expr_lam defns test_exp
        , expr_of_expr_lam defns then_exp
        , expr_of_expr_lam defns else_exp )
  | Let (var, exp, body) ->
      Let (var, expr_of_expr_lam defns exp, expr_of_expr_lam defns body)
  | Prim0 p ->
      Prim0 p
  | Prim1 (p, e) ->
      Prim1 (p, expr_of_expr_lam defns e)
  | Prim2 (p, e1, e2) ->
      Prim2 (p, expr_of_expr_lam defns e1, expr_of_expr_lam defns e2)
  | Do exps ->
      Do (List.map (expr_of_expr_lam defns) exps)
  | Call (exp, args) ->
      Call (expr_of_expr_lam defns exp, List.map (expr_of_expr_lam defns) args)
  | Lambda (args, body) ->
    let name = gensym "_lambda" in 
    defns := {name; args; body=expr_of_expr_lam defns body} :: !defns;
    Closure name

let program_of_s_exps (exps : s_exp list) : program =
  let defns = ref [] in
  let rec get_args args =
    match args with
    | Sym v :: args ->
        v :: get_args args
    | e :: _ ->
        raise (BadSExpression e)
    | [] ->
        []
  in
  let get_defn = function
    | Lst [Sym "define"; Lst (Sym name :: args); body] ->
        let args = get_args args in
        {name; args; body= body |> expr_lam_of_s_exp |> expr_of_expr_lam defns}
    | e ->
        raise (BadSExpression e)
  in
  let rec go exps =
    match exps with
    | [e] ->
        let body = e |> expr_lam_of_s_exp |> expr_of_expr_lam defns in
        {defns= List.rev !defns; body}
    | d :: exps ->
        let defn = get_defn d in
        defns := defn :: !defns ;
        go exps
    | _ ->
        raise (BadSExpression (Sym "empty"))
  in
  go exps

exception BadExpression of expr