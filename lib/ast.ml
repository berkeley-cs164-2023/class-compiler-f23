open S_exp

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
  | Call of string * expr list
  | True
  | False

type defn = {name: string; args: string list; body: expr}

type program = {defns: defn list; body: expr}

let is_defn defns name = List.exists (fun d -> d.name = name) defns

let get_defn defns name = List.find (fun d -> d.name = name) defns

let rec expr_of_s_exp : s_exp -> expr = function
  | Num x ->
      Num x
  | Sym "true" ->
      True
  | Sym "false" ->
      False
  | Sym var ->
      Var var
  | Lst [Sym "let"; Lst [Lst [Sym var; exp]]; body] ->
      Let (var, expr_of_s_exp exp, expr_of_s_exp body)
  | Lst (Sym "do" :: exps) when List.length exps > 0 ->
      Do (List.map expr_of_s_exp exps)
  | Lst [Sym "if"; test_s; then_s; else_s] ->
      If (expr_of_s_exp test_s, expr_of_s_exp then_s, expr_of_s_exp else_s)
  | Lst [Sym prim] when Option.is_some (prim0_of_string prim) ->
      Prim0 (Option.get (prim0_of_string prim))
  | Lst [Sym prim; arg] when Option.is_some (prim1_of_string prim) ->
      Prim1 (Option.get (prim1_of_string prim), expr_of_s_exp arg)
  | Lst [Sym prim; arg1; arg2] when Option.is_some (prim2_of_string prim) ->
      Prim2
        ( Option.get (prim2_of_string prim)
        , expr_of_s_exp arg1
        , expr_of_s_exp arg2 )
  | Lst (Sym f :: args) ->
      Call (f, List.map expr_of_s_exp args)
  | e ->
      raise (BadSExpression e)

let program_of_s_exps (exps : s_exp list) : program =
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
        {name; args; body= expr_of_s_exp body}
    | e ->
        raise (BadSExpression e)
  in
  let rec go exps defns =
    match exps with
    | [e] ->
        {defns= List.rev defns; body= expr_of_s_exp e}
    | d :: exps ->
        go exps (get_defn d :: defns)
    | _ ->
        raise (BadSExpression (Sym "empty"))
  in
  go exps []

exception BadExpression of expr