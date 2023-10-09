open S_exp

let gensym : string -> string =
  let counter = ref 0 in
  fun s ->
    let symbol = Printf.sprintf "%s__%d" s !counter in
    counter := !counter + 1 ;
    symbol

module ST = Map.Make (struct
  type t = string

  let compare = compare
end)

module Symtab = struct
  include ST

  let of_list l = l |> List.to_seq |> of_seq
end

type 'a symtab = 'a Symtab.t

let rec input_all (ch : in_channel) : string =
  try
    let c = input_char ch in
    String.make 1 c ^ input_all ch
  with End_of_file -> ""

exception BadExpression of s_exp

type defn = {name: string; args: string list; body: s_exp}

let sym = function Sym s -> s | e -> raise (BadExpression e)

let defns_and_body (exps : s_exp list) : defn list * s_exp =
  let get_defn = function
    | Lst [Sym "define"; Lst (Sym name :: args); body] ->
        {name; args= List.map sym args; body}
    | e ->
        raise (BadExpression e)
  in
  let rec go exps defns =
    match exps with
    | [e] ->
        (List.rev defns, e)
    | d :: exps ->
        go exps (get_defn d :: defns)
    | _ ->
        raise (BadExpression (Sym "empty"))
  in
  go exps []

let is_defn defns name = List.exists (fun d -> d.name = name) defns

let get_defn defns name = List.find (fun d -> d.name = name) defns

let defn_label s =
  let nasm_char c =
    match c with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_'
    | '$'
    | '#'
    | '@'
    | '~'
    | '.'
    | '?' ->
        c
    | _ ->
        '_'
  in
  Printf.sprintf "function_%s_%d" (String.map nasm_char s) (Hashtbl.hash s)