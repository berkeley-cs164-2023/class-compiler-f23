
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

  let add_list tab l = List.fold_left (fun tab (k, v) -> add k v tab) tab l

end

type 'a symtab = 'a Symtab.t

let rec input_all (ch : in_channel) : string =
  try
    let c = input_char ch in
    String.make 1 c ^ input_all ch
  with End_of_file -> ""

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