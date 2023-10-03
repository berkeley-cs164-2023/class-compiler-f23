let gensym : string -> string =
  let counter = ref 0 in
  fun s ->
    let symbol = Printf.sprintf "%s__%d" s !counter in
    counter := !counter + 1 ;
    symbol

module Symtab = Map.Make (struct
  type t = string

  let compare = compare
end)

type 'a symtab = 'a Symtab.t

let rec input_all (ch : in_channel) : string =
  try
    let c = input_char ch in
    String.make 1 c ^ input_all ch
  with End_of_file -> ""