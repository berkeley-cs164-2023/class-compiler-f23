open Stdlib

let parse (s : string) =
  let buf = Lexing.from_string s in
  Parse.main Lex.token buf

let parse_file file =
  let inx = open_in file in
  let lexbuf = Lexing.from_channel inx in
  Parse.main Lex.token lexbuf
