open Stdlib

let parse (s : string) =
  let buf = Lexing.from_string s in
  Parse.main Lex.token buf

let parse_file file =
  let inx = open_in file in
  let lexbuf = Lexing.from_channel inx in
  Parse.main Lex.token lexbuf

let parse_many (s : string) =
  let buf = Lexing.from_string s in
  Parse.many Lex.token buf

let parse_many_file file =
  let inx = open_in file in
  let lexbuf = Lexing.from_channel inx in
  Parse.many Lex.token lexbuf
