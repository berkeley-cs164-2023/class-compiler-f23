{
  open Parse

  exception Error of string
}


rule token = parse
| [' ' '\t' '\n'] (* also ignore newlines, not only whitespace and tabs *)
    { token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '-' ? ['0'-'9']+ as i
    { NUMBER (int_of_string i) }
| ['a'-'z' 'A'-'Z' '+' '-' '*'  '<' '=' '/' '>' '?']+['a'-'z' 'A'-'Z' '+' '-' '*'  '<' '=' '/' '>' '?' '0'-'9']* as s
    { SYMBOL s }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
