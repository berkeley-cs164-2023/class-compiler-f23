type s_exp = Num of int | Sym of string | Lst of s_exp list

type token = NUM of int | SYM of string | LPAREN | RPAREN

exception ParseError

let token_of_string (s : string) =
    match s with
    | "(" ->
        LPAREN
    | ")" ->
        RPAREN
    | _ -> (
        try NUM (int_of_string s) with _ -> SYM s )

let tokenize (s : string) =
    s |> String.split_on_char ' ' |> List.map token_of_string

