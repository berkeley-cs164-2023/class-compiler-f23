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

let rec parse_s_exp (toks : token list) : s_exp * token list =
    match toks with 
    | NUM n :: toks2 -> (Num n, toks2)
    | SYM s :: toks2 -> (Sym s, toks2)
    | LPAREN :: toks2 ->
        let exps3, toks3 = parse_lst toks2 in 
        (Lst exps3, toks3)
    | _ -> raise ParseError

and parse_lst (toks : token list) : s_exp list * token list =
    match toks with 
    | RPAREN :: toks2 -> ([], toks2)
    | _ -> 
        let exp2, toks2 = parse_s_exp toks in 
        let exps3, toks3 = parse_lst toks2 in 
        (exp2 :: exps3, toks3)

let parse (s : string) : s_exp =
    let toks = tokenize s in 
    let exp, l = parse_s_exp toks in 
    if List.length l = 0 then exp else raise ParseError


