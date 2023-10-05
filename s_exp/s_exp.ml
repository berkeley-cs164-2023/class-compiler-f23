type s_exp = Exp.t = Num of int | Sym of string | Lst of s_exp list

let show = Exp.show

let parse = Parser.parse

let parse_many = Parser.parse_many
