type t = Num of int | Sym of string | Lst of t list [@@deriving show]
