%{ open Exp %}

%token <int> NUMBER
%token <string> SYMBOL
%token LPAREN RPAREN
%token EOF

%start <Exp.t> main

%%

main:
| e = expr 
        { e }

expr:
| n = NUMBER
  { Num n }
| s = SYMBOL
  { Sym s }
| LPAREN l=lst RPAREN
  { Lst l }

lst:
|   { [] }
| e = expr l = lst
    { e ::l }
