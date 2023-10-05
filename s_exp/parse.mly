%{ open Exp %}

%token <int> NUMBER
%token <string> SYMBOL
%token LPAREN RPAREN
%token EOF

%type <Exp.t list> many
%type <Exp.t> main
%start main many

%%

main:
| e = expr EOF
        { e }

many:
| EOF
  { [] }
| e = expr l = many
  { e :: l }

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
