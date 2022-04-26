%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%start <int> main

%%

main:
| e = expr EOL
    { e }

(* A factor is atomic. *)

factor:
| i = INT
    { i }
| LPAREN e = expr RPAREN
    { e }

(* A signed factor is a factor, preceded with zero or more MINUS signs. *)

sfactor:
| f = factor
    { f }
| MINUS f = sfactor
    { -f }

(* A term is a nonempty product of signed factors. *)

term:
| f = sfactor
    { f }
| t = term TIMES f = sfactor
    { t * f }
| t = term DIV f = sfactor
    { if f = 0 then Int.max_int else t / f }

(* An expression is a nonempty sum of terms. *)

expr:
| t = term
    { t }
| e = expr PLUS t = term
    { e + t }
| e = expr MINUS t = term
    { e - t }
