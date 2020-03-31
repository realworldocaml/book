(* These are the functions that we need in order to write our semantic
   actions. *)

%parameter<Semantics : sig
  type number
  val inject: int -> number
  val ( + ): number -> number -> number
  val ( - ): number -> number -> number
  val ( * ): number -> number -> number
  val ( / ): number -> number -> number
  val ( ~-): number -> number
end>

(* The parser no longer returns an integer; instead, it returns an
   abstract number. *)

%start <Semantics.number> main

(* Let us open the [Semantics] module, so as to make all of its
   operations available in the semantic actions. *)

%{

  open Semantics

%}

%%

main:
| e = expr EOL
    { e }

expr:
| i = INT
    { inject i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = expr
    { e1 + e2 }
| e1 = expr MINUS e2 = expr
    { e1 - e2 }
| e1 = expr TIMES e2 = expr
    { e1 * e2 }
| e1 = expr DIV e2 = expr
    { e1 / e2 }
| MINUS e = expr %prec UMINUS
    { - e }

