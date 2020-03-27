(* Submitted by Martin Bodin.
   https://gitlab.inria.fr/fpottier/menhir/issues/4
   This file was accepted by Menhir prior to 2017/12/06
   but that was unintended.
   Its close companion bodin.mly was rejected. *)

%token NEW_LINE
%token LPAR RPAR
%token UNIT

%start<unit> main

%%

main:
  | e = expr_or_assign (empty)  { e }

expr_or_assign (el):
  | e = expr (el)           { e }

expr_or_assign_cr:
  | e = expr_or_assign (cr) { e }

expr (el):
  | el; p = LPAR; e = expr_or_assign_cr; cr; RPAR     { e }
  | e = UNIT                                            { e }

cr:
  | NEW_LINE cr { }
  |             { }

empty:
  |             { }

%%
