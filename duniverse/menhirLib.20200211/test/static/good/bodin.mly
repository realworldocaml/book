(* Submitted by Martin Bodin.
   https://gitlab.inria.fr/fpottier/menhir/issues/4
   This file was rejected by Menhir prior to 2017/12/06
   because expr_or_assign(el) invokes expr(el)
   which invokes expr_or_assign(cr). *)

%token NEW_LINE
%token LPAR RPAR
%token UNIT

%start<unit> main

%%

main:
  | e = expr_or_assign (empty)  { e }

expr_or_assign (el):
  | e = expr (el)           { e }

expr (el):
  | el; p = LPAR; e = expr_or_assign (cr); cr; RPAR     { e }
  | e = UNIT                                            { e }

cr:
  | NEW_LINE cr { }
  |             { }

empty:
  |             { }

%%
