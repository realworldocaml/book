(* This test is intended to exercise the recursive call
   in [SelectiveExpansion.recognize]. *)

%token LBRACKET RBRACKET EOF A
%start<unit> main

%%

wrap(X):
  LBRACKET X RBRACKET
    {}

seq(X):
  /* epsilon */
    {}
| X array(seq, wrap(X)) (* the presence of [wrap] creates a dangerous edge *)
    {}

array(seq, X):
  LBRACKET seq(X) RBRACKET
    {}

main:
  list(array(seq, A)) EOF
    {}

(* The parameter of [seq] has sort [*], so is not expanded during the
   selective expansion pass. The actual parameter [array(seq, A)] in
   [list(array(seq, A))] is recursively recognized as an application
   of [array(seq,_)] to the residual parameter [X]. *)

(*
  array(seq,_)/0 ->(safe) seq(_)/0
  list(_)/0 ->(safe) list(_)/0
  seq(_)/0 ->(dangerous) array(seq,_)/0
 *)
