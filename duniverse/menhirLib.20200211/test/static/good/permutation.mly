%token A B EOF
%start<unit> main

%%

(* A list of alternating X's and Y's,
   possibly empty, beginning with an X. *)

(* This grammar was rejected by Menhir prior to 2017/12/06
   because aseq(X, Y) calls itself recursively as aseq(Y, X). *)

aseq(X, Y):
  /* epsilon */  {}
| X aseq(Y, X)   {}

main:
  aseq(A, B) EOF {}
