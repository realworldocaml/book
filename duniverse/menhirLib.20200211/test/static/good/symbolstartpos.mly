%token A B C
%start<unit> main
%%
(* Case: token in front. *)
main: A B hop sugar { $symbolstartpos, $symbolstartofs }
(* Case: epsilon symbol in front, followed with non-nullable symbol in front. *)
hop: nothing bar A B { $symbolstartpos }
(* Case: nullable symbol in front. *)
bar: foo? B { $symbolstartpos }
foo: C nothing {}
(* Case: epsilon rule. *)
nothing: { $symbolstartpos }
(* Sugar. *)
sugar: c = C { $loc, $loc(c), $sloc }
