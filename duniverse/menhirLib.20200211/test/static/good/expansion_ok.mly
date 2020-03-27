(* This grammar looks very much like ../bad/expansion_diverges.mly
   except [wrap] does not use its argument, so expansion actually
   terminates. This grammar can (and should) be accepted. *)

%token A
%start<unit> start
%%

start:
 seq(A) {}

wrap(t):
 A {}

seq(t):
| wrap(seq(wrap(t))) {}
