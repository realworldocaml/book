(* Because [ignore] does not use its argument, expansion terminates.
   This grammar can (and should) be accepted. *)

%token A B C
%start<unit> start
%%

ignore(X):
  C {}

foo(X):
  ignore(foo(bar(X))) {}

bar(X):
  B {}

start:
 foo(A) {}
