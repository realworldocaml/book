%token A L R
%start<unit> main

%%

wrap(X):
  X
    {}

array(seq,X):
  L seq(wrap(X)) R
    {}

X(x):
  array(X,x)
    {}

(* Attempt to cause a name capture during selective expansion:
   array(seq,X) is specialized with (seq := X) but keeps its
   formal parameter X. This could create a confusion if formal
   parameters were not internally renamed. *)

main:
  array(X, A)
    {}
