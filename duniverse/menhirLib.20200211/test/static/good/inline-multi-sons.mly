%token A B EOF
%start<int> s
%%

(* We test the freshness of producer's name during inlining.
   ioption and delimited both use 'x' in their definition.
*)
s: ioption(pair(ioption(A), delimited(A, B, A))) EOF { 0 }
