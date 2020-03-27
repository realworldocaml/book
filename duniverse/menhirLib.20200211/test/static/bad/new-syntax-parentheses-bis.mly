%token A B C
%start<unit> main

%%

let b == B

let main :=
  A; (b | C)
     (* Error: a choice cannot be parenthesized like this. *)
