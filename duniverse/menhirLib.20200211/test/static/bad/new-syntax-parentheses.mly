%token A B C
%start<unit> main

%%

let main :=
  A; (B | C)
     (* Error: a choice cannot be parenthesized like this. *)
