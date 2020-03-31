(* This partial grammar specification defines the grammar's entry
   point to be an expression, followed with an end-of-line token. *)

%start <int> main

%%

main:
| e = expr EOL
    { e }

