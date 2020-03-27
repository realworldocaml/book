%token FOO BAR
%start<unit> main

%%

let main :=
  ~ = FOO; ~ = BAR; <()>
