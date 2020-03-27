%start <unit> name_clash_1_a
%token FOO

%%

name_clash_1_a:
  a { () }

a:
  FOO { () }

