%token <int> A
%token B C

%%

main: A B
  { $1 + $3 }

