%token <int> A
%token B C

%%

main: x = A; y = B
  { $2 + $1 }

