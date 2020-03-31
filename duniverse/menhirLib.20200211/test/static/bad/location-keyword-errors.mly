%token <int> A
%token B C

%%

main:
  x = A; y = B
    { $startpos(z) }
| A
    { $endpos($2) }

