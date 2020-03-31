%token <int> FOO
%token BAZ QUUX HOP
%token BAR int BAT /* error */

%%

main:
  FOO BAR

