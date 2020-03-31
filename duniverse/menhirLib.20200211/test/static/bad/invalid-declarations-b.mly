%token <int> FOO
%token BAZ QUUX HOP
%token BAR BAT
%token FOO QWD ASFLJ QWKJH QWDK
%start RAT /* error */

%%

main:
  FOO BAR

