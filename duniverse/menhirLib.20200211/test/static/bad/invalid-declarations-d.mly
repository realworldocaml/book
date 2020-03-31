%token <int> FOO
%token BAZ QUUX HOP
%token BAR BAT
%token FOO QWD ASFLJ QWKJH QWDK
%start<int> rat
%type<int> date time
%token BAZAR
%left FOO BAR
%right /* error */
%nonassoc BAR QWD QWD QWD ASD QWD D QWD WQD QWD
%token BAR

%%

main:
  {}


