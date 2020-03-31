%token FOO BAR
%type <int> INT
%token BAZ::::: QUUX /* the error is the colon */
%token HOP

%%

main: HOP BAZ QUUX { () }

