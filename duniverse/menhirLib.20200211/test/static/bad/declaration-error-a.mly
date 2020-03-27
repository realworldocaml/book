%token FOO BAR <int> /* the error is the type */
%type <int> INT
%token BAZ::::: QUUX /* the error is the colon */
%token HOP

%%

main: HOP BAZ QUUX { () }

