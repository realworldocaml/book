%type <int> INT
%token ( BAZ QUUX /* the error is the opening parenthesis */
%token HOP

%%

main: HOP BAZ QUUX { () }

