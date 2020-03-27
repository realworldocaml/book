%token FOO BAR <int> /* the error is the type */

%%

main: HOP BAZ QUUX { () }

