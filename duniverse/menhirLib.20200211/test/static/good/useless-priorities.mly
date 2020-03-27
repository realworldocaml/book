%token <int> INT
%token PLUS TIMES
%left PLUS
%left TIMES
%token EOF
%start <int> main
%%
expression: i = INT { i } | e = expression; o = op; f = expression { o e f }
op: PLUS { ( + ) } | TIMES { ( * ) }
main: e = expression EOF { e }
