(* We want the tokens to be independent of the [Semantics] parameter,
   so we declare them here, in a separate file, as opposed to within
   [parser.mly]. *)

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%%

