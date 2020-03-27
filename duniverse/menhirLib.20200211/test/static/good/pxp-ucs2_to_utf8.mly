/******************************************************/
/*    Claudio Sacerdoti Coen <sacerdot@cs.unibo.it>   */
/*                   14/05/2000                       */
/******************************************************/

%token <int>CHAR
%token <string>IDENT
%token LET
%token EQ
%token END_OF_LET
%token RBRACKET
%token PIPE
%token LBRACKET
%token RANGE
%token EOF
%start main
%type <Types.definition list> main

%%

main:
   EOF              { [] }
 | declaration main { $1::$2 }
;

declaration:
   LET IDENT EQ regexp END_OF_LET
      { { Types.id = $2 ; Types.rel = $4 } }
;

regexp:
   regexptoken PIPE regexp  { $1::$3 }
 | regexptoken              { [$1] }
;

regexptoken:
   CHAR                               { Types.Char $1 }
 | LBRACKET CHAR RANGE CHAR RBRACKET  { Types.Interval ($2,$4) }
 | IDENT                              { Types.Identifier $1 }
;
