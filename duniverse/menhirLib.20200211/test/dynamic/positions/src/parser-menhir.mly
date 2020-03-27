%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%token DOT COMMA

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%type<Aux.annotations> annotations
%start<Aux.main> main

%{ open Aux %}

%%

main:
| n = nothing e = expr EOL
    { ($startpos, $endpos), n, e, $symbolstartpos }

/* Added just to exercise productions with an empty right-hand side. */
%inline nothing:
| /* nothing */
    { ($startpos, $endpos) }

/* Added just to exercise productions with an empty right-hand side, in a choice. */
optional_dot:
| n = nothing
    { ($startpos, $endpos), Some n }
| DOT
    { ($startpos, $endpos), None }

%inline optional_comma:
| n = nothing
    { ($startpos, $endpos), Some n }
| COMMA
    { ($startpos, $endpos), None }

%inline annotations:
  optional_dot optional_comma
    { ($startpos, $endpos),
      $endpos($1), $startpos($2), $startofs, $symbolstartpos,
      $1, $2 }

raw_expr:
| INT
    { EInt }
| a = annotations LPAREN n = nothing e = expr RPAREN o = optional_dot
    { EParen(a, n, e, o, $symbolstartofs) }
| expr PLUS expr
| expr MINUS expr
| expr TIMES expr
| expr DIV expr
    { EBinOp ($1, $3) }
| MINUS expr %prec UMINUS
    { EUnOp $2 }

%inline expr:
  e = raw_expr
    { ($startpos, $endpos), $endpos($0), $endofs($0), e }

