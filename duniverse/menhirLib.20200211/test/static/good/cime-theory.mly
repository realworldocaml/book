/***************************************************************************

parser for (equational) theory

CiME Project - Démons research team - LRI - Université Paris XI

$Id: theory_parser.mly,v 1.4 2003/09/12 12:29:52 contejea Exp $

***************************************************************************/

%{

  open User_signatures
  open Term_algebra

%}

%token <User_signatures.symbol_id> IDENT
%token <int> INT
%token KW_ACU KW_ACI KW_AG KW_ACUN KW_BR
%token COMMA SEMICOLON OPENPAR CLOSEPAR
%token EOF

%start theory
%type <User_signatures.symbol_id Theory.elem_theory list> theory

%%

theory:
    EOF
      { [] }
  | decl
      {
        [$1]
      }
  | decl SEMICOLON theory
      {
	$1 :: $3
      }
;

decl:
  acu { $1 }
| aci { $1 }
| ag { $1 }
| acun { $1 }
| br { $1 }
;

acu:
KW_ACU OPENPAR IDENT COMMA IDENT CLOSEPAR
  {
    Theory.ACU($3,$5)
  }
;

aci:
KW_ACI OPENPAR IDENT CLOSEPAR
  {
    Theory.ACI($3)
  }
;

ag:
KW_AG OPENPAR IDENT COMMA IDENT COMMA IDENT CLOSEPAR
  {
    Theory.AG($3,$5,$7)
  }
;

acun:
KW_ACUN OPENPAR IDENT COMMA IDENT COMMA INT CLOSEPAR
  {
    Theory.ACUN($3,$5,$7)
  }
;

br:
KW_BR OPENPAR IDENT COMMA IDENT COMMA IDENT COMMA IDENT CLOSEPAR
  {
    Theory.BR($3,$5,$7,$9)
  }
;
