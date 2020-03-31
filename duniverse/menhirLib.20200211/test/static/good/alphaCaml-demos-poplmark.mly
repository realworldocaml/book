/*
 *  Parts of this file taken from the fullfsub implementation
 *  by the POPLmark team.
 *
 *  The parser constructs raw syntax trees, as defined by the
 *  module [Fsub.Raw]. In other words, identifiers are uninterpreted.
 *
 *  We do not check for duplicate record labels, but could (should).
 */

%{

open Strings
open Fsub.Raw

%}

%token LAMBDA
%token TTOP
%token LEQ
%token ALL
%token LET
%token IN
%token <string> UCID
%token <string> LCID
%token ARROW
%token COLON
%token COMMA
%token DOT
%token EOF
%token EQ
%token LCURLY
%token LPAREN
%token LSQUARE
%token RCURLY
%token RPAREN
%token RSQUARE
%token SEMI
%token USCORE

%start toplevel
%type <Fsub.Raw.toplevel> toplevel
%%

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
  | EOF
      { TopEOF }
  | Term SEMI toplevel
      { TopEval ($1, $3) }
  | UCID TyBinder SEMI toplevel
      { TopTypeBind ($1, $2, $4) }
  | LCID Binder SEMI toplevel
      { TopTermBind ($1, $2, $4) }

/* Right-hand sides of top-level bindings */
Binder :
  | COLON Type
      { $2 }

/* All type expressions */
Type :
  | ArrowType
      { $1 }
  | ALL UCID OType DOT Type
      { TForall ($2, $3, $5) }

/* Atomic types are those that never need extra parentheses */
AType :
  | LPAREN Type RPAREN
      { $2 }
  | UCID
      { TVar $1 }
  | TTOP
      { TTop }
  | LCURLY FieldTypes RCURLY
      { TRecord $2 }

TyBinder :
  | /* empty */
      { TTop }
  | LEQ Type
      { $2 }

/* An "arrow type" is a sequence of atomic types separated by
   arrows. */
ArrowType :
  | AType ARROW ArrowType
     { TArrow ($1, $3) }
  | AType
     { $1 }

Term :
  | AppTerm
      { $1 }
  | LAMBDA LCID COLON Type DOT Term
      { EAbs ($2, $4, $6) }
  | LET Pattern EQ Term IN Term
      { ELet ($2, $4, $6) }
  | LAMBDA UCID OType DOT Term
      { ETyAbs ($2, $3, $5) }

AppTerm :
  | PathTerm
      { $1 }
  | AppTerm PathTerm
      { EApp ($1, $2) }
  | AppTerm LSQUARE Type RSQUARE
      { ETyApp ($1, $3) }

PathTerm :
  | PathTerm DOT LCID
      { EProj ($1, $3) }
  | ATerm
      { $1 }

FieldTypes :
  | /* empty */
      { StringMap.empty }
  | NEFieldTypes
      { $1 }

NEFieldTypes :
  | LCID COLON Type
      { StringMap.singleton $1 $3 }
  | LCID COLON Type COMMA NEFieldTypes
      { StringMap.add $1 $3 $5 }

TermSeq :
  | Term
      { $1 }
  | Term SEMI TermSeq
      { ELet (PWildcard, $1, $3) }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
  | LPAREN TermSeq RPAREN
      { $2 }
  | LCID
      { EVar $1 }
  | LCURLY Fields RCURLY
      { ERecord $2 }

Fields :
  | /* empty */
      { StringMap.empty }
  | NEFields
      { $1 }

NEFields :
  | LCID EQ Term
      { StringMap.singleton $1 $3 }
  | LCID EQ Term COMMA NEFields
      { StringMap.add $1 $3 $5 }

OType :
  | /* empty */
      { TTop}
  | LEQ Type
      { $2 }

Pattern :
  | USCORE
      { PWildcard }
  | LCID COLON Type
      { PVar ($1, $3) }
  | LCURLY PatFields RCURLY
      { PRecord $2 }

PatFields :
  | /* empty */
      { StringMap.empty }
  | NEPatFields
      { $1 }

NEPatFields :
  | LCID EQ Pattern
      { StringMap.singleton $1 $3 }
  | LCID EQ Pattern COMMA NEPatFields
      { StringMap.add $1 $3 $5 }

