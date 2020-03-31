%{

open Sig
open MiniPrimitives

let app e1 e2 =
  EApp (e1, e2)

let infix id e1 e2 =
  app (app (EVar id) e1) e2

let seq e1 e2 =
  EBinding (BindValue [[], PTuple [], e1], e2)

%}

%token <string> LID
%token <string> UID
%token <int> INTEGER

%token LET
%token IN
%token EQUAL
%token BACKSLASH
%token DOT
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token LANGLE
%token RANGLE
%token TYPE
%token EXISTS
%token FORALL
%token ARROW
%token LPAREN
%token RPAREN
%token SEMI
%token COMMA
%token EOF
%token BANG
%token BAR
%token COLON
%token SLASH
%token DATA
%token MUTABLE
%token LEFTARROW
%token WILD
%token AS
%token REC
%token AND
%token MATCH
%token WITH
%token END
%token UNIT
%token STAR
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4

%left     INFIXOP0 EQUAL
%right    INFIXOP1
%left     INFIXOP2
%left     INFIXOP3
%right    INFIXOP4

%start program
%type <MiniPrimitives.t Sig.binding list> program

%%

program:
| bindings EOF                                                  { List.rev $1 }
;

scheme:
  forall typ                                                    { $1, $2 }
;

forall:
  /* epsilon */                                                 { [] }
| FORALL quantifiers DOT                                        { $2 }
;

quantifiers:
  quantifier                                                    { [ $1 ] }
| quantifiers quantifier                                        { $2 :: $1 }
;

quantifier:
  LID                                                           { $1 }
;

typ:
  type2                                                         { $1 }
;

type2:
| type1 ARROW type2                                             { TypArrow ($1, $3) }
| type1                                                         { $1 }
;

type1:
| type0 STAR type1                                              { match $3 with
                                                                  | TypTuple typs -> TypTuple ($1 :: typs)
								  | _ -> TypTuple [ $1; $3 ] }
| type0                                                         { $1 }
;

type0:
  LID                                                           { TypVar $1 }
| UNIT                                                          { TypTuple [] }
| LPAREN typ RPAREN                                             { $2 }
| LPAREN typ COMMA types RPAREN                                 { TypTuple ($2 :: $4) }

;
/* TEMPORARY autoriser les 'equations inline dans les types */

types:
  typ                                                           { [ $1 ] }
| typ COMMA types                                               { $1 :: $3 }
;

expression:
  expression400                                                 { $1 }
;

expression400:
  BACKSLASH pattern DOT expression400                           { ELambda ($2, $4) }
| binding IN expression400                                      { EBinding ($1, $3) }
| EXISTS quantifiers DOT expression400                          { EExists ($2, $4) }
/* | FORALL quantifiers DOT expression400                          { EForall ($2, $4) }
   This production has been suppressed to avoid redundancy with the [let forall] construct.
   This decision simplifies the code that deals with polymorphic recursion. */
| expression300 SEMI expression400                              { seq $1 $3 }
| expression300                                                 { $1 }
;

expression300:
  expression0 DOT LID LEFTARROW expression100                   { ERecordUpdate ($1, $3, $5) }
| expression200                                                 { $1 }
;

expression200:
  expression200 INFIXOP0 expression200                          { infix $2 $1 $3 }
| expression200 INFIXOP1 expression200                          { infix $2 $1 $3 }
| expression200 INFIXOP2 expression200                          { infix $2 $1 $3 }
| expression200 INFIXOP3 expression200                          { infix $2 $1 $3 }
| expression200 INFIXOP4 expression200                          { infix $2 $1 $3 }
| expression200 EQUAL expression200                             { infix "=" $1 $3 }
| expression100                                                 { $1 }
;

expression100:
  expression100 expression0                                     { EApp ($1, $2) }
| expression0                                                   { $1 }
;

expression0:
  LID                                                           { EVar $1 }
| INTEGER                                                       { EPrimApp (PIntegerConstant $1, []) }
| expression0 DOT LID                                           { ERecordAccess ($1, $3) }
| LBRACE bindings RBRACE                                        { ERecordExtend (List.rev $2, ERecordEmpty) }
| LPAREN RPAREN                                                 { ETuple [] }
| LPAREN expression RPAREN                                      { $2 }
| LPAREN expression COLON typ RPAREN                            { ETypeConstraint ($2, $4) }
| LPAREN expression COMMA expressions RPAREN                    { ETuple ($2 :: $4) }
;

expressions:
  expression                                                    { [ $1 ] }
| expression COMMA expressions                                  { $1 :: $3 }
;

bindings:
  /* epsilon */                                                 { [] }
| bindings binding                                              { $2 :: $1 }
;

binding:
| LET value_definitions                                         { BindValue $2 }
| LET REC value_definitions                                     { BindRecValue $3 }
;

value_definitions:
  value_definition                                              { [ $1 ] }
| value_definitions AND value_definition                        { $3 :: $1 }
;

value_definition:
  forall pattern0 equal_expression                              { ($1, $2, $3) }
;

equal_expression:
  EQUAL expression                                              { $2 }
| COLON typ EQUAL expression                                    { ETypeConstraint ($4, $2) }
| pattern0 equal_expression                                     { ELambda ($1, $2) }
;

record_fields:
  LID                                                           { [ $1 ] }
| record_fields COMMA LID                                       { $3 :: $1 }
;

pattern:
  pattern3                                                      { $1 }
;

pattern3:
  pattern2                                                      { $1 }
| pattern3 COLON typ                                            { PTypeConstraint ($1, $3) }
;

pattern2:
  pattern1                                                      { $1 }
| LID AS pattern2                                               { PAlias ($1, $3) }
;

pattern1:
  pattern0                                                      { $1 }
| UID pattern0s                                                 { PData ($1, List.rev $2) }
;

pattern0:
  LID                                                           { PVar $1 }
| WILD                                                          { PWildcard }
| INTEGER                                                       { assert false (* TEMPORARY *) }
| LPAREN RPAREN                                                 { PTuple [] }
| LPAREN pattern RPAREN                                         { $2 }
| LPAREN pattern COMMA patterns RPAREN                          { PTuple ($2 :: $4) }
;

patterns:
  pattern                                                       { [ $1 ] }
| pattern COMMA patterns                                        { $1 :: $3 }
;

pattern0s:
  /* epsilon */                                                 { [] }
| pattern0s pattern0                                            { $2 :: $1 }
;

opt_bar:
  /* epsilon */                                                 { () }
| BAR                                                           { () }
;

