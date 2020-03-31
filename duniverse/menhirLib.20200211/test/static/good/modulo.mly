/* This parser translates concrete syntax to external syntax. */

/* TEMPORARY songer au traitement des constructeurs: lorsqu'ils sont
   sous-appliqu'es, le parseur (?) devrait les eta-expanser */

/* TEMPORARY toujours permettre la possibilité de nommer les variables de types sur lesquelles on va abstraire */

%{

open Syntax

let abs_type formals body =
  List.fold_right (fun (channel, id, kind) body ->
    TypAbs (channel, id, kind, body)
  ) formals body

let seq e1 e2 =
  EBinding (BindValue [[], PTuple [], e1], e2)

let app e1 e2 =
  EApp (e1, e2)

let infix id e1 e2 =
  app (app (EVar id) e1) e2

%}

%token <string> LID
%token <string> UID
%token INT
%token <int> INTEGER

%token LET
%token IN
%token EQUAL
%token COLONCOLON
%token BACKSLASH
%token DOT
%token EXPORT
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token LANGLE
%token RANGLE
%token FIELD
%token TYPE
%token PROG
%token EXISTS
%token FROM
%token FORALL
%token ARROW
%token LPAREN
%token RPAREN
%token SEMI
%token MINUS
%token PLUS
%token VALUE
%token COMMA
%token EOF
%token ASSUME
%token DISPLAY
%token IMPORT
%token SOLVE
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
%token STAR
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4

%left     INFIXOP0 EQUAL
%right    INFIXOP1
%left     INFIXOP2 PLUS MINUS
%left     INFIXOP3
%right    INFIXOP4

%start program
%type <Syntax.program> program

%%

/* TEMPORARY left-recursion more efficient in yacc? */

program:
| declarations PROG expression EOF                              { $1, $3 }
;

declarations:
  /* epsilon */                                                 { [] }
| declaration declarations                                      { $1 :: $2 }
;

declaration:
  FIELD LID formals EQUAL scheme                                { DeclField ($2, false, $3, $5) }
| FIELD MUTABLE LID formals EQUAL scheme                        { DeclField ($3, true, $4, $6) }
| DATA LID                                                      { DeclData ($2, [], []) }
| DATA LID formals EQUAL opt_bar data_cases                     { DeclData ($2, $3, $6) }
;
/* TEMPORARY autoriser `let type' a toplevel */

formals:
  /* epsilon */                                                 { [] }
| formal formals                                                { $1 :: $2 }
;

formal:
  LID                                                           { "", $1, KindUnification.variable() }
| LID COLON LID                                                 { $1, $3, KindUnification.variable() }
| LPAREN LID COLONCOLON kind RPAREN                             { "", $2, $4 }
| LID COLON LPAREN LID COLONCOLON kind RPAREN                   { $1, $4, $6 }
;

kind:
  STAR                                                          { KindUnification.star }
; /* TEMPORARY add more kinds */

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
  LID                                                           { $1, KindUnification.variable() }
| LPAREN LID COLONCOLON kind RPAREN                             { $2, $4 }
;

typ:
  type2                                                         { $1 }
;

type2:
  BACKSLASH formal DOT type2                                    { let p, id, k = $2 in
                                                                  TypAbs (p, id, k, $4) }
| type1 ARROW type2                                             { TypArrow ($1, $3) }
| type1                                                         { $1 }
;

type1:
  type1 type0                                                   { TypApp ($1, "", $2) }
| type1 LID COLON type0                                         { TypApp ($1, $2, $4) }
| type0                                                         { $1 }
;

type0:
  LID                                                           { TypVar $1 }
| INT                                                           { TypInteger }
| LBRACE type2 RBRACE                                           { TypRecord $2 }
| LANGLE row RANGLE                                             { $2 }
| LPAREN RPAREN                                                 { TypTuple [] }
| LPAREN typ RPAREN                                             { $2 }
| LPAREN typ COMMA types RPAREN                                 { TypTuple ($2 :: $4) }
;

types:
  typ                                                           { [ $1 ] }
| typ COMMA types                                               { $1 :: $3 }
;

row:
  /* epsilon */                                                 { TypRowEmpty }
| LID                                                           { TypVar $1 }
| row_entry                                                     { TypRowCons ($1, TypRowEmpty) }
| row_entry SEMI row                                            { TypRowCons ($1, $3) }
;

row_entry:
  MINUS LID                                                     { $2, None }
| PLUS LID actuals                                              { $2, Some $3 }
;

actuals:
  /* epsilon */                                                 { [] }
| type0 actuals                                                 { ("", $1) :: $2 }
| LID COLON type0 actuals                                       { ($1, $3) :: $4 }
;

data_cases:
  data_case                                                     { [ $1 ] }
| data_cases BAR data_case                                      { $3 :: $1 }
;

data_case:
  UID data_params                                               { $1, $2 }
;

data_params:
  /* epsilon */                                                 { [] }
| type0 data_params                                             { $1 :: $2 }
;

expression:
  expression400                                                 { $1 }
;

expression400:
  BACKSLASH pattern DOT expression400                           { EAbs ($2, $4) }
| binding IN expression400                                      { EBinding ($1, $3) }
| EXISTS quantifiers DOT expression400                          { EExists ($2, $4) }
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
| expression200 PLUS expression200                              { infix "+" $1 $3 }
| expression200 MINUS expression200                             { infix "-" $1 $3 }
| expression200 EQUAL expression200                             { infix "=" $1 $3 }
| expression100                                                 { $1 }
;

expression100:
  expression100 expression0                                     { EApp ($1, $2) }
| DISPLAY expression0                                           { EDisplay $2 }
| expression0                                                   { $1 }
;

expression0:
  LID                                                           { EVar $1 }
| UID                                                           { EData $1 }
| INTEGER                                                       { EInteger $1 }
| expression0 DOT LID                                           { ERecordAccess ($3, $1) }
| LBRACE expression bindings RBRACE                             { ERecordExtend ($3, $2) }
| LBRACE bindings RBRACE                                        { ERecordExtend ($2, ERecordEmpty) }
| LPAREN RPAREN                                                 { ETuple [] }
| LPAREN expression RPAREN                                      { $2 }
| LPAREN expression COMMA expressions RPAREN                    { ETuple ($2 :: $4) }
| LPAREN expression COLONCOLON typ RPAREN                       { ETypeConstraint ($2, $4) }
| MATCH expression WITH opt_bar cases END                       { EMatch ($2, List.rev $5) }
;

expressions:
  expression                                                    { [ $1 ] }
| expression COMMA expressions                                  { $1 :: $3 }
;

bindings:
  /* epsilon */                                                 { [] }
| binding bindings                                              { $1 :: $2 }
;
/* Right-recursion is necessary to avoid a conflict between the two
   forms of records above, involving [expression bindings] vs.
   [bindings]. */

binding:
  LET IMPORT record_fields FROM expression                      { BindImport (Binds, $3, $5) }
| LET IMPORT EXPORT record_fields FROM expression               { BindImport (BindsAndDefines, $4, $6) }
| TYPE type_definitions                                         { BindType $2 }
| LET value_definitions                                         { BindValue $2 }
| LET REC value_definitions                                     { BindRecValue $3 }
;

type_definitions:
  type_definition                                               { [ $1 ] }
| type_definitions AND type_definition                          { $3 :: $1 }
;

value_definitions:
  value_definition                                              { [ $1 ] }
| value_definitions AND value_definition                        { $3 :: $1 }
;

value_definition:
  forall pattern0 equal_expression                              { ($1, $2, $3) }
;

type_definition:
| LID formals EQUAL typ                                         { ($1, abs_type $2 $4) }
;

equal_expression:
  EQUAL expression                                              { $2 }
| COLONCOLON typ EQUAL expression                               { ETypeConstraint ($4, $2) }
| pattern0 equal_expression                                     { EAbs ($1, $2) }
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
| pattern3 COLONCOLON typ                                       { PTypeConstraint ($1, $3) }
;

pattern2:
  pattern1                                                      { $1 }
| LID AS pattern2                                               { PAlias (Binds, $1, $3) }
| EXPORT LID AS pattern2                                        { PAlias (BindsAndDefines, $2, $4) }
;

pattern1:
  pattern0                                                      { $1 }
| UID pattern0s                                                 { PData ($1, List.rev $2) }
;

pattern0:
  LID                                                           { PVar (Binds, $1) }
| EXPORT LID                                                    { PVar (BindsAndDefines, $2) }
| WILD                                                          { PWildcard }
| INTEGER                                                       { PInteger $1 }
| LBRACE row_pattern RBRACE                                     { PRecord ($2, PRecordEmpty) }
| LBRACE pattern row_pattern RBRACE                             { PRecord ($3, $2) }
| LPAREN RPAREN                                                 { PTuple [] }
| LPAREN pattern RPAREN                                         { $2 }
| LPAREN pattern COMMA patterns RPAREN                          { PTuple ($2 :: $4) }
/* TEMPORARY add data patterns and multiple-branch functions/match */
;

pattern0s:
  /* epsilon */                                                 { [] }
| pattern0s pattern0                                            { $2 :: $1 }
;

patterns:
  pattern                                                       { [ $1 ] }
| pattern COMMA patterns                                        { $1 :: $3 }
;

row_pattern:
  /* epsilon */                                                 { [] }
| VALUE LID EQUAL pattern0 row_pattern                          { ($2, $4) :: $5 }
| VALUE LID row_pattern                                         { ($2, PVar (Binds, $2)) :: $3 }
| VALUE EXPORT LID row_pattern                                  { ($3, PVar (BindsAndDefines, $3)) :: $4 }
;

cases:
  case                                                          { [ $1 ] }
| cases BAR case                                                { $3 :: $1 }
;

case:
  pattern2 ARROW expression                                     { $1, $3 }
;

opt_bar:
  /* epsilon */                                                 { () }
| BAR                                                           { () }
;

