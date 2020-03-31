/* $Header: /home/pauillac/formel1/fpottier/cvs/toy/toyParser.mly,v 1.6.2.1 2000/01/27 15:49:44 fpottier Exp $ */
/*

This file defines the toy language's grammar.

*/

%{

open ToySyntax
open ToyParserUtil

%}

%token <string> IDENT
%token <string> CONSTRUCTOR
%token <int> INTCONST
%token <char> CHARCONST
%token <float> FLOATCONST
%token <string> STRINGCONST

%token AND
%token ARROW
%token AS
%token AT
%token ATAT
%token ASSIGN
%token BANG
%token BAR
%token BARRBRACKET
%token COLON
%token COMMA
%token DOT
%token ELSE
%token EQUAL
%token FALSE
%token FUN
%token FUNCTION
%token IF
%token IN
%token INCLUDE
%token LBRACE
%token LEFTARROW
%token LET
%token LBRACKETBAR
%token LPAREN
%token MATCH
%token OF
%token REC
%token REF
%token RBRACE
%token RPAREN
%token SEMI
%token SEMISEMI
%token SETMINUS
%token SHARP
%token THEN
%token TO
%token TRUE
%token TRY
%token TYPE
%token UNDERSCORE
%token UNIT
%token VALUE
%token WITH

%start phrase
%type <ToySyntax.phrase> phrase

%%

/* Toplevel phrases */

phrase:
    SHARP IDENT						        { PhraseDirective $2 }
      /* Toplevel directives, used to toggle runtime flags. */
  | LET binding_list SEMISEMI				        { PhraseLet(false, $2) }
  | LET REC binding_list SEMISEMI			        { PhraseLet(true, $3) }
      /* Toplevel definitions. */
/*  | VALUE IDENT COLON type_scheme SEMISEMI                      { PhraseValue($2, $4) } TEMPORARY
      Toplevel declarations. Used to add entries to the environment without supplying their implementation. */
  | expression SEMISEMI					        { PhraseExpr $1 }
      /* Toplevel expressions. */
  | INCLUDE STRINGCONST                                         { PhraseInclude $2 }
      /* Read and process a file containing source code. */
/* TEMPORARY | TYPE IDENT SEMISEMI                                         { PhraseAbstractType ($2, []) }
      Abstract type declaration.
  | TYPE IDENT LPAREN variance_comma_list RPAREN SEMISEMI       { PhraseAbstractType ($2, $4) }
      Abstract parameterized type declaration. The variance of each parameter must be specified by supplying
	 a plus or minus sign. */
;

/* Value expressions */

expression:
    expression3			      				{ $1 }
  | FUNCTION function_match	   				{ EFun $2 }
  | FUN fun_match						{ EFun $2 }
      /* Functions. The two constructions are essentially identical; refer to O'Caml syntax. */
  | MATCH expression WITH function_match			{ EApp (EFun $4, $2) }
  | LET binding_list IN expression		                { ELet(false, $2, $4) }
  | LET REC binding_list IN expression		                { ELet(true, $3, $5) }
  | REC pattern IN expression					{ EApp (EVar "_rec", EFun [[$2], $4]) }
      /* A primitive fix-point construct. */
  | TRY expression WITH try_match                               { ETry ($2, $4) }

expression3:
    expression2							{ $1 }
  | expression2 SEMI expression					{ ESeq($1, $3) }
      /* Sequences are internally translated to function applications. */

expression2:
    expression1							{ $1 }
  | IF expression1 THEN expression2 ELSE expression2		{ EIf ($2, $4, $6) }
  | expression1 COMMA expression2				{ EApp (EApp (EVar "_pair", $1), $3) }
  | expression1 ASSIGN expression1				{ EApp (EApp(EVar ":=", $1), $3) }
;

expression1:
    expression0						{ $1 }
  | CONSTRUCTOR expression0				{ EApp (EConstruct $1, $2) }
  | expression1 expression0			        { EApp ($1, $2) }
  | REF expression0                                     { EApp (EVar "ref", $2) }
;

expression0:
    IDENT 						{ EVar $1 }
  | CONSTRUCTOR						{ EApp (EConstruct $1, EConstant ConstUnit) }
  | constant						{ EConstant $1 }
  | LPAREN expression RPAREN	      			{ $2 }
  | LBRACE label_expression_list RBRACE			{ ERecord $2 }
  | LBRACKETBAR expr_semi_list BARRBRACKET              { EVector $2 }
  | BANG expression0					{ EApp (EVar "!", $2) }
  | expression0 DOT IDENT LEFTARROW expression1         { EApp (EApp (ERecordUpdate $3, $1), $5) }
  | expression0 DOT IDENT		       		{ EApp (ERecordAccess $3, $1) }
  | expression0 SETMINUS IDENT                          { EApp (ERecordRestrict $3, $1) }
  | expression0 AT expression1                          { EApp (EApp (EVar "@", $1), $3) }
  | expression0 ATAT expression1                        { EApp (EApp (EVar "@@", $1), $3) }
  | CONSTRUCTOR COLON expression0                       { ELabel ($1, $3) }
;

constant:
    INTCONST						{ ConstInt $1 }
  | FALSE						{ ConstBool false }
  | TRUE						{ ConstBool true }
  | UNIT						{ ConstUnit }
  | FLOATCONST                                          { ConstFloat $1 }
  | CHARCONST                                           { ConstChar $1 }
  | STRINGCONST                                         { ConstString $1 }
;

/* Auxiliary expression definitions */

label_expression:
    IDENT EQUAL expression2				{ $1, $3 }
;

label_expression_list:
    /* epsilon */					{ [] }
  | label_expression					{ [$1] }
  | label_expression SEMI label_expression_list		{ $1 :: $3 }
;

expr_semi_list:
    /* epsilon */                                       { [] }
  | expression2                                         { [$1] }
  | expression2 SEMI expr_semi_list                     { $1 :: $3 }
;

/* Patterns */

fun_match:
    simple_pattern_list ARROW expression BAR fun_match	{ ($1, $3) :: $5 }
  | simple_pattern_list ARROW expression		{ [$1, $3] }
;

function_match:
    pattern ARROW expression BAR function_match		{ ([$1], $3) :: $5 }
  | pattern ARROW expression				{ [[$1], $3] }
;

try_match:
    pattern ARROW expression BAR try_match		{ ($1, $3) :: $5 }
  | pattern ARROW expression				{ [$1, $3] }
;

simple_pattern:
    UNDERSCORE				       		{ PWildcard }
  | IDENT						{ PVar $1 }
  | constant						{ PConstant $1 }
  | CONSTRUCTOR						{ PConstruct ($1, PConstant ConstUnit) }
  | LPAREN pattern RPAREN				{ $2 }
  | LBRACE label_pattern_list RBRACE			{ PRecord $2 }
;

pattern:
    simple_pattern                                      { $1 }
  | pattern AS IDENT					{ PAlias ($1, $3) }
  | pattern COMMA pattern				{ PPair ($1, $3) }
  | CONSTRUCTOR simple_pattern				{ PConstruct ($1, $2) }
  | REF simple_pattern                                  { PRef $2 }
  | pattern BAR pattern                                 { POr ($1, $3) }
;

/* Auxiliary pattern definitions */

label_pattern_list:
    /* epsilon */					{ StringMap.empty }
  | IDENT EQUAL pattern					{ StringMap.singleton $1 $3 }
  | IDENT EQUAL pattern SEMI label_pattern_list		{
                                                          let label = $1
                                                          and lpmap = $5 in
							  try
							    let _ = StringMap.lookup label lpmap in
							    raise (DuplicateLabel label)
							  with Not_found ->
							    StringMap.add label $3 lpmap
                                                        }
;

simple_pattern_list :
    simple_pattern					{ [$1] }
  | simple_pattern simple_pattern_list			{ $1 :: $2 }
;

/* Binding lists */

binding_list:
    binding AND binding_list                            { $1 :: $3 }
  | binding                                             { [$1] }
;

binding:
    pattern EQUAL expression			        { ($1, $3) }
  | IDENT simple_pattern_list EQUAL expression          { (PVar $1, EFun [$2, $4]) }
;

