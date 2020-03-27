/*********************************************************************************************************************/
/*                                                                                                                   */
/*                                                   Wallace                                                         */
/*                                                                                                                   */
/*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 */
/*                                                                                                                   */
/*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   */
/*                                                                                                                   */
/*********************************************************************************************************************/
/* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/parser.mly,v 1.31.4.10 1999/06/21 12:21:01 francois Exp $ */
/*

This file defines the toy language's grammar. Quite a few features are supported; essentially all of Caml-Light's
language, with some differences, of course; in particular, there are no datatype definitions, since variant and
record types are entirely inferred.

*/

%{

open Types
open Type_expr
open Interpreter

%}

%token <string> IDENT
%token <string> CONSTRUCTOR
%token <int> INTCONST
%token <char> CHARCONST
%token <float> FLOATCONST
%token <string> STRINGCONST

%token ABS
%token AND
%token ARROW
%token AS
%token AT
%token ATAT
%token ASSIGN
%token BANG
%token BAR
%token BARRBRACKET
%token BOOL
%token CHAR
%token COERCE
%token COERCEBACK
%token COLON
%token COMMA
%token DOT
%token ELSE
%token EQUAL
%token FALSE
%token FLOAT
%token FUN
%token FUNCTION
%token GIVE
%token IF
%token IMPLIES
%token IN
%token INCLUDE
%token INT
%token LBRACE
%token LEFTARROW
%token LET
%token LBRACKET
%token LBRACKETBAR
%token LPAREN
%token MATCH
%token MAYBE
%token MINUS
%token MISSING
%token OF
%token PERCENT
%token PLUS
%token PRE
%token QUOTE
%token RAISE
%token RAISES
%token REC
%token REF
%token RBRACE
%token RBRACKET
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token STAR
%token STRING
%token THEN
%token TO
%token TRUE
%token TRY
%token TYPE
%token UNDERSCORE
%token UNIT
%token VALUE
%token WITH
%token WHERE

%start phrase
%type <Interpreter.phrase> phrase

%%

/* Toplevel phrases */

phrase:
    SHARP IDENT						        { PhraseDirective $2 }
      /* Toplevel directives, used to toggle runtime flags. */
  | LET binding_list SEMISEMI				        { PhraseLet(false, $2) }
  | LET REC binding_list SEMISEMI			        { PhraseLet(true, $3) }
      /* Toplevel definitions. */
  | VALUE IDENT COLON type_scheme SEMISEMI                      { PhraseValue($2, $4) }
      /* Toplevel declarations. Used to add entries to the environment without supplying their implementation. */
  | expression SEMISEMI					        { PhraseLet (false, [PVar "_unnamed", $1]) }
      /* Toplevel expressions. */
  | INCLUDE STRINGCONST                                         { PhraseInclude $2 }
      /* Read and process a file containing source code. */
  | TYPE IDENT SEMISEMI                                         { PhraseAbstractType ($2, []) }
      /* Abstract type declaration. */
  | TYPE IDENT LPAREN variance_comma_list RPAREN SEMISEMI       { PhraseAbstractType ($2, $4) }
      /* Abstract parameterized type declaration. The variance of each parameter must be specified by supplying
	 a plus or minus sign. */
;

/* Value expressions */

expression:
    expression3			      				{ $1 }
  | FUNCTION function_match	   				{ VGeneralFun $2 }
  | FUN fun_match						{ VGeneralFun $2 }
      /* Functions. The two constructions are essentially identical; refer to O'Caml syntax. */
  | MATCH expression WITH function_match			{ VApp (VGeneralFun $4, $2) }
  | LET binding_list IN expression		                { VLet(false, $2, $4) }
  | LET REC binding_list IN expression		                { VLet(true, $3, $5) }
  | REC pattern IN expression					{ VRec ($2, $4) }
      /* A primitive fix-point construct. It could be implemented in code instead of being primitive.
         There are some comments about this in module Typechecking. */
  | TRY expression WITH try_match                               { VGeneralTry ($2, $4) }

expression3:
    expression2							{ $1 }
  | expression2 SEMI expression					{ VApp(VGeneralFun [[PWildcard], $3], $1) }
      /* Sequences are internally translated to function applications. */

expression2:
    expression1							{ $1 }
  | IF expression1 THEN expression2 ELSE expression2		{ VIf ($2, $4, $6) }
  | expression1 COMMA expression2				{ VPair ($1, $3) }
  | expression1 ASSIGN expression1				{ VApp (VApp(VVar ":=", $1), $3) }
;

expression1:
    expression0						{ $1 }
  | CONSTRUCTOR expression0				{ VConstruct ($1, $2) }
  | expression1 expression0			        { VApp ($1, $2) }
  | RAISE expression0                                   { VApp (VVar "raise", $2) }
  | REF expression0                                     { VApp (VVar "ref", $2) }
;

expression0:
    IDENT 						{ VVar $1 }
  | CONSTRUCTOR						{ VConstruct ($1, VConstant ConstUnit) }
  | constant						{ VConstant $1 }
  | LBRACKET expression COLON constrained_type RBRACKET { VUsage ($2, $4) }
      /* This construct allows adding extra constraints on an expression's type, by simulating a use of the
         expression at the supplied type. */
  | LPAREN expression COLON type_scheme RPAREN		{ VCast ($2, $4) }
      /* This construct allows replacing an expression's type with the supplied type scheme entirely.
	 Of course, this requires that the supplied scheme be less general than the inferred scheme. */
  | LPAREN expression RPAREN	      			{ $2 }
  | LBRACE label_expression_list RBRACE			{ VRecord $2 }
  | LBRACKETBAR expr_semi_list BARRBRACKET              { VVector $2 }
  | BANG expression0					{ VApp (VVar "!", $2) }
  | expression0 DOT IDENT LEFTARROW expression1         { VApp (VApp (VRecordUpdate $3, $1), $5) }
  | expression0 DOT IDENT		       		{ VRecordAccess ($1, $3) }
  | expression0 AT expression1                          { VApp (VApp (VAsymRecordConcat, $1), $3) }
  | expression0 ATAT expression1                        { VApp (VApp (VSymRecordConcat, $1), $3) }
;

constant:
    INTCONST						{ ConstInt $1 }
  | FALSE						{ ConstBool false }
  | TRUE						{ ConstBool true }
  | UNIT						{ ConstUnit }
  | FLOATCONST                                          { ConstFloat $1 }
  | CHARCONST                                           { ConstChar $1 }
  | STRINGCONST                                         { ConstString $1 }
;;

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

label_pattern:
    IDENT EQUAL pattern					{ $1, $3 }
;

label_pattern_list:
    /* epsilon */					{ [] }
  | label_pattern					{ [$1] }
  | label_pattern SEMI label_pattern_list		{ $1 :: $3 }
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
  | IDENT simple_pattern_list EQUAL expression          { (PVar $1, VGeneralFun [$2, $4]) }
;

/* Type expressions */

constrained_type:
    type1									{ $1, TEBottom, [] }
  | type1 RAISES type1                                                          { $1, $3, [] }
  | type1 BAR LBRACE coercion_list RBRACE		         		{ $1, TEBottom, $4 }
  | type1 RAISES type1 BAR LBRACE coercion_list RBRACE         		        { $1, $3, $6 }
;

type_scheme:
    type1									{ [], $1, TEBottom, [] }
  | type1 RAISES type1                                                          { [], $1, $3, [] }
  | type1 BAR LBRACE coercion_list RBRACE		         		{ [], $1, TEBottom, $4 }
  | type1 RAISES type1 BAR LBRACE coercion_list RBRACE         		        { [], $1, $3, $6 }
  | COERCE context COERCEBACK IMPLIES type1					{ $2, $5, TEBottom, [] }
  | COERCE context COERCEBACK IMPLIES type1 RAISES type1			{ $2, $5, $7, [] }
  | COERCE context COERCEBACK IMPLIES type1 BAR LBRACE coercion_list RBRACE	{ $2, $5, TEBottom, $8 }
  | COERCE context COERCEBACK IMPLIES type1 RAISES type1 BAR LBRACE coercion_list RBRACE	{ $2, $5, $7, $10 }
      /* In addition to a type and a set of constraints, type schemes can contain a context, delimited by
	 angle brackets. See my PhD thesis (or Smith and Trifonov's paper) for an explanation of this
         feature. */
;

type_comma_list:
    type1						{ [$1] }
  | type1 COMMA type_comma_list   			{ $1 :: $3 }
;

type1:
    type0						{ $1 }
  | type0 ARROW type1 RAISES type0                      { TEAbstract ("->", [$1; $5; $3]) }
;

type0:
    INT							{ TEAbstract("int", []) }
  | BOOL						{ TEAbstract("bool", []) }
  | UNIT						{ TEAbstract("unit", []) }
  | STRING                                              { TEAbstract("string", []) }
  | CHAR                                                { TEAbstract("char", []) }
  | FLOAT                                               { TEAbstract("float", []) }
  | INTCONST						{ match $1 with
      	       	       	       	       	       	       	    0 -> TEBottom
							  | 1 -> TETop
							  | _ -> failwith "Unexpected integer in type expression."
							}
  | variable_leader IDENT				{ TEVar $2 }
  | type0 STAR type0					{ TEAbstract ("*", [$1; $3]) }
  | IDENT                                               { TEAbstract ($1, []) }
  | type0 IDENT                                         { TEAbstract ($2, [$1]) }
  | LPAREN type1 COMMA type_comma_list RPAREN IDENT     { TEAbstract ($6, $2 :: $4) }
  | LPAREN type1 COMMA type_comma_list RPAREN REF	{ TEAbstract ("ref", $2 :: $4) }
  | LBRACE record_row RBRACE				{ TERecord (fst $2, snd $2) }
  | LBRACKET variant_row RBRACKET			{ TEVariant (fst $2, snd $2) }
  | LPAREN type1 RPAREN					{ $2 }
;

variable_leader:
    QUOTE						{ () }
  | PLUS						{ () }
  | MINUS						{ () }
  | PERCENT						{ () }
      /* Variable names can be of the form 'a, as in O'Caml, but also of the forms +a, -a or %a. This feature
         allows parsing types output by the pretty-printer, where variables carry their sign(s). */
;

record_row:
    row_type						{ [], $1 }
  | IDENT COLON row_type SEMI record_row		{ let entries, rest = $5 in ($1, $3) :: entries, rest }
      /* A row consists of a series of label : row_type pairs, separated by semi-colons, followed by a row_type
         which represents all labels not explicitly named. */

;

variant_row:
    row_type						{ [], $1 }
  | CONSTRUCTOR COLON row_type SEMI variant_row		{ let entries, rest = $5 in ($1, $3) :: entries, rest }
;

row_type:
    ABS							{ TEAbsent }
  | PRE type1						{ TEPresent $2 }
  | MAYBE type1                                         { TEMaybe $2 }
  | MISSING                                             { TEMissing }
  | type1						{ $1 }
      /* A row type gives information about one of the row's fields. The field can be Absent or Present with a
         certain type; this information might also be unknown, in which case a row variable is typically used. */
;

coercion_list:
    coercion						{ $1 }
  | coercion SEMI coercion_list				{ $1 @ $3 }
;

coercion:
    type_comma_list COERCE type_comma_list              { [$1, $3] }
  | type_comma_list COERCE type1 COERCE type_comma_list { [($1, [$3]); ([$3], $5)] }
  | type1 EQUAL type1					{ [([$1], [$3]); ([$3], [$1])] }
;

context:
    context_entry					{ [$1] }
  | context_entry SEMI context				{ $1 :: $3 }
;

context_entry:
    IDENT COLON type1					{ $1, $3 }
;

/* Abstract type declarations */

variance_comma_list:
    variance                                            { [$1] }
  | variance COMMA variance_comma_list                  { $1 :: $3 }
;

variance:
    PLUS                                                { true }
  | MINUS                                               { false }
;
