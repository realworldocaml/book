/* $Header: /home/pauillac/formel1/fpottier/cvs/hmx/multi/parser.mly,v 1.4 2001/03/28 16:12:36 fpottier Exp $ */
/*

This file defines the toy language's grammar.

*/

%{

open Multi.Hm
open Multi.Primitives

let app e1 e2 =
  App(e1, e2)

let prim p =
  PrimApp (p, [])

let primapp p e =
  PrimApp (p, [e])

let primapp2 p e1 e2 =
  PrimApp (p, [e1; e2])

let sequence e1 e2 =
  Let ("-", primapp PrimEnsureUnit e1, e2)

(* A pattern is either a variable or the unit constant. A wildcard pattern is viewed as a variable "-", which cannot
   be named inside expressions, because the lexer will not view it as an identifier. *)

type pattern =
  | PVariable of string
  | PUnit

let rec make_fun patterns expr =
  match patterns with
  | [] ->
      expr
  | (PVariable x) :: patterns ->
      Lambda (x, make_fun patterns expr)
  | PUnit :: patterns ->
      Lambda ("-", sequence (Var "-") (make_fun patterns expr))

let rec make_let (pattern, body) =
  match pattern with
  | PVariable x ->
      x, body
  | PUnit ->
      "-", primapp PrimEnsureUnit body

let rec make_extension base = function
  | [] ->
      base
  | (mutflag, label, expr) :: rest ->
      primapp2
	(if mutflag then PrimExtendMutable label else PrimExtend label)
	(make_extension base rest) expr

let rec make_restriction base = function
  | [] ->
      base
  | label :: rest ->
      primapp (PrimRestrict label) (make_restriction base rest)

type case_pattern =
  | CaseOne of string
  | CaseAll

let rec make_case subject = function
  | [] ->
      prim PrimCaseNone
  | (CaseOne tag, e) :: rest ->

      (* For convenience, we have made PrimCaseOne a binary primitive operation. This allows avoiding unnecessary
	 $\eta$-expansions.

	 Note that our current encoding of ``case'' constructs into $\lambda$-abstractions and applications of
	 primitives is not satisfactory, because it forces all such constructs to have monomorphic type. *)

      primapp2 (PrimCaseOne tag) (make_fun [subject] e) (make_case subject rest)
  | (CaseAll, e) :: rest ->
      if rest <> [] then
	Printf.eprintf "Warning: unused `case' clauses.\n";
      make_fun [subject] e

%}

%token <string> UIDENT
%token <string> LIDENT
%token <int> INT

%token ARROW
%token BAR
%token CASE
%token CHOOSE
%token DOT
%token END
%token EQUAL
%token FUN
%token IN
%token LEFTARROW
%token LET
%token LPAREN
%token MUTABLE
%token RPAREN
%token LBRACE
%token OF
%token RBRACE
%token REC
%token SEMI
%token SEMISEMI
%token SETMINUS
%token UNDERSCORE
%token UNIT
%token MATCH

%start phrase
%type <Multi.Hm.phrase> phrase

%%

/* Phrases. */

phrase:
    expression SEMISEMI                                         { "-", $1 }
  | LET pattern pattern_list EQUAL expression SEMISEMI          { make_let ($2, make_fun $3 $5) }
  | LET REC LIDENT pattern_list EQUAL expression SEMISEMI       { let ff = Lambda ($3, make_fun $4 $6) in
                                                                  $3, primapp PrimFix ff }
;

/* Expressions. */

expression:
    expression2			      				{ $1 }
  | FUN pattern_list ARROW expression				{ make_fun $2 $4 }
  | LET pattern pattern_list EQUAL expression IN expression     { let name, body = make_let ($2, make_fun $3 $5) in
                                                                  Let (name, body, $7) }
  | LET REC LIDENT pattern_list EQUAL expression IN expression  { let ff = Lambda ($3, make_fun $4 $6) in
                                                                  Let ($3, primapp PrimFix ff, $8) }
;

expression2:
    expression1							{ $1 }
  | expression1 SEMI expression					{ sequence $1 $3 }
;

expression1:
    expression0						        { $1 }
  | expression1 expression0  			                { app $1 $2 }
  | CHOOSE expression0 expression0                              { primapp2 PrimChoice $2 $3 }
;

expression0:
    LIDENT 						        { Var $1 }
  | UIDENT 						        { prim (PrimConstructor $1) }
  | UNIT						        { prim PrimUnit }
  | INT                                                         { prim (PrimInt $1) }
  | LPAREN expression RPAREN	      			        { $2 }
  | expression0 LBRACE field_expr_list RBRACE	                { make_extension $1 $3 }
  | expression0 DOT LIDENT		       	 	        { primapp (PrimAccess $3) $1 }
  | expression0 SETMINUS LIDENT                                 { primapp (PrimRestrict $3) $1 }
  | expression0 SETMINUS LBRACE field_list RBRACE               { make_restriction $1 $4 }
  | expression0 DOT LIDENT LEFTARROW expression1                { primapp2 (PrimMutate $3) $1 $5 }
  | CASE LIDENT OF case_list END                                { app (make_case (PVariable $2) $4) (Var $2) }
  | CASE pattern EQUAL expression OF case_list END              { app (make_case $2 $6) $4 }
;

/* Auxiliary definitions. */

field_expr_list:
    /* epsilon */					        { [] }
  | field_expr                                                  { [$1] }
  | field_expr SEMI field_expr_list                      	{ $1 :: $3 }
;

field_expr:
    LIDENT EQUAL expression1                                    { false, $1, $3 }
  | MUTABLE LIDENT EQUAL expression1                            { true, $2, $4 }
;

field_list:
    /* epsilon */					        { [] }
  | LIDENT				                        { [$1] }
  | LIDENT SEMI field_list               	                { $1 :: $3 }
;

pattern_list:
    /* epsilon */					        { [] }
  | pattern pattern_list                   		        { $1 :: $2 }
;

pattern:
    LIDENT                                                      { PVariable $1 }
  | UNDERSCORE                                                  { PVariable "-" }
  | UNIT                                                        { PUnit }
;

case_list:
    /* epsilon */                                               { [] }
  | case_entry case_list                                        { $1 :: $2 }
;

case_entry:
    BAR case_pattern ARROW expression                           { $2, $4 }
;

case_pattern:
    UIDENT                                                      { CaseOne $1 }
  | UNDERSCORE                                                  { CaseAll }
;

