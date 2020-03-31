/* $Header: /home/pauillac/formel1/fpottier/cvs/hmx/sets/parser.mly,v 1.1.2.1 2002/12/26 11:08:34 fpottier Exp $ */
/*

This file defines the toy language's grammar.

*/

%{

open Sets.Hm
open Sets.Primitives

let sequence e1 e2 =
  App(Lambda("-", e2), e1)

let sequence arg body =
  sequence (PrimApp(PrimEnsureUnit, [arg])) body

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
      "-", PrimApp (PrimEnsureUnit, [body])

let rec make_set = function
  | [] ->
      PrimApp (PrimSetEmpty, [])
  | label :: rest ->
      PrimApp (PrimSetExtend label, [make_set rest])

%}

%token <string> IDENT

%token ARROW
%token CHOOSE
%token DOT
%token EQUAL
%token FUN
%token IN
%token LET
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token PLUS
%token QUESTION
%token SEMI
%token SEMISEMI
%token SETMINUS
%token TILDE
%token TRY
%token UNDERSCORE
%token UNIT
%token WITH
%token NORMAL
%token EXC
%token MATCH

%start phrase
%type <Sets.Hm.phrase> phrase

%%

/* Phrases. */

phrase:
    expression SEMISEMI                                         { "_", $1 }
  | LET pattern pattern_list EQUAL expression SEMISEMI          { make_let ($2, make_fun $3 $5) }
;

/* Expressions. */

expression:
    expression2			      				{ $1 }
  | FUN pattern_list ARROW expression				{ make_fun $2 $4 }
  | LET pattern pattern_list EQUAL expression IN expression     { let name, body = make_let ($2, make_fun $3 $5) in
                                                                  Let (name, body, $7) }
;

expression2:
    expression1							{ $1 }
  | expression1 SEMI expression					{ sequence $1 $3 }
;

expression1:
    expression0						        { $1 }
  | expression1 expression0  			                { App ($1, $2) }
  | CHOOSE expression0 expression0                              { PrimApp (PrimChoice, [$2;$3]) }
;

expression0:
    IDENT 						        { Var $1 }
  | UNIT						        { PrimApp (PrimUnit, []) }
  | NORMAL                                                      { PrimApp (PrimNormal, []) }
  | EXC                                                         { PrimApp (PrimExc, []) }
  | MATCH                                                       { PrimApp (PrimMatch, []) }
  | LPAREN expression RPAREN	      			        { $2 }
  | LBRACE label_list RBRACE			                { make_set $2 }
  | expression0 PLUS IDENT                                      { PrimApp (PrimSetExtend $3, [$1]) }
  | expression0 DOT IDENT		       	 	        { PrimApp (PrimSetMemberAssert $3, [$1]) }
  | expression0 SETMINUS IDENT                                  { PrimApp (PrimSetRestrict $3, [$1]) }
  | expression0 QUESTION IDENT                                  { PrimApp (PrimSetMemberTest $3, [$1]) }
  | expression0 TILDE IDENT                                     { PrimApp (PrimSetModify $3, [$1]) }
;

/* Auxiliary definitions. */

label_list:
    /* epsilon */					        { [] }
  | IDENT       					        { [$1] }
  | IDENT SEMI label_list                		        { $1 :: $3 }
;

pattern_list:
    /* epsilon */					        { [] }
  | pattern pattern_list                   		        { $1 :: $2 }
;

pattern:
    IDENT                                                       { PVariable $1 }
  | UNDERSCORE                                                  { PVariable "-" }
  | UNIT                                                        { PUnit }
;

