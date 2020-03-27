(* Original file: proverif.2.00/proverif2.00/src/parser.mly *)
%{
(*************************************************************
 *                                                           *
 *  Cryptographic protocol verifier                          *
 *                                                           *
 *  Bruno Blanchet, Vincent Cheval, and Marc Sylvestre       *
 *                                                           *
 *  Copyright (C) INRIA, CNRS 2000-2018                      *
 *                                                           *
 *************************************************************)

(*

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details (in file LICENSE).

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*)
%}
%{

open Parsing_helper
open Ptree
exception Syntax

%}

%token COMMA
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token SEMI
%token COLON
%token <Ptree.ident> IDENT
%token <int> INT
%token RED
%token EQUIV
%token EQUIVEQ
%token EQUAL
%token FUN
%token EQUATION
%token QUERY
%token NOUNIF
%token SLASH
%token STAR
%token DOT
%token WEDGE
%token EOF
%token NOT
%token ELIMTRUE
%token DIFF
%token PREDICATE

/* untyped front-end only */
%token REDUCTION
%token DATA
%token PARAM

/*typed front-end only*/
%token CLAUSES
%token CONST
%token SET
%token NAME
%token TYPE
%token FORALL

%left DIFF
%left WEDGE

/*untyped front-end*/
%start all
%type <Ptree.decl list> all
/*typed front-end*/
%start tall
%type <Ptree.tdecl list> tall


%%

term:
	IDENT LPAREN termseq RPAREN
	{ PFunApp ($1, $3) }
|	LPAREN termseq RPAREN
	{ PTuple $2 }
| 	IDENT LBRACKET termseq RBRACKET
	{ PName($1, $3) }
|	IDENT
	{ PIdent ($1) }

netermseq:
	term COMMA netermseq
	{ $1 :: $3 }
|	term 
	{ [$1] }

termseq:
        netermseq
        { $1 }
|	
	{ [] }

neidentseq:
        IDENT COMMA neidentseq
        { $1 :: $3 }
|	IDENT
	{ [$1] }

format:
	IDENT LPAREN formatseq RPAREN
	{ PFFunApp ($1, $3) }
|	LPAREN formatseq RPAREN
	{ PFTuple $2 }
| 	IDENT LBRACKET formatseq RBRACKET
	{ PFName($1, $3) }
|	IDENT
	{ PFIdent ($1) }
|       STAR IDENT
        { PFAny ($2) }

neformatseq:
	format COMMA neformatseq
	{ $1 :: $3 }
|	format 
	{ [$1] }
    
formatseq:
        neformatseq
        { $1 }
|	
	{ [] }

optint:
        SLASH INT
        { $2 }
| 
        { -1 }

identseq:
        neidentseq
        { $1 }
| 
        { [] }

/*untyped front-end*/

fact:
	IDENT COLON termseq
	{ PSimpleFact($1,$3), parse_extent() }
|       term DIFF term
        { PSNeq($1,$3), parse_extent() }

rule: 
        termand RED fact
        { Clause($1,$3) }
|       fact
        { Clause([],$1) }
|       termand EQUIV fact
        { Equiv($1,$3,true) }
|       termand EQUIVEQ fact
        { Equiv($1,$3,false) }

termand:
	fact WEDGE termand
	{ $1 :: $3 }
|	fact
	{ [$1] }

reduc:
	rule SEMI reduc
	{ $1 :: $3 }
|	rule DOT
	{ [$1] }
  
factformat:
  	IDENT COLON formatseq
	{ ($1,$3) }

/* Equations */

eqlist:
        term EQUAL term
        { [($1, $3)] }
|       term EQUAL term SEMI eqlist
	{ ($1, $3) :: $5 }

all:
        FUN IDENT SLASH INT DOT all
        { (FunDecl($2, $4)) :: $6 }
|       DATA IDENT SLASH INT DOT all
        { (DataFunDecl($2, $4)) :: $6 }
|       EQUATION eqlist DOT all
        { (Equation($2)) :: $4 }
|       QUERY fact DOT all
        { (Query $2) :: $4 } 
|       NOUNIF factformat optint DOT all
        { (NoUnif ($2,$3)) :: $5 } 
|       NOT fact DOT all
        { (Not $2) :: $4 } 
|       ELIMTRUE fact DOT all
        { (Elimtrue $2) :: $4 } 
|       PREDICATE IDENT SLASH INT identseq DOT all
        { (PredDecl($2, $4, $5)) :: $7 }
|       PARAM IDENT EQUAL IDENT DOT all
        { (Param($2,S $4)) :: $6 }
|       PARAM IDENT EQUAL INT DOT all
        { (Param($2,I $4)) :: $6 }
|	REDUCTION reduc EOF
	{ [Reduc($2)] }

/* typed front-end */

tfact:
	IDENT LPAREN termseq RPAREN
	{ PSimpleFact($1,$3), parse_extent() }
|       IDENT
        { PSimpleFact($1,[]), parse_extent() }
|       term DIFF term
        { PSNeq($1,$3), parse_extent() }

trule: 
        ttermand RED tfact
        { Clause($1,$3) }
|       tfact
        { Clause([],$1) }
|       ttermand EQUIV tfact
        { Equiv($1,$3,true) }
|       ttermand EQUIVEQ tfact
        { Equiv($1,$3,false) }

ttermand:
	tfact WEDGE ttermand
	{ $1 :: $3 }
|	tfact
	{ [$1] }

tfactformat:
  	IDENT LPAREN formatseq RPAREN
	{ ($1,$3) }

nevartype:
        IDENT COLON IDENT COMMA nevartype
        { ($1,$3)::$5 }
|
        IDENT COLON IDENT 
        { [($1,$3)] }

forallvartype:
        FORALL nevartype SEMI
        { $2 }
| 
        { [] }

treduc:
	forallvartype trule SEMI treduc
	{ ($1,$2) :: $4 }
|	forallvartype trule DOT
	{ [$1,$2] }
  
options:
        LBRACKET neidentseq RBRACKET
        { $2 }
| 
        { [] }

/* Equations */

teqlist:
    forallvartype term EQUAL term 
    { [($1, $2, $4)] }
|   forallvartype term EQUAL term SEMI teqlist
    { ($1, $2, $4)::$6 }

tall:
        TYPE IDENT DOT tall
        { TTypeDecl($2) :: $4 }
|       NAME IDENT COLON IDENT DOT tall
        { TNameDecl($2,$4) :: $6 }
|       FUN IDENT LPAREN identseq RPAREN COLON IDENT options DOT tall
        { (TFunDecl($2, $4, $7, $8)) :: $10 }
|       CONST IDENT COLON IDENT options DOT tall
        { (TConstDecl($2, $4, $5)) :: $7 }
|       EQUATION options teqlist DOT tall 
        /* I put the options first to avoid a shift/reduce conflict
	   between equation ... = n[terms]. 
	   and     equation ... = x [convergent]. */
        { (TEquation($3, $2)) :: $5 }
|       QUERY nevartype SEMI tfact DOT tall
        { (TQuery($2, $4)) :: $6 } 
|       QUERY tfact DOT tall
        { (TQuery([], $2)) :: $4 } 
|       NOUNIF nevartype SEMI tfactformat optint DOT tall
        { (TNoUnif ($2, $4,$5)) :: $7 } 
|       NOUNIF tfactformat optint DOT tall
        { (TNoUnif ([],$2,$3)) :: $5 } 
|       NOT nevartype SEMI tfact DOT tall
        { (TNot($2,$4)) :: $6 } 
|       NOT tfact DOT tall
        { (TNot([],$2)) :: $4 } 
|       ELIMTRUE nevartype SEMI tfact DOT tall
        { (TElimtrue($2,$4)) :: $6 } 
|       ELIMTRUE tfact DOT tall
        { (TElimtrue([],$2)) :: $4 } 
|       PREDICATE IDENT LPAREN identseq RPAREN options DOT tall
        { (TPredDecl($2, $4, $6)) :: $8 }
|       PREDICATE IDENT options DOT tall
        { (TPredDecl($2, [], $3)) :: $5 }
|       SET IDENT EQUAL IDENT DOT tall
        { (TSet($2,S $4)) :: $6 }
|       SET IDENT EQUAL INT DOT tall
        { (TSet($2,I $4)) :: $6 }
|	CLAUSES treduc EOF
	{ [TReduc($2)] }
