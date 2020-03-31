/**************************************************************************/
/*                                                                        */
/*  This file is part of Aorai plug-in of Frama-C.                        */
/*                                                                        */
/*  Copyright (C) 2007-2015                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*    INRIA (Institut National de Recherche en Informatique et en         */
/*           Automatique)                                                 */
/*    INSA  (Institut National des Sciences Appliquees)                   */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

/* $Id: ltlparser.mly,v 1.3 2009-02-13 07:59:29 uid562 Exp $ */

/* Originated from http://www.ltl2dstar.de/down/ltl2dstar-0.4.2.zip  */
%{
open Promelaast
open Logic_ptree

let observed_expressions=Hashtbl.create 97

let ident_count=ref 0
let get_fresh_ident () =
  ident_count:=!ident_count+1;
  ("buchfreshident"^(string_of_int !ident_count))
%}


%token LTL_TRUE LTL_FALSE LTL_LPAREN LTL_RPAREN

/* Logic operators */
%token LTL_OR LTL_IMPLIES LTL_LEFT_RIGHT_ARROW
%token LTL_AND
%token LTL_NOT
%token LTL_GLOBALLY LTL_FATALLY LTL_UNTIL LTL_RELEASE LTL_NEXT

%right LTL_OR LTL_IMPLIES LTL_LEFT_RIGHT_ARROW
%right LTL_AND
%nonassoc LTL_NOT
%right LTL_GLOBALLY LTL_FATALLY LTL_UNTIL LTL_RELEASE LTL_NEXT


/* Logic relations */
%token LTL_EQ LTL_LT LTL_GT LTL_LE LTL_GE LTL_NEQ
%right LTL_EQ LTL_LT LTL_GT LTL_LE LTL_GE LTL_NEQ


/* Arithmetic relations */
%token LTL_PLUS LTL_MINUS
%token LTL_DIV LTL_STAR LTL_MODULO
%right LTL_PLUS LTL_MINUS LTL_DIV LTL_STAR LTL_MODULO


/* Access */
%token LTL_RIGHT_ARROW LTL_DOT LTL_LEFT_SQUARE LTL_RIGHT_SQUARE LTL_ADRESSE
%token LTL_CALL LTL_RETURN LTL_CALL_OR_RETURN

/* Variables and constants */
%token <string> LTL_INT
%token <string> LTL_LABEL

/* Others */
%token EOF


%type <(Ltlast.formula * (string, (Logic_ptree.relation *  Promelaast.expression * Promelaast.expression)) Hashtbl.t)> ltl
%start ltl
%%

ltl
        : formula EOF {($1,observed_expressions)}
  ;


formula
        : LTL_TRUE
            {Ltlast.LTrue}
        | LTL_FALSE
	    {Ltlast.LFalse}
	| LTL_LPAREN formula LTL_RPAREN
	    { $2 }

	| LTL_GLOBALLY formula
	    { Ltlast.LGlobally($2) }
	| LTL_FATALLY  formula
	    { Ltlast.LFatally($2) }
	| formula LTL_UNTIL formula
	    { Ltlast.LUntil($1,$3) }
	| formula LTL_RELEASE formula
	    { Ltlast.LRelease($1,$3) }
	| LTL_NEXT formula
	    { Ltlast.LNext($2) }

	| formula LTL_OR formula
	    { Ltlast.LOr($1,$3) }
	| formula LTL_AND formula
	    { Ltlast.LAnd($1,$3) }
	| LTL_NOT formula
	    { Ltlast.LNot($2) }
	| formula LTL_IMPLIES formula
	    { Ltlast.LImplies($1,$3) }
	| formula LTL_LEFT_RIGHT_ARROW formula
	    { Ltlast.LIff($1,$3) }

	| LTL_CALL LTL_LPAREN LTL_LABEL LTL_RPAREN
	    { Ltlast.LCall($3)}
	| LTL_RETURN LTL_LPAREN LTL_LABEL LTL_RPAREN
	    { Ltlast.LReturn($3)}
	| LTL_CALL_OR_RETURN LTL_LPAREN LTL_LABEL LTL_RPAREN
	    { Ltlast.LCallOrReturn($3)}

/* returns a string identifer associated, through observed_expressions table, to the represented expression */
	| logic_relation
	    {
	      let id = get_fresh_ident () in
	        Hashtbl.add observed_expressions id $1;
	        Ltlast.LIdent(id)
	    }
  ;

logic_relation
	: arith_relation LTL_EQ  arith_relation { Eq, $1 , $3}
	| arith_relation LTL_LT  arith_relation { Lt, $1, $3 }
	| arith_relation LTL_GT  arith_relation { Gt, $1, $3 }
	| arith_relation LTL_LE  arith_relation { Le, $1, $3 }
	| arith_relation LTL_GE  arith_relation { Ge, $1, $3 }
	| arith_relation LTL_NEQ arith_relation { Neq, $1, $3 }
	| arith_relation { Neq, $1, PCst (IntConstant "0") }
  ;

arith_relation
        : arith_relation_mul LTL_PLUS arith_relation { PBinop(Badd,$1,$3) }
	| arith_relation_mul LTL_MINUS arith_relation { PBinop(Bsub,$1,$3) }
	| arith_relation_mul { $1 }
  ;


arith_relation_mul
	: arith_relation_mul LTL_DIV access_or_const { PBinop(Bdiv,$1,$3) }
	| arith_relation_mul LTL_STAR access_or_const { PBinop(Bmul,$1,$3) }
	| arith_relation_mul LTL_MODULO access_or_const { PBinop(Bmod,$1,$3)}
	| access_or_const { $1 }
  ;

/* returns a Lval exp or a Const exp*/
access_or_const
        : LTL_INT { PCst (IntConstant $1) }
        | LTL_MINUS LTL_INT { PUnop (Uminus,PCst (IntConstant $2)) }
	| access { $1 }
	| LTL_LPAREN arith_relation LTL_RPAREN { $2 }
  ;


/* returns a lval */
access
	: access LTL_RIGHT_ARROW LTL_LABEL { PField (PUnop(Ustar,$1),$3) }
	| access LTL_DOT LTL_LABEL { PField($1,$3) }
	| access_array {$1}

access_array
	: access_array LTL_LEFT_SQUARE access_or_const LTL_RIGHT_SQUARE
	    { PArrget($1,$3) }
    	| access_leaf {$1}


access_leaf
        : LTL_ADRESSE access { PUnop (Uamp,$2) }
	| LTL_STAR access { PUnop (Ustar, $2 ) }
	| LTL_LABEL { PVar $1 }
	| LTL_LPAREN access LTL_RPAREN { $2 }

  ;
