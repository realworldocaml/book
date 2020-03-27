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

/* $Id: promelaparser_withexps.mly,v 1.2 2008-10-02 13:33:29 uid588 Exp $ */

/* Originated from http://www.ltl2dstar.de/down/ltl2dstar-0.4.2.zip  */
%{
open Logic_ptree
open Promelaast
open Bool3

let to_seq c =
  [{ condition = Some c;
     nested = [];
     min_rep = Some Data_for_aorai.cst_one;
     max_rep = Some Data_for_aorai.cst_one;
   }]

let is_no_repet (min,max) =
  let is_one c = Extlib.may_map Data_for_aorai.is_cst_one ~dft:false c in
  is_one min && is_one max

let observed_states      = Hashtbl.create 1
let prefetched_states    = Hashtbl.create 1

let fetch_and_create_state name =
  Hashtbl.remove prefetched_states name ;
  try
    Hashtbl.find observed_states name
  with
    Not_found ->
      let s = Data_for_aorai.new_state name in
      Hashtbl.add observed_states name s; s
;;

let prefetch_and_create_state name =
    if (Hashtbl.mem prefetched_states name) ||
      not (Hashtbl.mem observed_states name)
    then
      begin
	let s= fetch_and_create_state name in
	Hashtbl.add prefetched_states name name;
	s
      end
    else
      (fetch_and_create_state name)
;;

type pre_cond = Behavior of string | Pre of Promelaast.condition

%}

%token CALL_OF  RETURN_OF  CALLORRETURN_OF
%token <string> IDENTIFIER
%token <string> INT
%token LCURLY RCURLY LPAREN RPAREN LSQUARE RSQUARE LBRACELBRACE RBRACERBRACE
%token RARROW
%token TRUE FALSE
%token NOT DOT AMP
%token COLON SEMI_COLON COMMA PIPE CARET QUESTION COLUMNCOLUMN
%token EQ LT GT LE GE NEQ PLUS MINUS SLASH STAR PERCENT OR AND
%token OTHERWISE
%token EOF

%nonassoc highest
%left LPAREN RPAREN
%left LCURLY
%right EQ LT GT LE GE NEQ PLUS MINUS SLASH STAR PERCENT OR AND
/* [VP] priorities taken from cparser.mly */
%left LSQUARE RSQUARE
%left DOT
%nonassoc NOT TRUE FALSE
%nonassoc QUESTION
%right SEMICOLON
%nonassoc lowest

%type <Promelaast.parsed_automaton> main
%start main
%%

main
  : options states {
  List.iter
    (fun(key, ids) ->
       match key with
           "init"   ->
             List.iter
               (fun id ->
                 try
	           (Hashtbl.find observed_states id).init <- True
                 with
	             Not_found ->
                       Aorai_option.abort "Error: no state '%s'\n" id)
               ids
         | "accept" ->
             List.iter
               (fun id -> try
	          (Hashtbl.find observed_states id).acceptation <- True
                with Not_found ->
                  Aorai_option.abort "no state '%s'\n" id) ids
         | "deterministic" -> Aorai_option.Deterministic.set true;
         | oth      -> Aorai_option.abort "unknown option '%s'\n" oth
    ) $1;
    let states=
      Hashtbl.fold
        (fun _ st l ->
	   if st.acceptation=Undefined || st.init=Undefined then
	     begin
	       Aorai_option.abort
                 "Error: the state '%s' is used but never defined.\n" st.name
	     end;
	   st::l)
        observed_states []
    in
    (try
       Hashtbl.iter
         (fun _ st -> if st.init=True then raise Exit) observed_states;
       Aorai_option.abort "Automaton does not declare an initial state"
     with Exit -> ());
    if Hashtbl.length prefetched_states >0 then
      begin
	let r = Hashtbl.fold
	  (fun s n _ ->
            s^"Error: the state '"^n^"' is used but never defined.\n")
	  prefetched_states
	  ""
	in
	Aorai_option.abort "%s" r
      end;
    (states, $2)
  }
  ;


options
  : options option { $1@[$2] }
  | option         { [$1] }
  ;

option
  : PERCENT IDENTIFIER opt_identifiers SEMI_COLON { ($2, $3) }
  ;

opt_identifiers
  : /* empty */ { [] }
  | COLON id_list { $2 }
  ;

id_list
  : id_list COMMA IDENTIFIER { $1@[$3] }
  | IDENTIFIER               { [$1] }
  ;

states
  : states state { $1@$2 }
  | state { $1 }
  ;

state
  : IDENTIFIER COLON transitions SEMI_COLON {
      let start_state = fetch_and_create_state $1 in
      let (_, transitions) =
        List.fold_left
          (fun (otherwise, transitions) (cross,stop_state) ->
            if otherwise then
              Aorai_option.abort
                "'other' directive in definition of %s \
                transitions is not the last one" start_state.name
            else begin
              let trans =
                { start=start_state; stop=stop_state;
	          cross=cross;       numt=(-1) }::transitions
              in
              let otherwise =
                match cross with
                  | Otherwise -> true
                  | Seq _ -> false
              in otherwise, trans
            end)
          (false,[]) $3
      in
      List.rev transitions
  }

transitions  /*=>  [transition; ...] */
  : transitions PIPE transition { $1@[$3] }
  | transition { [$1] }
  ;


transition:  /*=>  (guard, state) */
  | LCURLY seq_elt RCURLY RARROW IDENTIFIER
      { (Seq $2, prefetch_and_create_state $5) }
  | OTHERWISE RARROW IDENTIFIER {(Otherwise, prefetch_and_create_state $3) }
  | RARROW IDENTIFIER { (Seq (to_seq PTrue), prefetch_and_create_state $2) }
  ;

non_empty_seq:
  | seq_elt { $1 }
  | seq_elt SEMI_COLON seq { $1 @ $3 }
;

seq:
  | /* epsilon */ { [] }
  | non_empty_seq { $1 }
;

guard:
  | single_cond { to_seq $1 }
  | LSQUARE non_empty_seq RSQUARE { $2 }
  | IDENTIFIER pre_cond LPAREN seq RPAREN post_cond
      { let pre_cond =
          match $2 with
            | Behavior b -> PCall($1,Some b)
            | Pre c -> PAnd (PCall($1,None), c)
        in
        let post_cond =
          match $6 with
            | None -> PReturn $1
            | Some c -> PAnd (PReturn $1,c)
        in
        (to_seq pre_cond) @ $4 @ to_seq post_cond
      }
  | IDENTIFIER LPAREN non_empty_seq RPAREN post_cond
      { let post_cond =
          match $5 with
            | None -> PReturn $1
            | Some c -> PAnd (PReturn $1,c)
        in
        (to_seq (PCall ($1, None))) @ $3 @ to_seq post_cond
      }
  | IDENTIFIER LPAREN RPAREN post_cond
      { let post_cond =
          match $4 with
            | None -> PReturn $1
            | Some c -> PAnd (PReturn $1,c)
        in
        (to_seq (PCall ($1, None))) @ to_seq post_cond
      }
;

pre_cond:
  | COLUMNCOLUMN IDENTIFIER { Behavior $2 }
  | LBRACELBRACE single_cond RBRACERBRACE { Pre $2 }
;

post_cond:
  | /* epsilon */ { None }
  | LBRACELBRACE single_cond RBRACERBRACE { Some $2 }
;

seq_elt:
  | guard repetition {
    let min, max = $2 in
    match $1 with
      | [ s ] when Data_for_aorai.is_single s ->
        [ { s with min_rep = min; max_rep = max } ]
      | l ->
        if is_no_repet (min,max) then
          l (* [ a; [b;c]; d] is equivalent to [a;b;c;d] *)
        else [ { condition = None; nested = l; min_rep = min; max_rep = max } ]
  }
;

repetition:
  | /* empty */ %prec lowest
      { Some Data_for_aorai.cst_one, Some Data_for_aorai.cst_one }
  | PLUS { Some Data_for_aorai.cst_one, None}
  | STAR { None, None }
  | QUESTION { None, Some Data_for_aorai.cst_one }
  | LCURLY arith_relation COMMA arith_relation RCURLY { Some $2, Some $4 }
  | LCURLY arith_relation RCURLY { Some $2, Some $2 }
  | LCURLY arith_relation COMMA RCURLY { Some $2, None }
  | LCURLY COMMA arith_relation RCURLY { None, Some $3 }

single_cond:
  | CALLORRETURN_OF  LPAREN IDENTIFIER RPAREN
      { POr (PCall ($3,None), PReturn $3) }
  | CALL_OF  LPAREN IDENTIFIER RPAREN { PCall ($3,None) }
  | RETURN_OF  LPAREN IDENTIFIER RPAREN { PReturn $3 }
  | TRUE { PTrue }
  | FALSE { PFalse }
  | NOT single_cond { PNot $2 }
  | single_cond AND single_cond { PAnd ($1,$3) }
  | single_cond OR single_cond { POr ($1,$3) }
  | LPAREN single_cond RPAREN { $2 }
  | logic_relation { $1 }
;

logic_relation
  : arith_relation EQ arith_relation { PRel(Eq, $1, $3) }
  | arith_relation LT arith_relation { PRel(Lt, $1, $3) }
  | arith_relation GT arith_relation { PRel(Gt, $1, $3) }
  | arith_relation LE arith_relation { PRel(Le, $1, $3) }
  | arith_relation GE arith_relation { PRel(Ge, $1, $3) }
  | arith_relation NEQ arith_relation { PRel(Neq, $1, $3) }
  | arith_relation %prec TRUE { PRel (Neq, $1, PCst(IntConstant "0")) }
  ;

arith_relation
  : arith_relation_mul PLUS arith_relation { PBinop(Badd,$1,$3) }
  | arith_relation_mul MINUS arith_relation { PBinop(Bsub,$1,$3) }
  | arith_relation_mul %prec lowest { $1 }
  ;

arith_relation_mul
  : arith_relation_mul SLASH access_or_const { PBinop(Bdiv,$1,$3) }
  | arith_relation_mul STAR access_or_const { PBinop(Bmul, $1, $3) }
  | arith_relation_mul PERCENT access_or_const { PBinop(Bmod, $1, $3) }
  | arith_relation_bw %prec lowest { $1 }
  ;

arith_relation_bw
  : access_or_const %prec lowest { $1 }
  | arith_relation_bw AMP access_or_const { PBinop(Bbw_and,$1,$3) }
  | arith_relation_bw PIPE access_or_const { PBinop(Bbw_or,$1,$3) }
  | arith_relation_bw CARET access_or_const { PBinop(Bbw_xor,$1,$3) }

/* returns a Lval exp or a Const exp*/
access_or_const
  : INT { PCst (IntConstant $1) }
  | MINUS INT { PUnop (Uminus, PCst (IntConstant $2)) }
  | access %prec TRUE { $1 }
  | LPAREN arith_relation RPAREN { $2 }
  ;

/* returns a lval */
access
  : access DOT IDENTIFIER { PField($1,$3) }
  | access LSQUARE access_or_const RSQUARE { PArrget($1,$3) }
  | access_leaf	    {$1}
  ;

access_leaf
  : STAR access { PUnop (Ustar,$2) }
  | IDENTIFIER LPAREN RPAREN DOT IDENTIFIER { PPrm($1,$5) }
  | IDENTIFIER { PVar $1 }
  | LPAREN access RPAREN { $2 }
  ;
