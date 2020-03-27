(* Original file: KaSim.4.0.0/KaSim-4.0/grammar/kparser4.mly *)
/******************************************************************************/
/*  _  __ * The Kappa Language                                                */
/* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  */
/* | ' /  *********************************************************************/
/* | . \  * This file is distributed under the terms of the                   */
/* |_|\_\ * GNU Lesser General Public License Version 3                       */
/******************************************************************************/

%{
  let add_pos e x =
    (x,
    Locality.of_pos (Parsing.symbol_start_pos ()) (Parsing.rhs_end_pos e))
  let rhs_pos i =
    Locality.of_pos (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i)
  let end_pos = Parsing.rhs_end_pos
  let start_pos = Parsing.rhs_start_pos

  let internal_memory = ref []
  let add x = internal_memory := x :: !internal_memory
  let output () =
    let o = List.rev !internal_memory in let () = internal_memory := [] in o
%}

%token EOF COMMA DOT OP_PAR CL_PAR OP_CUR CL_CUR OP_BRA CL_BRA AT SEMICOLON LOG
%token PLUS MINUS MULT DIV MOD MAX MIN SINUS COSINUS TAN POW ABS SQRT EXPONENT
%token OR AND NOT THEN ELSE DIFF EQUAL SMALLER GREATER TRUE FALSE INFINITY
%token SHARP UNDERSCORE PIPE RAR LRAR EMAX TMAX CPUTIME TIME EVENT NULL_EVENT
%token COLON NEWLINE SIGNATURE TOKEN INIT LET OBS PLOT PERT CONFIG RUN APPLY
%token DELETE INTRO SNAPSHOT STOP FLUX TRACK ASSIGN PRINTF PLOTENTRY SPECIES_OF
%token DO REPEAT ALARM
%token <int> INT
%token <float> FLOAT
%token <string> ID LABEL STRING
%token <string> SPACE COMMENT

%start model
%type <Ast.parsing_instruction list> model

%start interactive_command
%type <(Ast.mixture,Ast.mixture,string,Ast.rule) Ast.command> interactive_command

%start standalone_effect_list
%type
  <(Ast.mixture,Ast.mixture,string,Ast.rule) Ast.modif_expr list> standalone_effect_list

%start standalone_bool_expr
%type <(Ast.mixture,string) Alg_expr.bool Locality.annot> standalone_bool_expr

%%

annot:
  | { [] }
  | NEWLINE annot { "
"::$2 }
  | SPACE annot { $1::$2 }
  | COMMENT annot { $1::$2 }
  ;

nbr:
  | INFINITY { Nbr.F infinity }
  | FLOAT { Nbr.F $1 }
  | INT { Nbr.I $1 }
  ;

link_state:
  | DOT { add_pos 1 Ast.LNK_FREE }
  | INT { add_pos 1 (Ast.LNK_VALUE ($1,())) }
  | UNDERSCORE { add_pos 1 Ast.LNK_SOME }
  | ID annot DOT annot ID
    { add_pos 5 (Ast.LNK_TYPE (($1,rhs_pos 1),($5,rhs_pos 5))) }
  | SHARP { add_pos 1 Ast.LNK_ANY }
  | ID annot error
    { raise (ExceptionDefn.Syntax_Error (add_pos 3 "incomplete link state")) }
  ;

link_states:
  | link_state annot { [$1] }
  | link_state annot link_states { $1 :: $3 }
  | link_state annot COMMA annot link_states { $1 :: $5 }
  ;

link_modif:
  | { None }
  | DIV annot DOT annot { Some None }
  | DIV annot INT annot { Some (Some ($3, rhs_pos 3)) }
  | DIV annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3 "incomplete link modification")) }
  ;

internal_state:
  | ID { add_pos 1 (Some $1) }
  | SHARP { add_pos 1 None }
  ;

internal_states:
  | internal_state annot { [$1] }
  | internal_state annot internal_states { $1 :: $3 }
  | internal_state annot COMMA annot internal_states { $1 :: $5 }
  ;

internal_modif:
  | { None }
  | DIV annot ID annot { Some ($3, rhs_pos 3) }
  | DIV annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3 "incomplete link modification")) }
  ;

site_link:
  | annot link_states link_modif CL_BRA { ($2, $3) }
  | annot error
    { raise (ExceptionDefn.Syntax_Error
               ("invalid linking state or missing ']'",rhs_pos 4)) }
  ;

site_internal:
  | internal_states internal_modif CL_CUR { ($1, $2) }
  | error
    { raise (ExceptionDefn.Syntax_Error
               ("invalid internal state or missing '}'",rhs_pos 3)) }
  ;

counter_modif:
  | PLUS annot EQUAL annot INT { ($5, rhs_pos 5) }
  | PLUS annot EQUAL annot MINUS annot INT { (- $7, rhs_pos 7) }
  | MINUS annot EQUAL annot INT { (- $5, rhs_pos 5) }
  ;

counter_test:
  | EQUAL annot INT { (Ast.CEQ $3,rhs_pos 3) }
  | GREATER annot EQUAL annot INT { (Ast.CGTE $5,rhs_pos 5) }
  | EQUAL annot ID { (Ast.CVAR $3,rhs_pos 3) }
  ;

site_counter:
  | counter_modif annot CL_CUR annot { (None, $1) }
  | counter_test annot CL_CUR annot { (Some $1, Locality.dummy_annot 0) }
  | counter_test annot DIV annot counter_modif annot CL_CUR annot
    { (Some $1,$5) }
  ;

site:
  | ID annot OP_BRA site_link annot OP_CUR annot site_internal annot
    { let (port_lnk, port_lnk_mod) = $4 in
      let (port_int, port_int_mod) = $8 in
      Ast.Port
        { Ast.port_nme=($1,rhs_pos 1); Ast.port_int;
          Ast.port_lnk; Ast.port_int_mod; Ast.port_lnk_mod; } }
  | ID annot OP_CUR annot site_internal annot OP_BRA site_link annot
    { let (port_int, port_int_mod) = $5 in
      let (port_lnk, port_lnk_mod) = $8 in
      Ast.Port
        { Ast.port_nme=($1,rhs_pos 1); Ast.port_int;
          Ast.port_lnk; Ast.port_int_mod; Ast.port_lnk_mod; } }
  | ID annot OP_BRA site_link annot
    { let (port_lnk, port_lnk_mod) = $4 in
      Ast.Port
        { Ast.port_nme=($1,rhs_pos 1); Ast.port_int=[];
          Ast.port_lnk; Ast.port_int_mod=None; Ast.port_lnk_mod; } }
  | ID annot OP_CUR annot site_internal annot
    { let (port_int, port_int_mod) = $5 in
      Ast.Port
        { Ast.port_nme=($1,rhs_pos 1);Ast.port_lnk=[];
          Ast.port_int; Ast.port_int_mod; Ast.port_lnk_mod=None; } }
  | ID annot OP_CUR annot site_counter
    { let (count_test,count_delta) = $5 in
      Ast.Counter
        { Ast.count_nme=($1,rhs_pos 1); Ast.count_test; Ast.count_delta } }
  | ID annot
    { Ast.Port
        { Ast.port_nme=($1,rhs_pos 1);Ast.port_lnk=[]; Ast.port_int=[];
          Ast.port_int_mod=None; Ast.port_lnk_mod=None; } }
  ;

interface:
  | { [] }
  | error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 1 ("Malformed site expression"))) }
  | site interface { $1 :: $2 }
  | site COMMA annot interface { $1 :: $4 }
  ;

agent_modif:
  | annot { None,start_pos 1,$1 }
  | annot PLUS annot { Some Ast.Create,end_pos 2,$3 }
  | annot MINUS annot { Some Ast.Erase,end_pos 2,$3 }
  ;

agent:
  | DOT annot { (Ast.Absent (rhs_pos 1),end_pos 1,$2) }
  | ID annot OP_PAR annot interface CL_PAR agent_modif
    { let modif,pend,an = $7 in
      (Ast.Present (($1,rhs_pos 1), $5, modif),pend,an) }
  | ID annot COLON annot ID annot OP_PAR annot interface CL_PAR agent_modif
    { let modif,pend,an = $11 in
      (Ast.Present (($5,rhs_pos 5), $9, modif),pend,an) }
  | ID annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3 ("Malformed agent '"^$1^"'"))) }
  ;

pattern:
  | agent COMMA annot pattern
    { let (x,_,_) = $1 in let (y,pend,p) = $4 in (x::y,pend,p) }
  | agent { let (x,pend,p) = $1 in ([x],pend,p) }
  ;

constant:
  | nbr { add_pos 1 (Alg_expr.CONST $1) }
  | EMAX { add_pos 1 (Alg_expr.STATE_ALG_OP (Operator.EMAX_VAR)) }
  | TMAX { add_pos 1 (Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR)) }
  | CPUTIME { add_pos 1 (Alg_expr.STATE_ALG_OP (Operator.CPUTIME)) }
  ;

variable:
  | PIPE annot ID annot PIPE { add_pos 5 (Alg_expr.TOKEN_ID ($3)) }
  | PIPE annot pattern PIPE
    { let (p,_,_) = $3 in add_pos 4 (Alg_expr.KAPPA_INSTANCE p) }
  | ID { add_pos 1 (Alg_expr.ALG_VAR ($1)) }
  | LABEL { add_pos 1 (Alg_expr.ALG_VAR ($1)) }
  | TIME { add_pos 1 (Alg_expr.STATE_ALG_OP (Operator.TIME_VAR)) }
  | EVENT { add_pos 1 (Alg_expr.STATE_ALG_OP (Operator.EVENT_VAR)) }
  | NULL_EVENT
    { add_pos 1 (Alg_expr.STATE_ALG_OP (Operator.NULL_EVENT_VAR)) }
  ;

small_alg_expr:
  | OP_PAR annot alg_expr CL_PAR { let (x,_,_) = $3 in x }
  | constant { $1 }
  | variable { $1 }
  | MAX annot small_alg_expr annot small_alg_expr
    { add_pos 5 (Alg_expr.BIN_ALG_OP(Operator.MAX,$3,$5)) }
  | MIN annot small_alg_expr annot small_alg_expr
    { add_pos 5 (Alg_expr.BIN_ALG_OP(Operator.MIN,$3,$5)) }
  | EXPONENT annot small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.EXP,$3)) }
  | SINUS annot small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.SINUS,$3)) }
  | COSINUS annot small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.COSINUS,$3)) }
  | TAN annot small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.TAN,$3)) }
  | ABS annot small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.INT,$3)) }
  | SQRT annot small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.SQRT,$3)) }
  | LOG annot small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.LOG,$3)) }
  | MINUS annot small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.UMINUS,$3)) }
  ;

alg_expr_up_to_mod:
  | small_alg_expr annot { ($1,end_pos 1,$2) }
  | small_alg_expr annot POW annot alg_expr_up_to_mod
    { let (x,y,z) = $5 in
      (add_pos 4 (Alg_expr.BIN_ALG_OP(Operator.POW,$1,x)),y,z) }
  ;

alg_expr_up_to_prod:
  | alg_expr_up_to_mod { $1 }
  | alg_expr_up_to_prod MOD annot alg_expr_up_to_mod
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_ALG_OP (Operator.MODULO,x,y),
        Locality.of_pos (start_pos 1) pend),
       pend,an) }
  ;

alg_expr_up_to_sum:
  | alg_expr_up_to_prod { $1 }
  | alg_expr_up_to_sum MULT annot alg_expr_up_to_prod
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_ALG_OP(Operator.MULT,x,y),
       Locality.of_pos (start_pos 1) pend),
       pend,an) }
  | alg_expr_up_to_sum DIV annot alg_expr_up_to_prod
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_ALG_OP(Operator.DIV,x,y),
        Locality.of_pos (start_pos 1) pend),
       pend,an) }
  ;

alg_expr_up_to_if:
  | alg_expr_up_to_sum { $1 }
  | alg_expr_up_to_if PLUS annot alg_expr_up_to_sum
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_ALG_OP(Operator.SUM,x,y),
        Locality.of_pos (start_pos 1) pend),
       pend,an) }
  | alg_expr_up_to_if MINUS annot alg_expr_up_to_sum
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_ALG_OP(Operator.MINUS,x,y),
        Locality.of_pos (start_pos 1) pend),
       pend,an) }

alg_expr:
  | alg_expr_up_to_if { $1 }
  | bool_expr THEN annot alg_expr ELSE annot small_alg_expr annot
    { let (i,_,_) = $1 in
      let (t,_,_) = $4 in
  ((Alg_expr.IF(i,t,$7),
    Locality.of_pos (start_pos 1) (end_pos 7)),end_pos 7,$8) }
  ;

boolean:
  | TRUE { true }
  | FALSE { false }
  ;

small_bool_expr:
  | OP_PAR annot bool_expr CL_PAR { let (x,_,_) = $3 in x }
  | TRUE { add_pos 1 Alg_expr.TRUE }
  | FALSE { add_pos 1 Alg_expr.FALSE }
  | NOT annot small_bool_expr
    { add_pos 3 (Alg_expr.UN_BOOL_OP(Operator.NOT,$3)) }
  ;

bool_expr_comp:
  | small_bool_expr annot { ($1,end_pos 1, $2) }
  | alg_expr_up_to_if GREATER annot alg_expr
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.COMPARE_OP(Operator.GREATER,x,y),
        Locality.of_pos (start_pos 1) pend),
       pend,an) }
  | alg_expr_up_to_if SMALLER annot alg_expr
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.COMPARE_OP(Operator.SMALLER,x,y),
        Locality.of_pos (start_pos 1) pend),
       pend,an) }
  | alg_expr_up_to_if EQUAL annot alg_expr
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.COMPARE_OP(Operator.EQUAL,x,y),
        Locality.of_pos (start_pos 1) pend),
       pend,an) }
  | alg_expr_up_to_if DIFF annot alg_expr
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.COMPARE_OP(Operator.DIFF,x,y),
        Locality.of_pos (start_pos 1) pend),
       pend,an) }
  ;

bool_expr_no_or:
  | bool_expr_comp { $1 }
  | bool_expr_comp AND annot bool_expr_no_or
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_BOOL_OP(Operator.AND,x,y),
        Locality.of_pos (start_pos 1) pend),
       pend,an) }
  ;

bool_expr:
  | bool_expr_no_or { $1 }
  | bool_expr_no_or OR annot bool_expr
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_BOOL_OP(Operator.OR,x,y),
        Locality.of_pos (start_pos 1) pend),
       pend,an) }
  ;

standalone_bool_expr:
  | annot bool_expr EOF { let (x,_,_) = $2 in x }
  | annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 2 "Problematic boolean expression")) }
  ;

arrow:
  | RAR {false}
  | LRAR {true}
  ;

sum_token:
  | small_alg_expr annot ID annot { [($1,($3,rhs_pos 3))],end_pos 3,$4 }
  | small_alg_expr annot ID annot COMMA annot sum_token
    { let (l,pend,an) = $7 in ($1,($3,rhs_pos 3)) :: l,pend,an }
  ;

rule_side:
  | pattern { let (p,pend,an) = $1 in (p,[],pend,an) }
  | pattern PIPE annot sum_token
    { let (p,_,_) = $1 in
      let (t,pend,an) = $4 in (p, t, pend, an) }
  | PIPE annot sum_token { let (t,pend,an) = $3 in ([], t, pend, an) }
  | pattern PIPE annot error
    { raise (ExceptionDefn.Syntax_Error
	(add_pos 4 "Malformed token expression, I was expecting a_0 t_0, ... \
, a_n t_n where t_i are tokens and a_i any algebraic formula")) }
  | PIPE annot error
    { raise (ExceptionDefn.Syntax_Error
	(add_pos 3 "Malformed token expression, I was expecting a_0 t_0, ... \
, a_n t_n where t_i are tokens and a_i any algebraic formula")) }
  ;

rule_content:
  | rule_side arrow annot rule_side
    { let (lhs,rm_token,_,_) = $1 in
      let (rhs,add_token,pend,an) = $4 in
      (Ast.Arrow {Ast.lhs; Ast.rm_token; Ast.rhs; Ast.add_token},$2,pend,an) }
  | rule_side arrow annot
    { let (lhs,rm_token,_,_) = $1 in
      (Ast.Arrow {Ast.lhs; Ast.rm_token; Ast.rhs=[]; Ast.add_token=[]},$2,end_pos 2,$3) }
  | arrow annot rule_side
    { let (rhs,add_token,pend,an) = $3 in
      (Ast.Arrow {Ast.lhs=[]; Ast.rm_token=[]; Ast.rhs; Ast.add_token},$1,pend,an) }
  | rule_side
    { let (mix,delta_token,pend,an) = $1 in
      (Ast.Edit {Ast.mix; Ast.delta_token},false,pend,an) }
  ;

alg_with_radius:
  | alg_expr { let (x,_,_) = $1 in (x,None) }
  | alg_expr COLON annot alg_expr
    { let (x,_,_) = $1 in let (y,_,_) = $4 in (x, Some y) }
  ;

rate:
  | OP_CUR annot alg_with_radius CL_CUR annot alg_expr
    { let (b,pend,an) = $6 in (b,Some $3,pend,an) }
  | alg_expr OP_CUR annot alg_with_radius CL_CUR annot
    { let (x,_,_) = $1 in (x,Some $4,end_pos 5,$6) }
  | alg_expr { let (a,pend,an) = $1 in (a,None,pend,an) }
  ;

birate:
  | AT annot rate { let (k2,k1,pend,an) = $3 in (k2,k1,None,None,pend,an) }
  | AT annot rate COMMA annot rate
    { let (k2,k1,_,_) = $3 in
      let (kback,kback1,pend,an) = $6 in
      (k2,k1,Some kback,kback1,pend,an) }
  ;

rule:
  | rule_content birate
    { let (k_def,k_un,k_op,k_op_un,pos_end,_annot) = $2 in
      let (rewrite,bidirectional,_,_) = $1 in
      ({
        Ast.rewrite;Ast.bidirectional;
        Ast.k_def; Ast.k_un; Ast.k_op; Ast.k_op_un;
      },Locality.of_pos (start_pos 1) pos_end) }
  | rule_content error
    { raise (ExceptionDefn.Syntax_Error (add_pos 2 "rule rate expected")) }
  ;

variable_declaration:
  | LABEL annot alg_expr { let (v,pend,an) = $3 in (($1,rhs_pos 1),v,pend,an) }
  | ID annot alg_expr { let (v,pend,an) = $3 in (($1,rhs_pos 1),v,pend,an) }
  | LABEL annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3 ("Illegal definition of variable '"^$1^"'"))) }
  | ID annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3 ("Illegal definition of variable '"^$1^"'"))) }
  | error
    { raise (ExceptionDefn.Syntax_Error (add_pos 1 ("label expected"))) }
  ;

id_list:
  | ID annot { [ $1,rhs_pos 1 ] }
  | ID annot COMMA annot id_list { ($1,rhs_pos 1) :: $5 }
  ;

init_declaration:
  | alg_expr pattern
    { let (v,_,_) = $1 in
      let (p,pend,_) = $2 in
      (v,Ast.INIT_MIX (p,Locality.of_pos (start_pos 2) pend)) }
  | alg_expr OP_PAR annot pattern CL_PAR annot
    { let (v,_,_) = $1 in
      let (p,pend,_) = $4 in
      (v,Ast.INIT_MIX (p,Locality.of_pos (start_pos 4) pend)) }
  | alg_expr id_list
    { let (v,_,_) = $1 in (v,Ast.INIT_TOK $2) }
/*
  | ID annot OP_CUR annot init_declaration CL_CUR annot
    { let (_,alg,init) = $5 in (Some ($1,rhs_pos 1),alg,init) }
*/
  | error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 1 "Malformed initial condition")) }
  ;

value_list:
  | STRING annot {[$1, rhs_pos 1]}
  | STRING annot value_list {($1,rhs_pos 1)::$3}
  ;

nonempty_print_expr:
  | STRING annot
    { ([Primitives.Str_pexpr (add_pos 1 $1)],end_pos 1,$2) }
  | alg_expr_up_to_if
    { let (a,pend,p) = $1 in ([Primitives.Alg_pexpr a],pend,p) }
  | print_expr_list { $1 }
  | OP_PAR annot print_expr_list CL_PAR annot
    { let (v,_,an) = $3 in (v,end_pos 4,an @ $5) }
  ;

print_expr_list:
  | STRING annot DOT annot nonempty_print_expr
    { let (l,pend,p) = $5 in (Primitives.Str_pexpr ($1, rhs_pos 1)::l,pend,p) }
  | alg_expr_up_to_if DOT annot nonempty_print_expr
    { let (l,pend,p) = $4 in
      let (v,_,_) = $1 in
      (Primitives.Alg_pexpr v::l,pend,p) }
  ;

print_expr:
  | annot { ([],start_pos 1,$1) }
  | annot nonempty_print_expr { $2 }
  ;

effect:
  | ASSIGN annot ID annot alg_expr
    { let (a,pend,p) = $5 in (Ast.UPDATE (($3,rhs_pos 3),a),pend,p) }
  | ASSIGN annot LABEL annot alg_expr
    { let (a,pend,p) = $5 in (Ast.UPDATE (($3,rhs_pos 3),a),pend,p) }
  | TRACK annot LABEL annot boolean annot
    { (Ast.CFLOWLABEL ($5,($3,rhs_pos 3)),end_pos 5,$6) }
  | TRACK annot pattern boolean annot
    { let (pat,epat,_) = $3 in
      (Ast.CFLOWMIX ($4,(pat,Locality.of_pos (start_pos 3) epat)),end_pos 4, $5) }
  | FLUX annot nonempty_print_expr boolean annot
    { let (p,_,_) = $3 in
      ((if $4 then Ast.DIN (Primitives.RELATIVE,p) else Ast.DINOFF p),
       end_pos 4,$5) }
  | FLUX annot nonempty_print_expr STRING annot boolean annot
    { let (p,_,_) = $3 in
      if $6 && $4 = "absolute" then
        (Ast.DIN (Primitives.ABSOLUTE,p),end_pos 6,$7)
      else if $6 && $4 = "probability" then
        (Ast.DIN (Primitives.PROBABILITY,p),end_pos 6,$7)
      else if $6 && $4 = "relative" then
        (Ast.DIN (Primitives.RELATIVE,p),end_pos 6,$7)
      else raise (ExceptionDefn.Syntax_Error
                    ("Incorrect DIN expression",rhs_pos 4)) }
  | APPLY annot alg_expr rule_content
    { let (rewrite,_,pend,an) = $4 in
      let (v,_,_) = $3 in
      Ast.APPLY(v,
                ({ Ast.rewrite; Ast.bidirectional = false;
                   Ast.k_def=Alg_expr.const Nbr.zero;Ast.k_un=None;
                   Ast.k_op=None; Ast.k_op_un=None},
                 Locality.of_pos (start_pos 3) pend)),
      pend,an
    }
  | INTRO annot alg_expr pattern
    { let (m,pend,p) = $4 in
      let (v,_,_) = $3 in
      (Ast.APPLY(v,
                 ({Ast.rewrite =
		   Ast.Edit {Ast.mix=Ast.to_created_mixture m;
                     Ast.delta_token=[];};
		   Ast.bidirectional=false;
                   Ast.k_def=Alg_expr.const Nbr.zero; Ast.k_un=None;
                   Ast.k_op=None; Ast.k_op_un=None},
                  Locality.of_pos (start_pos 4) pend)),
       pend,p) }
  | INTRO annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3 "Malformed intervention instruction, I was expecting \
'$ADD alg_expression kappa_expression'")) }
  | DELETE annot alg_expr pattern
    { let (m,pend,p) = $4 in
      let (v,_,_) = $3 in
      (Ast.APPLY(v,
               ({Ast.rewrite =
		 Ast.Edit {Ast.mix=Ast.to_erased_mixture m;
                   Ast.delta_token=[];};
		 Ast.bidirectional=false;
                 Ast.k_def=Alg_expr.const Nbr.zero; Ast.k_un=None;
                 Ast.k_op=None; Ast.k_op_un=None},
                Locality.of_pos (start_pos 4) pend)),
       pend,p) }
  | DELETE annot error
           { raise (ExceptionDefn.Syntax_Error
                      (add_pos 3 "Malformed intervention instruction, I was \
expecting '$DEL alg_expression kappa_expression'")) }
  | SNAPSHOT print_expr { let (s,pend,p) = $2 in (Ast.SNAPSHOT s,pend,p) }
  | STOP print_expr { let (s,pend,p) = $2 in (Ast.STOP s,pend,p) }
  | PRINTF print_expr GREATER print_expr
    { let (f,pend,p) = $4 in let (c,_,_) = $2 in (Ast.PRINT (f,c),pend,p) }
  | PRINTF print_expr { let (c,pend,p) = $2 in (Ast.PRINT ([],c),pend,p) }
  | PLOTENTRY annot { (Ast.PLOTENTRY,end_pos 1,$2) }
  | SPECIES_OF annot pattern boolean annot GREATER print_expr
    {
      let (file,pend,p) = $7 in
      let (pat,pendp,_) = $3 in
      (Ast.SPECIES_OF ($4,file,(pat, Locality.of_pos (start_pos 3) pendp)),
       pend,p) }
  ;

partial_effect_list:
  | effect SEMICOLON annot { let (e,_,_) = $1 in ([e],end_pos 2,$3) }
  | effect { let (e,p,a) = $1 in ([e],p,a) }
  | effect SEMICOLON annot partial_effect_list
    { let (e,_,_) = $1 in let (l,pend,a) = $4 in (e::l,pend,a) }

effect_list:
  | OP_PAR annot partial_effect_list CL_PAR annot { $3 }
  | effect SEMICOLON annot { let (e,_,_) = $1 in ([e],end_pos 2,$3) }
  | effect SEMICOLON annot effect_list
    { let (e,_,_) = $1 in let (l,pend,a) = $4 in (e::l,pend,a) }
  ;

standalone_effect_list:
  | annot partial_effect_list EOF { let (e,_,_) = $2 in e }
  | error
    { raise (ExceptionDefn.Syntax_Error (add_pos 1 "Problematic effect list")) }
  ;

perturbation_alarm:
  | annot { None }
  | annot ALARM annot nbr annot { Some $4 }
  ;

perturbation_post:
  | { (None, Parsing.symbol_start_pos (),[]) }
  | REPEAT annot bool_expr { let (b,pend,p) = $3 in (Some b,pend,p) }
  ;

perturbation_declaration:
  | perturbation_alarm bool_expr DO annot effect_list perturbation_post
    { let (pre,_,_) = $2 in
      let (e,_,_) = $5 in
      let (post,_,_) = $6 in
      ($1,Some pre,e,post) }
  | perturbation_alarm DO annot effect_list perturbation_post
    { let (e,_,_) = $4 in let (post,_,_) = $5 in ($1,None,e,post) }
  ;

sentence:
  | LABEL annot rule
    { add (Ast.RULE(Some ($1, rhs_pos 1),$3)) }
  | LABEL annot EQUAL annot alg_expr
    { let (v,_,_) = $5 in add (Ast.DECLARE (($1,rhs_pos 1),v)) }
  | rule { add (Ast.RULE (None,$1)) }
  | SIGNATURE annot agent { let (a,_,_) = $3 in add (Ast.SIG a) }
  | SIGNATURE annot error
    { raise
        (ExceptionDefn.Syntax_Error (add_pos 3 "Malformed agent signature")) }
  | TOKEN annot ID annot { add (Ast.TOKENSIG ($3,rhs_pos 3)) }
  | PLOT annot alg_expr { let (v,_,_) = $3 in add (Ast.PLOT v) }
  | PLOT annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3
                  "Malformed plot instruction, \
an algebraic expression is expected")) }
  | LET annot variable_declaration
    { let (i,v,_,_) = $3 in add (Ast.DECLARE (i,v)) }
  | OBS annot variable_declaration { let (i,v,_,_) = $3 in add (Ast.OBS (i,v)) }
  | INIT annot init_declaration
    { let (alg,init) = $3 in add (Ast.INIT (alg,init)) }
  | PERT perturbation_declaration { add (Ast.PERT ($2, rhs_pos 2)) }
  | CONFIG annot STRING annot value_list
    { add (Ast.CONFIG (($3,rhs_pos 3),$5)) }
  ;

model_body:
  | sentence model_body { $2 }
  | EOF { output () }
  ;

model:
  | annot model_body { $2 }
  | error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 1 "Incorrect beginning of sentence")) }
  ;

interactive_command:
  | annot RUN annot SEMICOLON { Ast.RUN (Locality.dummy_annot Alg_expr.FALSE) }
  | annot RUN annot bool_expr SEMICOLON { let (pause,_,_) = $4 in Ast.RUN pause }
  | annot effect SEMICOLON { let (eff,_,_) = $2 in Ast.MODIFY [eff] }
  | annot EOF { Ast.QUIT }
  | error
    { raise (ExceptionDefn.Syntax_Error (add_pos 1 "Unrecognized command")) }
  ;
