/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2015                                               */
/*    CEA   (Commissariat à l'énergie atomique et aux énergies            */
/*           alternatives)                                                */
/*    INRIA (Institut National de Recherche en Informatique et en         */
/*           Automatique)                                                 */
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

/* Grammar for C annotations */

%{

  open Cil
  open Cil_types
  open Logic_ptree
  open Logic_utils

  let loc () = (symbol_start_pos (), symbol_end_pos ())
  let info x = { lexpr_node = x; lexpr_loc = loc () }
  let loc_info loc x = { lexpr_node = x; lexpr_loc = loc }
  let loc_start x = fst x.lexpr_loc
  let loc_end x = snd x.lexpr_loc

  (* Normalize p1 && (p2 && p3) into (p1 && p2) && p3 *)
  let rec pland p1 p2 =
    match p2.lexpr_node with
      | PLand (p3,p4) ->
        let loc = (loc_start p1, loc_end p3) in
        PLand(loc_info loc (pland p1 p3),p4)
      | _ -> PLand(p1,p2)

  let rec plor p1 p2 =
    match p2.lexpr_node with
      | PLor(p3,p4) ->
        let loc = (loc_start p1, loc_end p3) in
        PLor(loc_info loc (plor p1 p3),p4)
      | _ -> PLor(p1,p2)

  let clause_order i name1 name2 =
    raise
      (Not_well_formed
         ((rhs_start_pos i, rhs_end_pos i),
          "wrong order of clause in contract: "
          ^ name1 ^ " after " ^ name2 ^ "."))

  let missing i token next_token =
    raise
      (Not_well_formed
         ((rhs_start_pos i, rhs_end_pos i),
          Pretty_utils.sfprintf "expecting '%s' before %s" token next_token))

  type sense_of_relation = Unknown | Disequal | Less | Greater

  let check_empty (loc,msg) l =
    match l with
        [] -> ()
      | _ -> raise (Not_well_formed (loc,msg))

  let relation_sense rel sense =
    match rel, sense with
        Eq, _ -> sense, true
      | Neq, Unknown -> Disequal, true (* No chain of disequality for now*)
      | (Gt|Ge), (Unknown|Greater) -> Greater, true
      | (Lt|Le), (Unknown|Less) -> Less, true
      | _ -> sense, false

  let type_variables_stack = Stack.create ()

  let enter_type_variables_scope l =
    List.iter Logic_env.add_typename l;
    Stack.push l type_variables_stack

  let exit_type_variables_scope () =
    let l = Stack.pop type_variables_stack in
    List.iter Logic_env.remove_typename l

  let rt_type = ref false

  let set_rt_type () = rt_type:= true

  let reset_rt_type () = rt_type:=false

  let is_rt_type () = !rt_type

  let loc_decl d = { decl_node = d; decl_loc = loc () }

  let wrap_extended = List.map (fun (n,p) -> n,0, p)

  let concat_froms a1 a2 =
    let compare_pair (b1,_) (b2,_) = is_same_lexpr b1 b2 in
    (* NB: the following has an horrible complexity, but the order of
       clauses in the input is preserved. *)
    let concat_one acc (_,f2 as p)  =
      try
        let (_,f1) = List.find (compare_pair p) acc
        in
        match (f1, f2) with
          | _,FromAny ->
            (* the new fundeps does not give more information than the one
               which is already present. Just ignore it.
             *)
           acc
          | FromAny, _ ->
              (* the new fundeps is strictly more precise than the old one.
                 We replace the old dependency by the new one, but keep
                 the location at its old place in the list. This ensures
                 that we get the exact same clause if we try to
                 link the original contract with its pretty-printed version. *)
              Extlib.replace compare_pair p acc
          | From _, From _ ->
            (* we keep the two functional dependencies,
               as they have to be proved separately. *)
            acc @ [p]
      with Not_found -> acc @ [p]
    in List.fold_left concat_one a1 a2

  let concat_allocation fa1 fa2 =
    match fa1,fa2 with
      | FreeAllocAny,_ -> fa2
      | _,FreeAllocAny -> fa1
      | FreeAlloc(f1,a1),FreeAlloc(f2,a2) -> FreeAlloc(f2@f1,a2@a1)

  (* a1 represents the assigns _after_ the current clause a2. *)
  let concat_assigns a1 a2 =
    match a1,a2 with
        WritesAny,a -> Writes (concat_froms [] a)
      | Writes [], [] -> a1
      | Writes [], _  | Writes _, [] ->
        raise (
          Not_well_formed (loc(),"Mixing \\nothing and a real location"))
      | Writes a1, a2 -> Writes (concat_froms a2 a1)

  let concat_loop_assigns_allocation annots bhvs2 a2 fa2=
    (* NB: this is supposed to merge assigns related to named behaviors, in
       case of annotation like
       for a,b: assigns x,y;
       for b,c: assigns z,t;
       DO NOT CALL this function for loop assigns not attached to specific
       behaviors.
     *)
    assert (bhvs2 <> []);
    if fa2 == FreeAllocAny && a2 == WritesAny
    then annots
    else
    let split l1 l2 =
      let treat_one (only1,both,only2) x =
        if List.mem x l1 then
          (Extlib.filter_out (fun y -> x=y) only1,x::both,only2)
        else (only1,both,x::only2)
      in List.fold_left treat_one (l1,[],[]) l2
    in
    let treat_one ca (bhvs2,acc) =
      match ca,a2,fa2 with
          (AAssigns(bhvs1,a1)),(Writes a2),_ ->
            let (only1,both,only2) = split bhvs1 bhvs2 in
            (match both with
              | [] -> bhvs2, ca::acc
              | _ ->
                let common_annot = AAssigns(both,concat_assigns a1 a2) in
                let annots =
                  match only1 with
                    | [] -> common_annot :: acc
                    | _ -> AAssigns(only1,a1) :: common_annot :: acc
                in only2,annots)
        | (AAllocation(bhvs1,fa1)),_,(FreeAlloc _) ->
           let (only1,both,only2) = split bhvs1 bhvs2 in
            (match both with
              | [] -> bhvs2, ca::acc
              | _ ->
                let common_annot =
                  AAllocation(both,concat_allocation fa1 fa2)
                in
                let annots =
                  match only1 with
                    | [] -> common_annot :: acc
                    | _ -> AAllocation(only1,fa1) :: common_annot :: acc
                in only2,annots)
         | _,_,_ -> bhvs2,ca::acc
    in
    let (bhvs2, annots) = List.fold_right treat_one annots (bhvs2,[]) in
    match bhvs2 with
      | [] -> annots (* Already considered all cases. *)
      | _ ->
	  let annots = if a2 <> WritesAny
	    then AAssigns (bhvs2,a2) :: annots
            else annots
	  in
	  if fa2 <> FreeAllocAny
	    then AAllocation (bhvs2,fa2) :: annots
            else annots

  let obsolete name ~source ~now =
    Kernel.warning ~source
      "parsing obsolete ACSL construct '%s'. '%s' should be used instead."
      name now

  let check_registered kw =
    if Logic_utils.is_extension kw then kw else raise Parsing.Parse_error

  let escape =
    let regex1 = Str.regexp "\\(\\(\\\\\\\\\\)*[^\\]\\)\\(['\"]\\)" in
    let regex2 = Str.regexp "\\(\\\\\\\\\\)*\\\\$" in
    fun str ->
      let str = Str.global_replace regex1 "\\1\\\\3" str in
      Str.global_replace regex2 "\\1\\\\" str

%}

/*****************************************************************************/
/* IMPORTANT NOTE: When you add a new token, be sure that it will be         */
/* recognized by the any: rule at the end of this file.                      */
/* Otherwise, the token will not be usable inside a contract.                */
/*****************************************************************************/

%token MODULE FUNCTION CONTRACT INCLUDE EXT_AT EXT_LET
/* ACSL extension for external spec  file */
%token <string> IDENTIFIER TYPENAME
%token <bool*string> STRING_LITERAL
%token <Logic_ptree.constant> CONSTANT
%token <string> CONSTANT10
%token LPAR RPAR IF ELSE COLON COLON2 COLONCOLON DOT DOTDOT DOTDOTDOT
%token INT INTEGER REAL BOOLEAN BOOL FLOAT LT GT LE GE EQ NE COMMA ARROW EQUAL
%token FORALL EXISTS IFF IMPLIES AND OR NOT SEPARATED
%token TRUE FALSE OLD AT RESULT
%token BLOCK_LENGTH BASE_ADDR OFFSET VALID VALID_READ VALID_INDEX VALID_RANGE
%token ALLOCATION STATIC REGISTER AUTOMATIC DYNAMIC UNALLOCATED
%token ALLOCABLE FREEABLE FRESH
%token DOLLAR QUESTION MINUS PLUS STAR AMP SLASH PERCENT LSQUARE RSQUARE EOF
%token GLOBAL INVARIANT VARIANT DECREASES FOR LABEL ASSERT SEMICOLON NULL EMPTY
%token REQUIRES ENSURES ALLOCATES FREES ASSIGNS LOOP NOTHING SLICE IMPACT PRAGMA FROM
%token EXITS BREAKS CONTINUES RETURNS
%token VOLATILE READS WRITES
%token LOGIC PREDICATE INDUCTIVE AXIOMATIC AXIOM LEMMA LBRACE RBRACE
%token GHOST MODEL CASE
%token VOID CHAR SIGNED UNSIGNED SHORT LONG DOUBLE STRUCT ENUM UNION
%token BSUNION INTER
%token LTCOLON COLONGT TYPE BEHAVIOR BEHAVIORS ASSUMES COMPLETE DISJOINT
%token TERMINATES
%token BIFF BIMPLIES HAT HATHAT PIPE TILDE GTGT LTLT
%token SIZEOF LAMBDA LET
%token TYPEOF BSTYPE
%token WITH CONST
%token INITIALIZED DANGLING
%token CUSTOM

%nonassoc lowest
%right prec_named
%nonassoc IDENTIFIER TYPENAME SEPARATED
%nonassoc prec_forall prec_exists prec_lambda LET
%right QUESTION prec_question
%left IFF
%right IMPLIES
%left OR
%left HATHAT
%left AND
%left PIPE
%left BIFF
%right BIMPLIES
%left HAT
%left AMP
%nonassoc prec_no_rel
%left prec_rel_list /* for list of relations (LT GT LE GE EQ NE) */
%left LT
%left LTLT GTGT
%left PLUS MINUS
%left STAR SLASH PERCENT CONST VOLATILE
%right prec_cast TILDE NOT prec_unary_op
%nonassoc LTCOLON COLONGT
%left DOT ARROW LSQUARE
%right prec_par
%nonassoc highest

%type <Logic_ptree.lexpr> lexpr_eof
%start lexpr_eof

%type <Logic_ptree.annot> annot
%start annot

%type <Logic_ptree.spec * Cabs.cabsloc> spec
%start spec

%type <Logic_ptree.ext_spec> ext_spec
%start ext_spec

%%

enter_kw_c_mode:
/* empty */ { enter_kw_c_mode () }

exit_kw_c_mode:
/* empty */ { exit_kw_c_mode () }

enter_rt_type:
/* empty */ { if is_rt_type () then enter_rt_type_mode () }

exit_rt_type:
/* empty */ { if is_rt_type () then exit_rt_type_mode () }

begin_rt_type:
/* empty */ { set_rt_type () }

end_rt_type:
/* empty */ { reset_rt_type () }

/*** predicates and terms ***/

lexpr_list:
| /* epsilon */ { [] }
| ne_lexpr_list  { $1 }
;

ne_lexpr_list:
| lexpr                    { [$1] }
| lexpr COMMA ne_lexpr_list { $1 :: $3 }
;

lexpr_eof:
| full_lexpr EOF { $1 }
;

lexpr_option:
| /* epsilon */ { None }
| lexpr         { Some $1 }
;

lexpr:
  /* predicates */
| lexpr IMPLIES lexpr { info (PLimplies ($1, $3)) }
| lexpr IFF lexpr { info (PLiff ($1, $3)) }
| lexpr OR lexpr     { info (plor $1 $3) }
| lexpr AND lexpr    { info (pland $1 $3) }
| lexpr HATHAT lexpr    { info (PLxor ($1, $3)) }
/* terms */
| lexpr AMP lexpr { info (PLbinop ($1, Bbw_and, $3)) }
| lexpr PIPE lexpr { info (PLbinop ($1, Bbw_or, $3)) }
| lexpr HAT lexpr { info (PLbinop ($1, Bbw_xor, $3)) }
| lexpr BIMPLIES lexpr { info (PLbinop (info (PLunop (Ubw_not, $1)), Bbw_or, $3)) }
| lexpr BIFF lexpr { info (PLbinop (info (PLunop (Ubw_not, $1)), Bbw_xor, $3)) }
| lexpr QUESTION lexpr COLON2 lexpr %prec prec_question
    { info (PLif ($1, $3, $5)) }
/* both terms and predicates */
| any_identifier COLON lexpr %prec prec_named { info (PLnamed ($1, $3)) }
| string COLON lexpr %prec prec_named
      { let (iswide,str) = $1 in
        if iswide then begin
           let l = loc () in
           raise (Not_well_formed(l, "Wide strings are not allowed as labels."))
         end;
        let str = escape str in
         info (PLnamed (str, $3))
       }
| lexpr_rel %prec prec_rel_list { $1 }
;

lexpr_rel:
| lexpr_end_rel %prec prec_no_rel { $1 }
| lexpr_inner rel_list %prec prec_rel_list
      { let rel, rhs, _, oth_rel = $2 in
        let loc = loc_start $1, loc_end rhs in
        let relation = loc_info loc (PLrel($1,rel,rhs)) in
        match oth_rel with
            None -> relation
          | Some oth_relation -> info (pland relation oth_relation)
      }
;

lexpr_binder:
| LET bounded_var EQUAL lexpr SEMICOLON lexpr %prec LET {info (PLlet($2,$4,$6))}
| FORALL binders SEMICOLON lexpr  %prec prec_forall
      { info (PLforall ($2, $4)) }
| EXISTS binders SEMICOLON lexpr  %prec prec_exists
      { info (PLexists ($2, $4)) }
| LAMBDA binders SEMICOLON lexpr  %prec prec_lambda
      { info (PLlambda ($2,$4)) }
;

lexpr_end_rel:
  lexpr_inner %prec prec_no_rel { $1 }
| lexpr_binder { $1 }
| NOT lexpr_binder { info (PLnot $2) }
;

rel_list:
| relation lexpr_end_rel %prec prec_rel_list
  { $1, $2, fst(relation_sense $1 Unknown), None }
| relation lexpr_inner rel_list %prec prec_rel_list
  {
    let next_rel, rhs, sense, oth_rel = $3 in
    let (sense, correct) = relation_sense $1 sense
    in
    if correct then
      let loc = loc_start $2, loc_end rhs in
      let my_rel = loc_info loc (PLrel($2,next_rel,rhs)) in
      let oth_rel = match oth_rel with
          None -> my_rel
        | Some rel ->
	    let loc = loc_start $2, loc_end rel in
	    loc_info loc (pland my_rel rel)
      in
      $1,$2,sense,Some oth_rel
    else begin
      let loc = Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3 in
      raise (Not_well_formed(loc,"Inconsistent relation chain."));
    end
  }
;

relation:
| LT    { Lt }
| GT    { Gt }
| LE    { Le }
| GE    { Ge }
| EQ    { Eq }
| NE    { Neq }
    /* C. Marche: added to produce better error messages */
| EQUAL {
      let l = loc () in
      raise
        (Not_well_formed(l,
                         "Assignment operators not allowed in annotations."))
    }
;

lexpr_inner:
| string {
      let (is_wide,content) = $1 in
      let cst = if is_wide then
        WStringConstant content
      else
        StringConstant content
      in
      info (PLconstant cst)
    }
| NOT lexpr_inner { info (PLnot $2) }
| TRUE { info PLtrue }
| FALSE { info PLfalse }
| VALID opt_label_1 LPAR lexpr RPAR { info (PLvalid ($2,$4)) }
| VALID_READ opt_label_1 LPAR lexpr RPAR { info (PLvalid_read ($2,$4)) }
| VALID_INDEX opt_label_1 LPAR lexpr COMMA lexpr RPAR {
  let source = fst (loc ()) in
  obsolete ~source "\\valid_index(addr,idx)" ~now:"\\valid(addr+idx)";
  info (PLvalid ($2,info (PLbinop ($4, Badd, $6)))) }
| VALID_RANGE opt_label_1 LPAR lexpr COMMA lexpr COMMA lexpr RPAR {
  let source = fst (loc ()) in
  obsolete "\\valid_range(addr,min,max)"
    ~source ~now:"\\valid(addr+(min..max))";
  info (PLvalid
          ($2,info (PLbinop ($4, Badd, (info (PLrange((Some $6),Some $8)))))))
}
| INITIALIZED opt_label_1 LPAR lexpr RPAR { info (PLinitialized ($2,$4)) }
| DANGLING opt_label_1 LPAR lexpr RPAR { info (PLdangling ($2,$4)) }
| FRESH opt_label_2 LPAR lexpr COMMA lexpr RPAR { info (PLfresh ($2,$4, $6)) }
| BASE_ADDR opt_label_1 LPAR lexpr RPAR { info (PLbase_addr ($2,$4)) }
| BLOCK_LENGTH opt_label_1 LPAR lexpr RPAR { info (PLblock_length ($2,$4)) }
| OFFSET opt_label_1 LPAR lexpr RPAR { info (PLoffset ($2,$4)) }
| ALLOCABLE opt_label_1 LPAR lexpr RPAR { info (PLallocable ($2,$4)) }
| FREEABLE opt_label_1 LPAR lexpr RPAR { info (PLfreeable ($2,$4)) }
| ALLOCATION opt_label_1 LPAR lexpr RPAR  { Format.eprintf "Warning: \\static not yet implemented." ;
	   (* TODO: *) raise Parse_error }
| AUTOMATIC { Format.eprintf "Warning: \\static not yet implemented." ;
	   (* TODO: *) raise Parse_error }
| DYNAMIC { Format.eprintf "Warning: \\dynamic not yet implemented." ;
	   (* TODO: *) raise Parse_error }
| REGISTER { Format.eprintf "Warning: \\register not yet implemented." ;
	   (* TODO: *) raise Parse_error }
| STATIC { Format.eprintf "Warning: \\static not yet implemented." ;
	   (* TODO: *) raise Parse_error }
| UNALLOCATED { Format.eprintf "Warning: \\unallocated not yet implemented." ;
	   (* TODO: *) raise Parse_error }
| NULL { info PLnull }
| constant { info (PLconstant $1) }
| lexpr_inner PLUS lexpr_inner { info (PLbinop ($1, Badd, $3)) }
| lexpr_inner MINUS lexpr_inner { info (PLbinop ($1, Bsub, $3)) }
| lexpr_inner STAR lexpr_inner { info (PLbinop ($1, Bmul, $3)) }
| lexpr_inner SLASH lexpr_inner { info (PLbinop ($1, Bdiv, $3)) }
| lexpr_inner PERCENT lexpr_inner { info (PLbinop ($1, Bmod, $3)) }
| lexpr_inner ARROW identifier_or_typename { info (PLarrow ($1, $3)) }
| lexpr_inner DOT identifier_or_typename { info (PLdot ($1, $3)) }
| lexpr_inner LSQUARE range RSQUARE { info (PLarrget ($1, $3)) }
| lexpr_inner LSQUARE lexpr RSQUARE { info (PLarrget ($1, $3)) }
| MINUS lexpr_inner %prec prec_unary_op { info (PLunop (Uminus, $2)) }
| PLUS  lexpr_inner %prec prec_unary_op { $2 }
| TILDE lexpr_inner { info (PLunop (Ubw_not, $2)) }
| STAR  lexpr_inner %prec prec_unary_op { info (PLunop (Ustar, $2)) }
| AMP   lexpr_inner %prec prec_unary_op { info (PLunop (Uamp, $2)) }
| SIZEOF LPAR lexpr RPAR { info (PLsizeofE $3) }
| SIZEOF LPAR logic_type RPAR { info (PLsizeof $3) }
| OLD LPAR lexpr RPAR { info (PLold $3) }
| AT LPAR lexpr COMMA label_name RPAR { info (PLat ($3, $5)) }
| RESULT { info PLresult }
| SEPARATED LPAR ne_lexpr_list RPAR
      { info (PLseparated $3) }
| identifier LPAR ne_lexpr_list RPAR
      { info (PLapp ($1, [], $3)) }
| identifier LBRACE ne_label_args RBRACE LPAR ne_lexpr_list RPAR
      { info (PLapp ($1, $3, $6)) }
| identifier LBRACE ne_label_args RBRACE
      { info (PLapp ($1, $3, [])) }
| identifier %prec IDENTIFIER { info (PLvar $1) }
| lexpr_inner GTGT lexpr_inner { info (PLbinop ($1, Brshift, $3))}
| lexpr_inner LTLT lexpr_inner { info (PLbinop ($1, Blshift, $3))}
| LPAR lexpr RPAR %prec prec_par { info $2.lexpr_node }
| LPAR range RPAR { info $2.lexpr_node }
| LPAR cast_logic_type RPAR lexpr_inner %prec prec_cast
      { info (PLcast ($2, $4)) }
| lexpr_inner LTCOLON lexpr_inner %prec prec_cast
      { info (PLsubtype ($1, $3)) }
| lexpr_inner COLONGT logic_type %prec prec_cast
      { info (PLcoercion ($1, $3)) }
| lexpr_inner COLONGT lexpr_inner %prec prec_cast
      { info (PLcoercionE ($1, $3)) }
| TYPEOF LPAR lexpr RPAR { info (PLtypeof $3) }
| BSTYPE LPAR type_spec RPAR { info (PLtype $3) }
| BSTYPE LPAR type_spec stars RPAR { info (PLtype ($4 $3)) }
    /* tsets */
| EMPTY { info PLempty }
| BSUNION LPAR lexpr_list RPAR { info (PLunion $3) }
| INTER LPAR lexpr_list RPAR { info (PLinter $3) }
| LBRACE lexpr RBRACE
      { info (PLsingleton ($2)) }
| LBRACE lexpr PIPE binders RBRACE
      {info (PLcomprehension ($2,$4,None)) }
| LBRACE lexpr PIPE binders SEMICOLON lexpr RBRACE
      { info (PLcomprehension ($2,$4,Some $6)) }
    /* Aggregated object initialization */
| LBRACE field_init RBRACE
      { info (PLinitField($2)) }
| LBRACE array_init RBRACE
      { info (PLinitIndex($2)) }
| LBRACE lexpr WITH update RBRACE
      { List.fold_left
	  (fun a (path,upd_val) -> info (PLupdate(a,path,upd_val))) $2 $4 }
/*
| LET bounded_var EQUAL lexpr SEMICOLON lexpr %prec LET {info (PLlet($2,$4,$6))}*/
;

ne_label_args:
| identifier_or_typename { [ $1 ] }
| identifier_or_typename COMMA ne_label_args { $1 :: $3 }

string:
| STRING_LITERAL { $1 }
| string STRING_LITERAL {
      let (is_wide,prefix) = $1 in
      let (is_wide2,suffix) = $2 in
      (is_wide || is_wide2, prefix ^ suffix)
    }
;

range:
| lexpr_option DOTDOT lexpr_option { info (PLrange($1,$3)) }
;

/*** Aggregated object initialization ***/

field_path_elt:
| DOT identifier_or_typename { $2 }
;
field_init_elt:
| field_path_elt EQUAL lexpr { ($1, $3) }
;

field_init:
| field_init_elt                  { [$1] }
| field_init_elt COMMA field_init { $1::$3 }
;

array_path_elt:
| LSQUARE lexpr RSQUARE      { $2 }
| LSQUARE range RSQUARE      { $2 }
;

array_init_elt:
| array_path_elt EQUAL lexpr { ($1, $3) }


array_init:
| array_init_elt                  { [$1] }
| array_init_elt COMMA array_init { $1::$3 }
;

/*** Functional update ***/
update:
| update_elt                  { [$1] }
| update_elt COMMA update { $1::$3 }
;

update_elt:
| path EQUAL lexpr                { $1, PLupdateTerm $3 }
| path EQUAL LBRACE WITH update RBRACE { $1, PLupdateCont $5 }
;

path:
| path_elt      { [$1] }
| path_elt path { $1::$2 }
;

path_elt:
| field_path_elt { PLpathField $1 }
| array_path_elt { PLpathIndex $1 }
;

/*** binders ***/

binders:
| binders_reentrance { let (_lt, vars) = $1 in vars }
;

binders_reentrance:
| decl_spec { let (lt, var) = $1 in (lt, [var]) }
| binders_reentrance COMMA decl_spec
    { let _, vars = $1 in
      let (lt, var) = $3 in
        (lt, vars @ [ var ])
    }
| binders_reentrance COMMA var_spec
    { let last_type_spec, vars = $1 in
        (last_type_spec, vars @ [ let (modif, name) = $3 in (modif last_type_spec, name)])
    }
;

decl_spec:
| type_spec var_spec { ($1, let (modif, name) = $2 in (modif $1, name))  }
;

var_spec:
|       var_spec_bis { let (outer, inner,name) = $1 in
                       ((fun x -> outer (inner x)), name)}
| stars var_spec_bis
  { let (outer, inner, name) = $2 in
      ((fun x -> outer (inner ($1 x))), name) }
;

constant:
| CONSTANT   { $1 }
| CONSTANT10 { IntConstant $1 }
;

constant_option:
|  constant { Some $1 }
| /* empty */ { None }
;

var_spec_bis:
| identifier     { ((fun x -> x),(fun x -> x), $1) }
| var_spec_bis LSQUARE constant_option RSQUARE
      { let (outer, inner, name) = $1 in
          (outer, (fun x -> inner (LTarray (x,$3))), name)
      }
| LPAR var_spec RPAR { let (modif, name) = $2 in (modif, (fun x -> x), name) }
| var_spec_bis LPAR abs_param_type_list RPAR
      { let (outer, inner,name) = $1 in
        let params = $3 in
        (outer, (fun x -> inner (LTarrow (params,x))), name)
      }
;

abs_param_type_list:
| /* empty */    { [ ] }
| abs_param_list { $1 }
| abs_param_list COMMA DOTDOTDOT {
    Format.eprintf "Warning: elipsis type is not yet implemented." ;
    (* TODO: *) raise Parse_error
  }
;

abs_param_list:
| abs_param { [ $1 ] }
| abs_param_list COMMA abs_param { $1 @ [ $3 ] }
;

/* TODO: abs_param should be less restrictive than parameter
since its name can be omitted
*/
abs_param:
| logic_type { $1 }
;

/*** restricted type expressions ***/

id_as_typename:
| identifier { LTnamed($1, []) }
;

ne_parameters:
| parameter { [$1] }
| parameter COMMA ne_parameters { $1 :: $3 }
;

parameter:
| type_spec var_spec { let (modif, name) = $2 in (modif $1, name)}
| id_as_typename var_spec { let (modif, name) = $2 in (modif $1, name) }
;


/*** type expressions ***/

logic_type:
| type_spec abs_spec_option { $2 $1 }
;

cv:
  CONST { }
| VOLATILE { }
;

type_spec_cv:
     type_spec { $1 }
|    cv type_spec { $2 }
|    type_spec cv { $1 }

cast_logic_type:
 | type_spec_cv abs_spec_cv_option { $2 $1 }
;

logic_rt_type:
| id_as_typename { $1 }
| begin_rt_type logic_type end_rt_type { $2 }
;

abs_spec_option:
| /* empty */ %prec TYPENAME  { fun t -> t }
| abs_spec { $1 }
;

abs_spec_cv_option:
| /* empty */ %prec TYPENAME  { fun t -> t }
| abs_spec_cv { $1 }
;

abs_spec:
|                    tabs { $1 }
| stars                   %prec TYPENAME { $1 }
| stars              tabs                { fun t -> $2 ($1 t) }
| stars abs_spec_bis      %prec TYPENAME { fun t -> $2 ($1 t) }
| stars abs_spec_bis tabs                { fun t -> $2 ($3 ($1 t)) }
|       abs_spec_bis tabs                { fun t -> $1 ($2 t) }
|       abs_spec_bis      %prec TYPENAME { $1 }
;

abs_spec_cv:
|                         tabs { $1 }
| stars_cv                      %prec TYPENAME { $1 }
| stars_cv                 tabs                { fun t -> $2 ($1 t) }
| stars_cv abs_spec_bis_cv      %prec TYPENAME { fun t -> $2 ($1 t) }
| stars_cv abs_spec_bis_cv tabs                { fun t -> $2 ($3 ($1 t)) }
|          abs_spec_bis_cv tabs                { fun t -> $1 ($2 t) }
|          abs_spec_bis_cv      %prec TYPENAME { $1 }
;

abs_spec_bis:
| LPAR abs_spec RPAR { $2 }
| abs_spec_bis LPAR abs_param_type_list RPAR { fun t -> $1 (LTarrow($3,t)) };
;

abs_spec_bis_cv:
| LPAR abs_spec_cv RPAR { $2 }
| abs_spec_bis_cv LPAR abs_param_type_list RPAR { fun t -> $1 (LTarrow($3,t)) };
;

stars:
| STAR          { fun t -> LTpointer t }
| stars STAR    { fun t -> $1 (LTpointer t) }
;

stars_cv:
| STAR          { fun t -> LTpointer t }
| STAR cv       { fun t -> LTpointer t }
| stars_cv STAR    { fun t -> $1 (LTpointer t) }
| stars_cv STAR cv { fun t -> $1 (LTpointer t) }
;

tabs:
| LSQUARE constant_option RSQUARE %prec TYPENAME
    {
      fun t -> LTarray (t,$2)
    }
| LSQUARE constant_option RSQUARE tabs
    {
      fun t -> (LTarray ($4 t,$2))
    }
;

type_spec:
| INTEGER        { LTinteger }
| REAL           { LTreal }
| BOOLEAN        { LTnamed (Utf8_logic.boolean,[]) }
| VOID           { LTvoid }
| BOOL           { LTint IBool }
| CHAR           { LTint IChar }       /** [char] */
| SIGNED CHAR    { LTint ISChar }      /** [signed char] */
| UNSIGNED CHAR  { LTint IUChar }      /** [unsigned char] */
| INT            { LTint IInt }        /** [int] */
| SIGNED INT     { LTint IInt }        /** [int] */
| UNSIGNED INT   { LTint IUInt }       /** [unsigned int] */
| UNSIGNED       { LTint IUInt }
| SHORT          { LTint IShort }      /** [short] */
| SIGNED SHORT   { LTint IShort }      /** [short] */
| UNSIGNED SHORT { LTint IUShort }     /** [unsigned short] */
| LONG           { LTint ILong }       /** [long] */
| SIGNED LONG    { LTint ILong }       /** [long] */
| UNSIGNED LONG  { LTint IULong }      /** [unsigned long] */
| SIGNED LONG INT{ LTint ILong }       /** [long] */
| LONG  INT      { LTint ILong }       /** [long] */
| UNSIGNED LONG INT { LTint IULong }      /** [unsigned long] */
| LONG LONG      { LTint ILongLong }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| SIGNED LONG LONG   { LTint ILongLong }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| UNSIGNED LONG LONG { LTint IULongLong }  /** [unsigned long long]
                                (or [unsigned _int64] on Microsoft Visual C) */
| LONG LONG INT     { LTint ILongLong }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| SIGNED LONG LONG INT  { LTint ILongLong }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| UNSIGNED LONG LONG INT { LTint IULongLong }  /** [unsigned long long]
                                (or [unsigned _int64] on Microsoft Visual C) */
| FLOAT             { LTfloat FFloat }
| DOUBLE            { LTfloat FDouble }
| LONG DOUBLE       { LTfloat FLongDouble }
| STRUCT exit_rt_type identifier_or_typename { LTstruct $3 }
| ENUM   exit_rt_type identifier_or_typename { LTenum $3 }
| UNION  exit_rt_type identifier_or_typename  { LTunion $3 }
| TYPENAME          { LTnamed ($1,[]) }
| TYPENAME LT enter_rt_type  ne_logic_type_list GT exit_rt_type
      { LTnamed($1,$4) }
;

ne_logic_type_list:
| logic_type                          { [$1] }
| logic_type COMMA enter_rt_type ne_logic_type_list { $1 :: $4 }
;

/*** from annotations ***/

full_lexpr:
| enter_kw_c_mode lexpr exit_kw_c_mode { $2 }
;

full_identifier:
| enter_kw_c_mode identifier exit_kw_c_mode { $2 }
;

full_identifier_or_typename:
| enter_kw_c_mode identifier_or_typename exit_kw_c_mode { $2 }
;

full_parameters:
| enter_kw_c_mode ne_parameters exit_kw_c_mode { $2 }
;

full_parameter:
| enter_kw_c_mode parameter exit_kw_c_mode { $2 }
;

full_zones:
| enter_kw_c_mode zones exit_kw_c_mode  { $2 }
;

full_ne_lexpr_list:
enter_kw_c_mode ne_lexpr_list exit_kw_c_mode { $2 }
;

full_logic_type:
| enter_kw_c_mode logic_type exit_kw_c_mode { $2 }
;

full_logic_rt_type:
| enter_kw_c_mode logic_rt_type exit_kw_c_mode { $2 }

full_assigns:
| enter_kw_c_mode assigns exit_kw_c_mode { $2 }
;

/*** ACSL extension for external spec file ***/

ext_spec:
 | ext_global_clauses_opt ext_module_specs_opt ext_global_specs_opt EOF { ("",$1,$2)::$3 }
;

ext_global_clauses_opt:
 | /* empty */         { [] }
 | ext_global_clauses  { $1 }
;

ext_global_clauses:
| ext_global_clause                    { [$1] }
| ext_global_clause ext_global_clauses { $1::$2 }
;

ext_global_clause:
| decl  { Ext_decl (loc_decl $1) }
| EXT_LET any_identifier EQUAL full_lexpr SEMICOLON { Ext_macro ($2, $4) }
| INCLUDE string SEMICOLON { let b,s = $2 in Ext_include(b,s, loc()) }
;

ext_global_specs_opt:
 | /* empty */       { [] }
 | ext_global_specs  { $1 }
;

ext_global_specs:
| ext_global_spec                  { [$1] }
| ext_global_spec ext_global_specs { $1::$2 }
;

ext_global_spec:
| ext_module_markup ext_global_clauses_opt ext_module_specs
    { ($1,$2,$3) }
| ext_module_markup
    { ($1,[],[]) }
;

ext_module_specs_opt:
 | /* empty */      { [] }
 | ext_module_specs { $1 }
;

ext_module_specs:
| ext_module_spec                  { [$1] }
| ext_module_spec ext_module_specs { $1::$2 }
;

ext_module_spec:
| ext_function_markup ext_function_specs_opt { ($1,$2) }
;

ext_function_specs_opt:
| /* empty */         { [] }
| ext_function_specs  { $1 }
;

ext_function_specs:
| ext_at_loop_markup  { []}
| ext_at_stmt_markup  { []}
| ext_function_spec   { [$1] }
| ext_function_spec ext_function_specs { $1::$2 }
;

ext_function_spec:
| ext_global_clause
    { Ext_glob $1 }
| ext_at_loop_markup ext_stmt_loop_spec
    { Ext_loop_spec($1,$2,loc()) }
| ext_at_stmt_markup ext_stmt_loop_spec
    { Ext_stmt_spec($1,$2,loc()) }
| ext_contract_markup contract
    { let s,pos = $2 in Ext_spec (s,pos) }
;

ext_stmt_loop_spec:
| annotation { $1 }
| ext_contract_markup contract { let s, pos = $2 in Acode_annot (pos, AStmtSpec ([],s)) }
;

ext_identifier_opt:
| /* empty*/     { "" }
| ext_identifier { $1 }
;

ext_identifier:
| any_identifier { $1 }
;

ext_module_markup:
| MODULE ext_identifier COLON { $2 }
;

ext_function_markup:
| FUNCTION ext_identifier COLON { $2, loc() }
;

ext_contract_markup:
| CONTRACT ext_identifier_opt COLON { $2 }
;

ext_at_loop_markup:
| EXT_AT LOOP CONSTANT10 COLON { $3 }
;

ext_at_stmt_markup:
| EXT_AT CONSTANT10 COLON     { $2 }
| EXT_AT any_identifier COLON { $2 }
;

/*** function and statement contracts ***/

spec:
| contract EOF { $1 }
;

contract:
| requires terminates decreases simple_clauses behaviors complete_or_disjoint
    { let requires=$1 in
      let (allocation,assigns,post_cond,extended) = $4 in
      let behaviors = $5 in
      let (completes,disjoints) = $6 in
      let behaviors =
        if
          requires <> [] || post_cond <> [] ||
	    allocation <> FreeAllocAny ||
            assigns <> WritesAny || extended <> []
        then
	  let allocation =
	    if allocation <> FreeAllocAny then Some allocation else None
	  in
            (mk_behavior ~requires ~post_cond ~assigns ~allocation
	       ~extended:(wrap_extended extended) ()) :: behaviors
        else behaviors
      in
        { spec_terminates = $2;
          spec_variant = $3;
          spec_behavior = behaviors;
          spec_complete_behaviors = completes;
          spec_disjoint_behaviors = disjoints;
        }, loc()
    }
| requires ne_terminates REQUIRES { clause_order 3 "requires" "terminates" }
| requires terminates ne_decreases REQUIRES
      { clause_order 4 "requires" "decreases" }
| requires terminates ne_decreases TERMINATES
      { clause_order 4 "terminates" "decreases" }
| requires terminates decreases ne_simple_clauses REQUIRES
      { clause_order 5 "requires" "post-condition or assigns" }
| requires terminates decreases ne_simple_clauses TERMINATES
      { clause_order 5 "terminates" "post-condition or assigns" }
| requires terminates decreases ne_simple_clauses DECREASES
      { clause_order 5 "decreases" "post-condition or assigns" }
| requires terminates decreases simple_clauses ne_behaviors TERMINATES
      { clause_order 6 "terminates" "behavior" }
| requires terminates decreases simple_clauses ne_behaviors DECREASES
      { clause_order 6 "decreases" "behavior" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  REQUIRES
      { clause_order 7 "requires" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  TERMINATES
      { clause_order 7 "terminates" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  DECREASES
      { clause_order 7 "decreases" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  BEHAVIOR
      { clause_order 7 "behavior" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  ASSIGNS
      { clause_order 7 "assigns" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  ALLOCATES
      { clause_order 7 "allocates" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  FREES
      { clause_order 7 "frees" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  post_cond_kind
      { clause_order 7 "post-condition" "complete or disjoint" }
;

// use that to detect potentially missing ';' at end of clause
clause_kw:
| REQUIRES { "requires" }
| ASSUMES {"assumes"}
| ASSIGNS { "assigns" }
| post_cond { snd $1 }
| DECREASES { "decreases"}
| BEHAVIOR { "behavior"}
| ALLOCATES {"allocates"}
| FREES {"frees"}
| COMPLETE {"complete"}
| DISJOINT {"disjoint"}
/* often, we'll be in c_kw_mode, where these keywords are
   recognized as identifiers... */
| IDENTIFIER { $1 }
| EOF { "end of annotation" }

requires:
| /* epsilon */ { [] }
| ne_requires { $1 }
;

ne_requires:
| REQUIRES full_lexpr SEMICOLON requires { $2::$4 }
| REQUIRES full_lexpr clause_kw { missing 2 ";" $3}
;

terminates:
| /* epsilon */              { None }
| ne_terminates { Some $1 }
;

ne_terminates:
| TERMINATES full_lexpr SEMICOLON { $2 }
| TERMINATES full_lexpr clause_kw { missing 2 ";" $3 }
;

decreases:
| /* epsilon */   { None }
| ne_decreases { Some $1 }
;

ne_decreases:
| DECREASES variant SEMICOLON { $2 }
| DECREASES variant clause_kw { missing 2 ";" $3 }
;

variant:
| full_lexpr FOR any_identifier { ($1, Some $3) }
| full_lexpr                    { ($1, None) }
;

simple_clauses:
| /* epsilon */ { FreeAllocAny,WritesAny,[],[] }
| ne_simple_clauses { $1 }
;

allocation:
| ALLOCATES full_zones { FreeAlloc([],$2) }
| FREES full_zones { FreeAlloc($2,[]) }

ne_simple_clauses:
| post_cond_kind full_lexpr SEMICOLON simple_clauses
    { let allocation,assigns,post_cond,extended = $4 in allocation,assigns,(($1,$2)::post_cond),extended }
| allocation SEMICOLON simple_clauses
    { let allocation,assigns,post_cond,extended = $3 in
      let a = concat_allocation allocation $1 in
      a,assigns,post_cond,extended
    }
| ASSIGNS full_assigns SEMICOLON simple_clauses
    { let allocation,assigns,post_cond,extended = $4 in
      let a = concat_assigns assigns $2
      in allocation,a,post_cond,extended
    }
| grammar_extension SEMICOLON simple_clauses
    { let allocation,assigns,post_cond,extended = $3 in
      allocation,assigns,post_cond,$1::extended
    }
| post_cond_kind full_lexpr clause_kw { missing 2 ";" $3 }
| allocation clause_kw { missing 1 ";" $2 }
| ASSIGNS full_assigns clause_kw { missing 2 ";" $3 }
| grammar_extension clause_kw { missing 1 ";" $2 }
;

grammar_extension:
/* Grammar Extensibility for plugins */
| grammar_extension_name full_zones { $1,$2 }
;

post_cond_kind:
| post_cond { fst $1 }
;

behaviors:
| /* epsilon */ { [] }
| ne_behaviors { $1 }

ne_behaviors:
| BEHAVIOR behavior_name COLON behavior_body behaviors
      { let (assumes,requires,(allocation,assigns,post_cond,extended)) = $4 in
	let behaviors = $5 in
	let allocation = Some allocation in
	let b =
	  Cil.mk_behavior
            ~name:$2 ~assumes ~requires ~post_cond ~assigns ~allocation
            ~extended:(wrap_extended extended) ()
	in b::behaviors
      }

behavior_body:
| assumes requires simple_clauses { $1,$2,$3 }
| assumes ne_requires ASSUMES
      { clause_order 3 "assumes" "requires" }
| assumes requires ne_simple_clauses ASSUMES
      { clause_order 4 "assumes" "assigns or post-condition" }
| assumes requires ne_simple_clauses REQUIRES
      { clause_order 4 "requires" "assigns or post-condition" }
;

assumes:
| /* epsilon */ { [] }
| ASSUMES full_lexpr SEMICOLON assumes { $2::$4 }
| ASSUMES full_lexpr clause_kw { missing 2 ";" $3 }
;

complete_or_disjoint:
| /* epsilon */ { [],[] }
| ne_complete_or_disjoint { $1 }

ne_complete_or_disjoint:
| COMPLETE BEHAVIORS behavior_name_list SEMICOLON
    complete_or_disjoint
      { let complete,disjoint = $5 in $3::complete, disjoint }
| DISJOINT BEHAVIORS behavior_name_list SEMICOLON
          complete_or_disjoint
      { let complete,disjoint = $5 in complete,$3::disjoint }
/* complete behaviors decreases; is valid (provided there's a behavior
   named decreases)
*/
| COMPLETE BEHAVIORS ne_behavior_name_list clause_kw { missing 3 ";" $4 }
| DISJOINT BEHAVIORS ne_behavior_name_list clause_kw { missing 3 ";" $4 }
;

/*** assigns and tsets ***/

assigns:
| zones { List.map (fun x -> (x,FromAny)) $1 }
| ne_zones FROM zones {List.map (fun x -> (x, From $3)) $1}
/* | ne_zones FROM zones EQUAL lexpr
  { Format.eprintf
      "Warning: functional expression of \\from clause is ignored (not yet implemented)." ;
    List.map (fun x -> (x, $3)) $1
  }*/
;

zones:
| ne_zones { $1 }
| NOTHING  { [] }
;

ne_zones:
| ne_lexpr_list { $1 }
;

/*** annotations ***/

annot:
| annotation EOF  { $1 }
| is_spec any EOF { Aspec }
| decl_list EOF   { Adecl ($1) }
| CUSTOM any_identifier COLON custom_tree EOF { Acustom(loc (),$2, $4) }
;

custom_tree:
| TYPE type_spec  { CustomType $2 }
| LOGIC lexpr %prec prec_named    { CustomLexpr $2 }
| any_identifier_non_logic %prec lowest { CustomOther($1,[]) }
| any_identifier_non_logic LPAR custom_tree_list RPAR %prec lowest { CustomOther($1,$3) }
;

custom_tree_list:
| custom_tree   { [$1] }
| custom_tree COMMA custom_tree_list  { $1::$3 }

annotation:
| loop_annotations
      { let (b,v,p) = $1 in
	(* TODO: do better, do not lose the structure ! *)
	let l = b@v@p in
        Aloop_annot (loc (), l) }
| FOR ne_behavior_name_list COLON contract
      { let s, pos = $4 in Acode_annot (pos, AStmtSpec ($2,s)) }
| code_annotation { Acode_annot (loc(),$1) }
| code_annotation beg_code_annotation
      { raise
          (Not_well_formed (loc(),
                            "Only one code annotation is allowed per comment"))
      }
| full_identifier_or_typename { Aattribute_annot (loc (), $1) }
;

/*** loop annotations ***/

loop_annotations:
| loop_annot_stack
    { let (i,fa,a,b,v,p) = $1 in
      let invs = List.map (fun i -> AInvariant([],true,i)) i in
      let oth = match a with
        | WritesAny -> b
        | Writes _ ->
            (* by definition all existing AAssigns are tied to at least
               one behavior. No need to merge against them. *)
            AAssigns ([],a)::b
      in
      let oth = match fa with
        | FreeAllocAny -> oth
        | _ -> AAllocation ([],fa)::oth
      in
	(invs@oth,v,p)
    }
;

/* TODO: gather loop assigns that are related to the same behavior */
loop_annot_stack:
| loop_invariant loop_annot_opt
    { let (i,fa,a,b,v,p) = $2 in ($1::i,fa,a,b,v,p) }
| loop_effects loop_annot_opt
    { let (i,fa,a,b,v,p) = $2 in (i,fa,concat_assigns a $1,b,v,p) }
| loop_allocation loop_annot_opt
    { let (i,fa,a,b,v,p) = $2 in (i,concat_allocation fa $1,a,b,v,p) }
| FOR ne_behavior_name_list COLON loop_annot_stack
    { let (i,fa,a,b,v,p) = $4 in
      let behav = $2 in
      let invs = List.map (fun i -> AInvariant(behav,true,i)) i in
      let oth = concat_loop_assigns_allocation b behav a fa in
      ([],FreeAllocAny,WritesAny,invs@oth,v,p)
    }
| loop_variant loop_annot_opt
    { let pos,loop_variant = $1 in
      let (i,fa,a,b,v,p) = $2 in
      check_empty
        (pos,"loop invariant is not allowed after loop variant.") i ;
      (match fa with
        | FreeAlloc(f,a) ->
	    check_empty
              (pos,"loop frees is not allowed after loop variant.") f ;
	    check_empty
              (pos,"loop allocates is not allowed after loop variant.") a
        | FreeAllocAny -> ());
      (match a with
          WritesAny -> ()
        | Writes _ ->
          raise
            (Not_well_formed
               (pos,"loop assigns is not allowed after loop variant.")));
      check_empty
        (pos,"loop behavior is not allowed after loop variant.") b ;
      check_empty
        (pos,"loop annotations can have at most one variant.") v ;
      (i,fa,a,b,AVariant loop_variant::v,p) }
| loop_pragma loop_annot_opt
    { let (i,fa,a,b,v,p) = $2 in (i,fa,a,b,v,APragma (Loop_pragma $1)::p) }
| loop_grammar_extension loop_annot_opt {
    raise
    (Not_well_formed
       (loc(),"Grammar extension for loop annotations is not yet implemented"))
  }
;

loop_annot_opt:
| /* epsilon */
    { ([], FreeAllocAny, WritesAny, [], [], []) }
| loop_annot_stack
    { $1 }
;

loop_effects:
| LOOP ASSIGNS full_assigns SEMICOLON { $3 }
;

loop_allocation:
| LOOP allocation SEMICOLON { $2 }
;

loop_invariant:
| LOOP INVARIANT full_lexpr SEMICOLON { $3 }
;

loop_variant:
| LOOP VARIANT variant SEMICOLON { loc(),$3 }
;

/* Grammar Extensibility for plugins */
loop_grammar_extension:
| LOOP grammar_extension SEMICOLON {
    raise (Not_well_formed (loc(),"Grammar extension for loop annotations is not yet implemented"))
  }
;

loop_pragma:
| LOOP PRAGMA any_identifier full_ne_lexpr_list SEMICOLON
  { if $3 = "UNROLL_LOOP" || $3 = "UNROLL" then
      (if $3 <> "UNROLL" then
	 Format.eprintf "Warning: use of deprecated keyword '%s'.\nShould use 'UNROLL' instead.@." $3;
       Unroll_specs $4)
    else if $3 = "WIDEN_VARIABLES" then
      Widen_variables $4
    else if $3 = "WIDEN_HINTS" then
      Widen_hints $4
    else raise (Not_well_formed (loc(),"Unknown loop pragma")) }
;

/*** code annotations ***/

beg_code_annotation:
| IMPACT {}
| SLICE {}
| FOR {}
| ASSERT {}
| INVARIANT {}
;

code_annotation:
| slice_pragma     { APragma (Slice_pragma $1) }
| impact_pragma    { APragma (Impact_pragma $1) }
| FOR ne_behavior_name_list COLON ASSERT full_lexpr SEMICOLON
      { AAssert ($2,$5) }
| FOR ne_behavior_name_list COLON INVARIANT full_lexpr SEMICOLON
      { AInvariant ($2,false,$5) }
| ASSERT full_lexpr SEMICOLON    { AAssert ([],$2) }
| INVARIANT full_lexpr SEMICOLON { AInvariant ([],false,$2) }
;

slice_pragma:
| SLICE PRAGMA any_identifier full_lexpr SEMICOLON
    { if $3 = "expr" then SPexpr $4
      else raise (Not_well_formed (loc(), "Unknown slice pragma")) }
| SLICE PRAGMA any_identifier SEMICOLON
    { if $3 = "ctrl" then SPctrl
      else if $3 = "stmt" then SPstmt
      else raise (Not_well_formed (loc(), "Unknown slice pragma")) }
;

impact_pragma:
| IMPACT PRAGMA any_identifier full_lexpr SEMICOLON
    { if $3 = "expr" then IPexpr $4
      else raise (Not_well_formed (loc(), "Unknown impact pragma")) }
| IMPACT PRAGMA any_identifier SEMICOLON
    { if $3 = "stmt" then IPstmt
      else raise (Not_well_formed (loc(), "Unknown impact pragma")) }
;

/*** declarations and logical definitions ***/

decl_list:
| decl            { [loc_decl $1] }
| decl decl_list  { (loc_decl $1) :: $2 }

decl:
| GLOBAL INVARIANT any_identifier COLON full_lexpr SEMICOLON
    { LDinvariant ($3, $5) }
| VOLATILE ne_zones volatile_opt SEMICOLON { LDvolatile ($2, $3) }
| type_annot {LDtype_annot $1}
| model_annot {LDmodel_annot $1}
| logic_def  { $1 }
| deprecated_logic_decl { $1 }
;

volatile_opt:
| /* empty */ { None, None }
| READS any_identifier volatile_opt
              { let read,write=$3 in
                  if read = None then
		    (Some $2),write
		  else
                    (Format.eprintf "Warning: read %s ignored@." $2; $3)
	      }
| WRITES any_identifier volatile_opt
              { let read,write=$3 in
                  if write = None then
		    read,(Some $2)
		  else
                    (Format.eprintf "Warning: write %s ignored@." $2; $3)
	      }
;

type_annot:
| TYPE INVARIANT any_identifier LPAR full_parameter RPAR EQUAL
    full_lexpr SEMICOLON
  { let typ,name = $5 in{ inv_name = $3; this_name = name; this_type = typ; inv = $8; } }
;

opt_semicolon:
| /* epsilon */ { }
| SEMICOLON { }

model_annot:
| MODEL type_spec LBRACE full_parameter opt_semicolon RBRACE SEMICOLON
  { let typ,name = $4 in
    { model_for_type = $2; model_name = name; model_type = typ; }
  }
;

poly_id_type:
| full_identifier
    { enter_type_variables_scope []; ($1,[]) }
| full_identifier LT ne_tvar_list GT
        { enter_type_variables_scope $3; ($1,$3) }
;

/* we need to recognize the typename as soon as it has been declared,
  so that it can be used in data constructors in the type definition itself
*/
poly_id_type_add_typename:
| poly_id_type { let (id,_) = $1 in Logic_env.add_typename id; $1 }
;

poly_id:
| poly_id_type { let (id,tvar) = $1 in (id,[],tvar) }
| full_identifier LBRACE ne_label_list RBRACE
      { enter_type_variables_scope []; ($1,$3,[]) }
| full_identifier LBRACE ne_label_list RBRACE LT ne_tvar_list GT
      { enter_type_variables_scope $6; $1,$3,$6 }
;

opt_parameters:
| /*epsilon*/ { [] }
| parameters { $1 }
;

parameters:
| LPAR full_parameters RPAR { $2 }
;

logic_def:
/* logic function definition */
| LOGIC full_logic_rt_type poly_id opt_parameters EQUAL full_lexpr SEMICOLON
    { let (id, labels, tvars) = $3 in
      exit_type_variables_scope ();
      LDlogic_def (id, labels, tvars, $2, $4, $6) }
/* predicate definition */
| PREDICATE poly_id opt_parameters EQUAL full_lexpr SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDpredicate_def (id, labels, tvars, $3, $5) }
/* inductive predicate definition */
| INDUCTIVE poly_id parameters LBRACE indcases RBRACE
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDinductive_def(id, labels, tvars, $3, $5) }
| LEMMA poly_id COLON full_lexpr SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDlemma (id, false, labels, tvars, $4) }
| AXIOMATIC any_identifier LBRACE logic_decls RBRACE
    { LDaxiomatic($2,$4) }
| TYPE poly_id_type_add_typename EQUAL typedef SEMICOLON
        { let (id,tvars) = $2 in
          exit_type_variables_scope ();
          LDtype(id,tvars,Some $4)
        }
;

deprecated_logic_decl:
/* OBSOLETE: logic function declaration */
| LOGIC full_logic_rt_type poly_id opt_parameters SEMICOLON
    { let (id, labels, tvars) = $3 in
      let source = fst (loc ()) in
      exit_type_variables_scope ();
      obsolete  "logic declaration" ~source ~now:"an axiomatic block";
      LDlogic_reads (id, labels, tvars, $2, $4, None) }
/* OBSOLETE: predicate declaration */
| PREDICATE poly_id opt_parameters SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      let source = fst (loc ()) in
      obsolete "logic declaration" ~source ~now:"an axiomatic block";
      LDpredicate_reads (id, labels, tvars, $3, None) }
/* OBSOLETE: type declaration */
| TYPE poly_id_type SEMICOLON
    { let (id,tvars) = $2 in
      Logic_env.add_typename id;
      exit_type_variables_scope ();
      let source = fst (loc ()) in
      obsolete "logic type declaration" ~source ~now:"an axiomatic block";
      LDtype(id,tvars,None)
    }
/* OBSOLETE: axiom */
| AXIOM poly_id COLON full_lexpr SEMICOLON
    { let (id,_,_) = $2 in
      raise
	(Not_well_formed
	   (loc(),"Axiom " ^ id ^ " is declared outside of an axiomatic."))
    }
;


logic_decls:
| /* epsilon */
    { [] }
| logic_decl_loc logic_decls
    { $1::$2 }
;

logic_decl:
| logic_def  { $1 }
/* logic function declaration */
| LOGIC full_logic_rt_type poly_id opt_parameters reads_clause SEMICOLON
    { let (id, labels, tvars) = $3 in
      exit_type_variables_scope ();
      LDlogic_reads (id, labels, tvars, $2, $4, $5) }
/* predicate declaration */
| PREDICATE poly_id opt_parameters reads_clause SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDpredicate_reads (id, labels, tvars, $3, $4) }
/* type declaration */
| TYPE poly_id_type SEMICOLON
    { let (id,tvars) = $2 in
      Logic_env.add_typename id;
      exit_type_variables_scope ();
      LDtype(id,tvars,None) }
/* axiom */
| AXIOM poly_id COLON full_lexpr SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDlemma (id, true, labels, tvars, $4) }
;

logic_decl_loc:
| logic_decl { loc_decl $1 }
;


reads_clause:
| /* epsilon */ { None }
| READS zones { Some $2 }
;

typedef:
| ne_datacons_list { TDsum $1 }
| full_logic_type { TDsyn $1 }
;

datacons_list:
| /* epsilon */ { [] }
| PIPE datacons datacons_list { $2 :: $3 }
;

ne_datacons_list:
| datacons datacons_list { $1 :: $2 }
| PIPE datacons datacons_list { $2 :: $3 }
;

datacons:
| full_identifier { ($1,[]) }
| full_identifier LPAR ne_type_list RPAR { ($1,$3) }
;

ne_type_list:
| full_logic_type { [$1] }
| full_logic_type COMMA ne_type_list { $1::$3 }

indcases:
| /* epsilon */
    { [] }
| CASE poly_id COLON full_lexpr SEMICOLON indcases
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      (id,labels,tvars,$4)::$6 }
;


ne_tvar_list:
| full_identifier                    { [$1] }
| full_identifier COMMA ne_tvar_list { $1 :: $3 }
;

ne_label_list:
| label_name                     { [$1] }
| label_name COMMA ne_label_list { $1 :: $3 }
;

opt_label_1:
| opt_label_list { match $1 with
		     | [] -> None
		     | l::[] -> Some l
		     | _ -> raise (Not_well_formed (loc(),"Only one label is allowed")) }
;

opt_label_2:
| opt_label_list { match $1 with
		     | [] -> None
		     | l1::l2::[] -> Some (l1,l2)
		     | _::[] -> raise (Not_well_formed (loc(),"One label is missing"))
		     | _ -> raise (Not_well_formed (loc(),"Only two labels are allowed")) }
;

opt_label_list:
| /* epsilon */               { [] }
| LBRACE ne_label_list RBRACE { $2 }
;

/* names */
label_name:
| any_identifier { $1 }
;

behavior_name_list:
| /* epsilon */         { [] }
| ne_behavior_name_list { $1 }
;

ne_behavior_name_list:
| behavior_name                             { [$1] }
| behavior_name COMMA ne_behavior_name_list {$1 :: $3}
;

behavior_name:
| any_identifier { $1 }
;

any_identifier:
| identifier_or_typename { $1 }
| keyword { $1 }
;

any_identifier_non_logic:
| identifier_or_typename { $1 }
| non_logic_keyword { $1 }

identifier_or_typename:
| IDENTIFIER { $1 }
| TYPENAME { $1 }
;

identifier:
| IDENTIFIER { $1 }
;

bounded_var:
| identifier { $1 }
| TYPENAME  /* Since TYPENAME cannot be accepted by lexpr rule */
    { raise
	(Not_well_formed(loc (),
			 "Type names are not allowed as binding variable"))
    }
;

c_keyword:
| CASE { "case" }
| CHAR { "char" }
| BOOLEAN { "boolean" }
| BOOL { "_Bool" }
| CONST { "const" }
| DOUBLE { "double" }
| ELSE { "else" }
| ENUM { "enum" }
| FLOAT { "float" }
| IF { "if" }
| INT { "int" }
| LONG { "long" }
| SHORT { "short" }
| SIGNED { "signed" }
| SIZEOF { "sizeof" }
| STATIC { "static" }
| STRUCT { "struct" }
| UNION { "union" }
| UNSIGNED { "unsigned" }
| VOID { "void" }
;

acsl_c_keyword:
| FOR { "for" }
| VOLATILE { "volatile" }
;

post_cond:
| ENSURES { Normal, "normal" }
| EXITS   { Exits, "exits" }
| BREAKS  { Breaks, "breaks" }
| CONTINUES { Continues, "continues" }
| RETURNS { Returns, "returns" }
;

is_acsl_spec:
| post_cond  { snd $1 }
| ASSIGNS    { "assigns" }
| ALLOCATES  { "allocates" }
| FREES      { "frees" }
| BEHAVIOR   { "behavior" }
| REQUIRES   { "requires" }
| TERMINATES { "terminates" }
| COMPLETE   { "complete" }
| DECREASES  { "decreases" }
| DISJOINT   { "disjoint" }
;

is_acsl_decl_or_code_annot:
| ASSERT    { "assert" }
| ASSUMES   { "assumes" }
| GLOBAL    { "global" }
| IMPACT    { "impact" }
| INDUCTIVE { "inductive" }
| INVARIANT { "invariant" }
| LEMMA     { "lemma" }
| LOOP      { "loop" }
| PRAGMA    { "pragma" }
| PREDICATE { "predicate" }
| SLICE     { "slice" }
| TYPE      { "type" }
| MODEL     { "model" }
| AXIOM     { "axiom" }
| VARIANT   { "variant" }
| AXIOMATIC { "axiomatic" }
;

is_acsl_other:
| BEHAVIORS { "behaviors" }
| INTEGER { "integer" }
| LABEL { "label" }
| READS { "reads" }
| REAL { "real" }
| WRITES { "writes" }
;

is_ext_spec:
| CONTRACT { "contract" }
| FUNCTION { "function" }
| MODULE   { "module" }
| INCLUDE  { "include" }
| EXT_AT   { "at" }
| EXT_LET  { "let" }
;

keyword:
| LOGIC     { "logic" }
| non_logic_keyword { $1 }

non_logic_keyword:
| c_keyword      { $1 }
| acsl_c_keyword { $1 }
| is_ext_spec    { $1 }
| is_acsl_spec   { $1 }
| is_acsl_decl_or_code_annot { $1 }
| is_acsl_other  { $1 }
| CUSTOM { "custom" }
;

/* ACSL extension language */
grammar_extension_name:
| full_identifier_or_typename { check_registered $1 }
| is_acsl_other { check_registered $1 }
| c_keyword     { check_registered $1 }
;

/* Spec are parsed after the function prototype itself. This rule distinguishes
   between spec and other annotations by the first keyword of the annotation.
   in order to return the appropriate token in clexer.mll
*/
is_spec:
| is_acsl_spec { () }
| grammar_extension_name { () } /* ACSL extension language */
;

bs_keyword:
| ALLOCABLE { () }
| ALLOCATION { () }
| AUTOMATIC { () }
| AT { () }
| BASE_ADDR { () }
| BLOCK_LENGTH { () }
| DYNAMIC { () }
| EMPTY { () }
| FALSE { () }
| FORALL { () }
| FREEABLE { () }
| FRESH { () }
| FROM { () }
| INTER { () }
| LAMBDA { () }
| LET { () }
| NOTHING { () }
| NULL { () }
| OLD { () }
| OFFSET { () }
| REGISTER { () }
| RESULT { () }
| SEPARATED { () }
| TRUE { () }
| BSTYPE { () }
| TYPEOF { () }
| BSUNION { () }
| UNALLOCATED { () }
| VALID { () }
| VALID_INDEX { () }
| VALID_RANGE { () }
| VALID_READ { () }
| INITIALIZED { () }
| DANGLING { () }
| WITH { () }
;

wildcard:
| any_identifier { () }
| bs_keyword { () }
| AMP { () }
| AND { () }
| ARROW { () }
| BIFF { () }
| BIMPLIES { () }
| COLON { () }
| COLON2 { () }
| COLONCOLON { () }
| COLONGT { () }
| COMMA { () }
| CONSTANT { () }
| CONSTANT10 { () }
| DOLLAR { () }
| DOT { () }
| DOTDOT { () }
| DOTDOTDOT { () }
| EQ { () }
| EQUAL { () }
| EXISTS { () }
| GE { () }
| GHOST { () }
| GT { () }
| GTGT { () }
| HAT { () }
| HATHAT { () }
| IFF { () }
| IMPLIES { () }
| LBRACE { () }
| LE { () }
| LPAR { () }
| LSQUARE { () }
| LT { () }
| LTCOLON { () }
| LTLT { () }
| MINUS { () }
| NE { () }
| NOT { () }
| OR { () }
| PERCENT { () }
| PIPE { () }
| PLUS { () }
| QUESTION { () }
| RBRACE { () }
| RPAR { () }
| RSQUARE { () }
| SEMICOLON { () }
| SLASH { () }
| STAR { () }
| STRING_LITERAL { () }
| TILDE { () }
;

any:
| wildcard { () }
| wildcard any { () }
;

%%

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
