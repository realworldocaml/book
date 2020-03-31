(* Original file: zenon.0.8.4/zenon-0.8.4/parsecoq.mly *)
/*  Copyright 2005 INRIA  */

%{

open Printf;;

open Expr;;
open Namespace;;
open Phrase;;

let rec mk_type_string e =
  match e with
  | Evar (s, _) -> s
  | Emeta _ -> assert false
  | Eapp ("*", [e1; e2], _) ->
     sprintf "(%s*%s)" (mk_type_string e1) (mk_type_string e2)
  | Eapp ("%", [e1; e2], _) ->
     sprintf "((%s)%%%s)" (mk_type_string e1) (mk_type_string e2)
  | Eapp (s, args, _) ->
     let inside =
       List.fold_left (fun s a -> sprintf "%s %s" s (mk_type_string a)) s args
     in
     sprintf "(%s)" inside
  | Eimply (e1, e2, _) ->
     sprintf "(%s -> %s)" (mk_type_string e1) (mk_type_string e2)
  | _ -> assert false (* FIXME TODO *)
;;

let mk_eapp (s, args) =
  match (s, args) with
  | "and", [e1; e2] -> eand (e1, e2)
  | "or", [e1; e2] -> eor (e1, e2)
  | "not", [e1] -> enot (e1)
  | _ -> eapp (s, args)
;;

let mk_apply (e, l) =
  match e with
  | Eapp (s, args, _) -> mk_eapp (s, args @ l)
  | Evar (s, _) -> mk_eapp (s, l)
  | _ -> raise Parse_error
;;

let rec mk_arobas_apply (id, l) =
  match l with
  | Evar ("_", _) :: t -> mk_arobas_apply (id, t)
  | [] -> evar (id)
  | _ -> eapp (id, l)
;;

let mk_all bindings body =
  let f (var, ty) e = eall (evar var, ty, e) in
  List.fold_right f bindings body
;;

let mk_ex bindings body =
  let f (var, ty) e = eex (evar var, ty, e) in
  List.fold_right f bindings body
;;

let mk_lam bindings body =
  let f (var, ty) e = elam (evar var, ty, e) in
  List.fold_right f bindings body
;;

let mk_fix ident ty bindings body =
  let f (var, ty) e = elam (evar var, ty, e) in
  (ident, eapp ("$fix", [ List.fold_right f ((ident, ty) :: bindings) body ]))
;;

let rec get_params e =
  match e with
  | Elam (v, _, body, _) ->
      let (p, e1) = get_params body in
      (v :: p, e1)
  | _ -> ([], e)
;;

let mk_let id expr body =
  substitute_2nd [(evar id, expr)] body
;;

let mk_let_fix (id, def) body = mk_let id def body;;

let mk_pattern (constr, args) body =
  let bindings = List.map (fun v -> (v, "")) args in
  mk_lam bindings (eapp ("$match-case", [evar (constr); body]))
;;

let mk_inductive name bindings constrs =
  let args = List.map fst bindings in
  let g (tcon, targs) =
    if tcon = name && targs = args then Self
    else Param (String.concat " " (tcon :: targs))
  in
  let f (cname, args) = (cname, List.map g args) in
  Inductive (name, args, List.map f constrs, name ^ "_ind")
;;

let mk_pairs e l =
  let f x y = eapp ("@", [evar "Datatypes.pair"; evar "_"; evar "_"; x; y]) in
  List.fold_left f e l
;;

let mk_string s = evar ("\"" ^ s ^ "\"") ;;
%}

%token <string> IDENT
%token <string> STRING
%token <string> NUM

%token BANG_
%token PERCENT_
%token AMPER_
%token AMPER_AMPER_
%token LPAREN_
%token LPAREN_RPAREN_
%token RPAREN_
%token STAR_
%token PLUS_
%token PLUS_PLUS_
%token COMMA_
%token DASH_
%token DASH_GT_
%token PERIOD_
%token PERIOD_LPAREN_
%token PERIOD_PERIOD_
%token SLASH_
%token SLASH_BACKSL_
%token COLON_
%token COLON_COLON_
%token COLON_LT_
%token COLON_EQ_
%token COLON_GT_
%token SEMI_
%token LT_
%token LT_DASH_
%token LT_DASH_GT_
%token LT_COLON_
%token LT_EQ_
%token LT_GT_
%token EQ_
%token EQ_GT_
%token EQ_UNDER_D_
%token GT_
%token GT_DASH_GT_
%token GT_EQ_
%token QUEST_
%token QUEST_EQ_
%token AROBAS_
%token LBRACK_
%token BACKSL_SLASH_
%token RBRACK_
%token HAT_
%token LBRACE_
%token BAR_
%token BAR_DASH_
%token BAR_BAR_
%token RBRACE_
%token TILDE_

%token MUSTUSE

%token DEFINITION
%token DEPENDS
%token ELSE
%token END
%token EXISTS
%token FALSE
%token FIX
%token FIXPOINT
%token FORALL
%token FUN
%token FUNCTION
%token IF
%token IN
%token INDUCTIVE
%token LET
%token MATCH
%token ON
%token PARAMETER
%token STRUCT
%token THEN
%token THEOREM
%token TRUE
%token WITH

%token BEGINPROOF
%token <string> BEGINNAME
%token BEGINHEADER
%token ENDPROOF

%token EOF

%nonassoc let_in
%nonassoc IDENT FQN
%right COMMA_
%nonassoc FORALL EXISTS FUN EQ_GT_ IF THEN ELSE IN
%left LT_DASH_GT_
%right DASH_GT_
%right BACKSL_SLASH_
%right SLASH_BACKSL_
%nonassoc EQ_ LT_GT_
%nonassoc TILDE_
%left apply

%start file
%type <string * (Phrase.phrase * bool) list> file

%%

file:
  | hyp_def_list THEOREM IDENT COLON_ expr PERIOD_ EOF
      { ($3, (Hyp (goal_name, enot $5, 0), false) :: $1) }
  | proof_head hyp_def_list THEOREM IDENT COLON_ expr PERIOD_ ENDPROOF EOF
      { ($4, (Hyp (goal_name, enot $6, 0), false) :: $2) }
  | expr hyp_def_list EOF
      { (* Error.warn "deprecated input format"; *)
        (thm_default_name, (Hyp (goal_name, enot $1, 0), false) :: $2) }
  | proof_head expr hyp_def_list ENDPROOF EOF
      { (* Error.warn "deprecated input format"; *)
        ($1, (Hyp (goal_name, enot $2, 0), false) :: $3) }
;

proof_head:
  | BEGINPROOF proofheaders BEGINNAME proofheaders
      { $3 }
  | BEGINPROOF proofheaders
      { "theorem" }
;

proofheaders:
  | /* empty */
      { () }
  | BEGINHEADER proofheaders
      { () }
;

expr:
  | FORALL bindings COMMA_ expr
      { mk_all $2 $4 }
  | EXISTS bindings COMMA_ expr
      { mk_ex $2 $4 }

  | FUN bindings EQ_GT_ expr
      { mk_lam $2 $4 }

  | LET fix IN expr %prec let_in
      { mk_let_fix $2 $4 }

  | LET IDENT COLON_EQ_ expr IN expr %prec let_in
      { mk_let $2 $4 $6 }

  | LET LPAREN_ IDENT COLON_ junk RPAREN_ COLON_EQ_ expr IN expr %prec let_in
      { mk_let $3 $8 $10 }

  | LET IDENT COLON_ junk COLON_EQ_ expr IN expr %prec let_in
      { mk_let $2 $6 $8 }

  | MATCH expr WITH pat_expr_list END
      { eapp ("$match", $2 :: $4) }

  | IF expr THEN expr ELSE expr
      { eapp ("FOCAL.ifthenelse", [$2; $4; $6]) }

  | expr DASH_GT_ expr
      { eimply ($1, $3) }

  | expr LT_DASH_GT_ expr
      { eequiv ($1, $3) }

  | expr BACKSL_SLASH_ expr
      { eor ($1, $3) }

  | expr SLASH_BACKSL_ expr
      { eand ($1, $3) }

  | expr EQ_ expr
      { eapp ("=", [$1; $3]) }
  | expr LT_GT_ expr
      { enot (eapp ("=", [$1; $3])) }

  | TILDE_ expr
      { enot ($2) }

  | expr1 expr1_list  %prec apply
      { mk_apply ($1, $2) }

  | AROBAS_ IDENT expr1_list %prec apply
      { mk_eapp ("@", evar ($2) :: $3) }

  | AROBAS_ IDENT %prec apply
      { mk_eapp ("@", [evar ($2)]) }

  | expr1
      { $1 }
;

fix:
  | FIX IDENT bindings LBRACE_ STRUCT IDENT RBRACE_ COLON_ typ COLON_EQ_ expr
      { mk_fix $2 $9 $3 $11 }
;

expr1:
  | IDENT
      { evar ($1) }
  | NUM
      { eapp ($1, []) }
  /* Added F. Pessaux, Makes Zenon parsing strings and considering them like
     simple atoms. */
  | STRING
      { eapp ("$string", [mk_string $1]) }
  | LPAREN_ expr comma_expr_list RPAREN_
      { mk_pairs $2 $3 }
  | LPAREN_ expr STAR_ expr RPAREN_
      { eapp ("*", [$2; $4]) }
  | LPAREN_ expr PERCENT_ IDENT RPAREN_
      { eapp ("%", [$2; evar ($4)]) }
  | LPAREN_ expr RPAREN_
      { $2 }
  | TRUE
      { etrue }
  | FALSE
      { efalse }
;

expr1_list:
  | expr1                  { [$1] }
  | expr1 expr1_list       { $1 :: $2 }
;

comma_expr_list:
  | COMMA_ expr
      { [$2] }
  | COMMA_ expr comma_expr_list
      { $2 :: $3 }
;

pat_expr_list:
  | /* empty */
      { [] }
  | BAR_ pattern EQ_GT_ expr pat_expr_list
      { mk_pattern $2 $4 :: $5 }
;

pattern:
  | LPAREN_ pattern RPAREN_
      { $2 }
  | IDENT idlist
      { ($1, $2) }
  | LPAREN_ IDENT COMMA_ IDENT RPAREN_
      { ("Datatypes.pair", [$2; $4]) }
;

bindings:
  | simplebinding          { $1 }
  | binding_list           { $1 }
;

simplebinding:
  | idlist COLON_ typ  { List.map (fun v -> (v, $3)) $1 }
;

idlist:
  | /* empty */            { [] }
  | IDENT idlist           { $1 :: $2 }
;

binding_list:
  | /* empty */
      { [] }
  | IDENT binding_list
      { ($1, "") :: $2 }
  | LPAREN_ simplebinding RPAREN_ binding_list
      { $2 @ $4 }
;

typ:
  | expr                   { mk_type_string $1 }
;

/* normal identifier or unparsed  expression */
id_or_expr:
  | IDENT  { $1 }
  | STRING { $1 }
;


hyp_def:
  | PARAMETER id_or_expr COLON_ expr PERIOD_
      { Hyp ($2, $4, 1) }
  | DEFINITION id_or_expr COLON_EQ_ expr PERIOD_
      { let (params, expr) = get_params $4 in
        Def (DefReal ($2, $2, params, expr, None)) }
  | DEFINITION IDENT compact_args COLON_ typ COLON_EQ_ expr PERIOD_
      {
       let compact_params = $3 in
       let (other_params, expr) = get_params $7 in
       Def (DefReal ($2, $2, (compact_params @ other_params), expr, None))
      }
  | FIXPOINT IDENT compact_args LBRACE_ STRUCT IDENT RBRACE_
             COLON_ typ COLON_EQ_ expr PERIOD_
      {
       let compact_params = $3 in
       let (other_params, expr) = get_params $11 in
       Def (DefReal ($2, $2, (compact_params @ other_params), expr, Some $6))
      }
  | FUNCTION IDENT compact_args COLON_ typ LBRACE_ expr RBRACE_ COLON_EQ_
    expr PERIOD_
      { Def (DefRec ($7, $2, $3, $10)) }
  | INDUCTIVE IDENT binding_list COLON_ IDENT COLON_EQ_ constr_list PERIOD_
      { mk_inductive $2 $3 $7 }
;


compact_args:
    /* empty */                                          { [] }
  | LPAREN_ IDENT COLON_ typ RPAREN_ compact_args    { (evar $2) :: $6 }
;


dep_hyp_def:
  | DEPENDS ON hyp_def         {
      (* FIXME activate this warning at some point.
      Error.warn "use of obsolete \"Depends on\" syntax";
      *)
      ($3, true)
    }
  | MUSTUSE hyp_def            { ($2, true) }
  | hyp_def                    { ($1, false) }
;

hyp_def_list:
  | dep_hyp_def hyp_def_list   { $1 :: $2 }
  | /* empty */                { [] }
;

constr_list:
  | BAR_ IDENT COLON_ constr_type constr_list
      { ($2, $4) :: $5 }
  | /* empty */
      { [] }
;

constr_type:
  | arg_type                          { [] }
  | arg_type DASH_GT_ constr_type     { $1 :: $3 }
  | LPAREN_ arg_type DASH_GT_ constr_type RPAREN_
                                      { $2 :: $4 }
;

arg_type:
  | LPAREN_ arg_type RPAREN_          { $2 }
  | IDENT idlist                      { ($1, $2) }
;

junk:
  | /* empty */                       { () }
  | IDENT junk                        { () }
  | STAR_ junk                        { () }
  | PERCENT_ junk                     { () }
  | NUM junk                          { () }
  | LPAREN_RPAREN_ junk               { () }
  | PERIOD_ junk                      { () }
  | COLON_ junk                       { () }
  | PERIOD_LPAREN_ junk RPAREN_ junk  { () }
  | LPAREN_ junk RPAREN_ junk         { () }
;

%%
