(* Original file: why3.1.1.0/why3-1.1.0/plugins/python/py_parser.mly *)
(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2018   --   Inria - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

%{
  open Why3
  open Ptree
  open Py_ast

  let () = Exn_printer.register (fun fmt exn -> match exn with
    | Error -> Format.fprintf fmt "syntax error"
    | _ -> raise exn)

  let floc s e = Loc.extract (s,e)
  let mk_id id s e = { id_str = id; id_ats = []; id_loc = floc s e }
  let mk_pat  d s e = { pat_desc  = d; pat_loc  = floc s e }
  let mk_term d s e = { term_desc = d; term_loc = floc s e }
  let mk_expr loc d = { expr_desc = d; expr_loc = loc }
  let mk_stmt loc d = Dstmt { stmt_desc = d; stmt_loc = loc }

  let variant_union v1 v2 = match v1, v2 with
    | _, [] -> v1
    | [], _ -> v2
    | _, ({term_loc = loc},_)::_ -> Loc.errorm ~loc
        "multiple `variant' clauses are not allowed"

  let get_op s e = Qident (mk_id (Ident.op_get "") s e)
  let upd_op s e = Qident (mk_id (Ident.op_update "") s e)

  let empty_spec = {
    sp_pre     = [];    sp_post    = [];  sp_xpost  = [];
    sp_reads   = [];    sp_writes  = [];  sp_alias  = [];
    sp_variant = [];
    sp_checkrw = false; sp_diverge = false; sp_partial = false;
  }

  let spec_union s1 s2 = {
    sp_pre     = s1.sp_pre @ s2.sp_pre;
    sp_post    = s1.sp_post @ s2.sp_post;
    sp_xpost   = s1.sp_xpost @ s2.sp_xpost;
    sp_reads   = s1.sp_reads @ s2.sp_reads;
    sp_writes  = s1.sp_writes @ s2.sp_writes;
    sp_alias   = s1.sp_alias @ s2.sp_alias;
    sp_variant = variant_union s1.sp_variant s2.sp_variant;
    sp_checkrw = s1.sp_checkrw || s2.sp_checkrw;
    sp_diverge = s1.sp_diverge || s2.sp_diverge;
    sp_partial = s1.sp_partial || s2.sp_partial;
  }

%}

%token <string> INTEGER
%token <string> STRING
%token <Py_ast.binop> CMP
%token <string> IDENT
%token DEF IF ELSE ELIF RETURN WHILE FOR IN AND OR NOT NONE TRUE FALSE
%token FROM IMPORT BREAK
%token EOF
%token LEFTPAR RIGHTPAR LEFTSQ RIGHTSQ COMMA EQUAL COLON BEGIN END NEWLINE
%token PLUS MINUS TIMES DIV MOD
(* annotations *)
%token INVARIANT VARIANT ASSUME ASSERT CHECK REQUIRES ENSURES LABEL
%token FUNCTION PREDICATE
%token ARROW LARROW LRARROW FORALL EXISTS DOT THEN LET

(* precedences *)

%nonassoc IN
%nonassoc DOT ELSE
%right ARROW LRARROW
%right OR
%right AND
%nonassoc NOT
%right CMP
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc unary_minus prec_prefix_op
%nonassoc LEFTSQ

%start file

%type <Py_ast.file> file
%type <Py_ast.decl> stmt

%%

file:
| NEWLINE* EOF
    { [] }
| NEWLINE? dl=nonempty_list(decl) NEWLINE? EOF
    { dl }
;

decl:
| import { $1 }
| def    { $1 }
| stmt   { $1 }
| func   { $1 }

import:
| FROM m=ident IMPORT l=separated_list(COMMA, ident) NEWLINE
  { Dimport (m, l) }

func:
| FUNCTION id=ident LEFTPAR l=separated_list(COMMA, ident) RIGHTPAR NEWLINE
  { Dlogic (true, id, l) }
| PREDICATE id=ident LEFTPAR l=separated_list(COMMA, ident) RIGHTPAR NEWLINE
  { Dlogic (false, id, l) }

def:
| DEF f = ident LEFTPAR x = separated_list(COMMA, ident) RIGHTPAR
  COLON NEWLINE BEGIN s=spec l=nonempty_list(stmt) END
    { Ddef (f, x, s, l) }
;

spec:
| (* epsilon *)     { empty_spec }
| single_spec spec  { spec_union $1 $2 }

single_spec:
| REQUIRES t=term NEWLINE
    { { empty_spec with sp_pre = [t] } }
| ENSURES e=ensures NEWLINE
    { { empty_spec with sp_post = [floc $startpos(e) $endpos(e), e] } }
| variant
    { { empty_spec with sp_variant = $1 } }

ensures:
| term
    { let id = mk_id "result" $startpos $endpos in
      [mk_pat (Pvar id) $startpos $endpos, $1] }

expr:
| d = expr_desc
   { mk_expr (floc $startpos $endpos) d }
;

expr_desc:
| NONE
    { Enone }
| TRUE
    { Ebool true }
| FALSE
    { Ebool false }
| c = INTEGER
    { Eint c }
| s = STRING
    { Estring s }
| id = ident
    { Eident id }
| e1 = expr LEFTSQ e2 = expr RIGHTSQ
    { Eget (e1, e2) }
| MINUS e1 = expr %prec unary_minus
    { Eunop (Uneg, e1) }
| NOT e1 = expr
    { Eunop (Unot, e1) }
| e1 = expr o = binop e2 = expr
    { Ebinop (o, e1, e2) }
| e1 = expr TIMES e2 = expr
    { match e1.expr_desc with
      | Elist [e1] -> Emake (e1, e2)
      | _ -> Ebinop (Bmul, e1, e2) }
| f = ident LEFTPAR e = separated_list(COMMA, expr) RIGHTPAR
    { Ecall (f, e) }
| LEFTSQ l = separated_list(COMMA, expr) RIGHTSQ
    { Elist l }
| LEFTPAR e = expr RIGHTPAR
    { e.expr_desc }
;

%inline binop:
| PLUS  { Badd }
| MINUS { Bsub }
| DIV   { Bdiv }
| MOD   { Bmod }
| c=CMP { c    }
| AND   { Band }
| OR    { Bor  }
;

located(X):
| X { mk_stmt (floc $startpos $endpos) $1 }
;

suite:
| s = simple_stmt NEWLINE
    { [s] }
| NEWLINE BEGIN l = nonempty_list(stmt) END
    { l }
;

stmt:
| located(stmt_desc)      { $1 }
| s = simple_stmt NEWLINE { s }

stmt_desc:
| IF c = expr COLON s1 = suite s2=else_branch
    { Sif (c, s1, s2) }
| WHILE e = expr COLON b=loop_body
    { let i, v, l = b in Swhile (e, i, v, l) }
| FOR x = ident IN e = expr COLON b=loop_body
    { let i, _, l = b in Sfor (x, e, i, l) }
;

else_branch:
| /* epsilon */
    { [] }
| ELSE COLON s2=suite
    { s2 }
| ELIF c=expr COLON s1=suite s2=else_branch
    { [mk_stmt (floc $startpos $endpos) (Sif (c, s1, s2))] }


loop_body:
| s = simple_stmt NEWLINE
  { [], [], [s] }
| NEWLINE BEGIN a=loop_annotation l=nonempty_list(stmt) END
  { fst a, snd a, l }

loop_annotation:
| (* epsilon *)
    { [], [] }
| invariant loop_annotation
    { let (i, v) = $2 in ($1::i, v) }
| variant loop_annotation
    { let (i, v) = $2 in (i, variant_union $1 v) }

invariant:
| INVARIANT i=term NEWLINE { i }

variant:
| VARIANT l=comma_list1(term) NEWLINE { List.map (fun t -> t, None) l }

simple_stmt: located(simple_stmt_desc) { $1 };

simple_stmt_desc:
| RETURN e = expr
    { Sreturn e }
| id = ident EQUAL e = expr
    { Sassign (id, e) }
| e1 = expr LEFTSQ e2 = expr RIGHTSQ EQUAL e3 = expr
    { Sset (e1, e2, e3) }
| k=assertion_kind t = term
    { Sassert (k, t) }
| e = expr
    { Seval e }
| BREAK
    { Sbreak }
| LABEL id=ident
    { Slabel id }
;

assertion_kind:
| ASSERT  { Expr.Assert }
| ASSUME  { Expr.Assume }
| CHECK   { Expr.Check }

ident:
  id = IDENT { mk_id id $startpos $endpos }
;

/* logic */

mk_term(X): d = X { mk_term d $startpos $endpos }

term: t = mk_term(term_) { t }

term_:
| term_arg_
    { match $1 with (* break the infix relation chain *)
      | Tinfix (l,o,r) -> Tinnfix (l,o,r)
      | Tbinop (l,o,r) -> Tbinnop (l,o,r)
      | d -> d }
| NOT term
    { Tnot $2 }
| o = prefix_op ; t = term %prec prec_prefix_op
    { Tidapp (Qident o, [t]) }
| l = term ; o = bin_op ; r = term
    { Tbinop (l, o, r) }
| l = term ; o = infix_op_1 ; r = term
    { Tinfix (l, o, r) }
| l = term ; o = infix_op_234 ; r = term
    { Tidapp (Qident o, [l; r]) }
| IF term THEN term ELSE term
    { Tif ($2, $4, $6) }
| LET id=ident EQUAL t1=term IN t2=term
    { Tlet (id, t1, t2) }
| q=quant l=comma_list1(ident) DOT t=term
    { let var id = id.id_loc, Some id, false, None in
      Tquant (q, List.map var l, [], t) }
| id=ident LEFTPAR l=separated_list(COMMA, term) RIGHTPAR
    { Tidapp (Qident id, l) }

quant:
| FORALL  { Dterm.DTforall }
| EXISTS  { Dterm.DTexists }

term_arg: mk_term(term_arg_) { $1 }

term_arg_:
| ident       { Tident (Qident $1) }
| INTEGER     { Tconst (Number.(ConstInt { ic_negative = false ; ic_abs = int_literal_dec $1})) }
| NONE        { Ttuple [] }
| TRUE        { Ttrue }
| FALSE       { Tfalse }
| term_sub_                 { $1 }

term_sub_:
| LEFTPAR term RIGHTPAR                             { $2.term_desc }
| term_arg LEFTSQ term RIGHTSQ
    { Tidapp (get_op $startpos($2) $endpos($2), [$1;$3]) }
| term_arg LEFTSQ term LARROW term RIGHTSQ
    { Tidapp (upd_op $startpos($2) $endpos($2), [$1;$3;$5]) }

%inline bin_op:
| ARROW   { Dterm.DTimplies }
| LRARROW { Dterm.DTiff }
| OR      { Dterm.DTor }
| AND     { Dterm.DTand }

%inline infix_op_1:
| c=CMP  { let op = match c with
          | Beq  -> "="
          | Bneq -> "<>"
          | Blt  -> "<"
          | Ble  -> "<="
          | Bgt  -> ">"
          | Bge  -> ">="
          | Badd|Bsub|Bmul|Bdiv|Bmod|Band|Bor -> assert false in
           mk_id (Ident.op_infix op) $startpos $endpos }

%inline prefix_op:
| MINUS { mk_id (Ident.op_prefix "-")  $startpos $endpos }

%inline infix_op_234:
| DIV    { mk_id "div" $startpos $endpos }
| MOD    { mk_id "mod" $startpos $endpos }
| PLUS   { mk_id (Ident.op_infix "+") $startpos $endpos }
| MINUS  { mk_id (Ident.op_infix "-") $startpos $endpos }
| TIMES  { mk_id (Ident.op_infix "*") $startpos $endpos }

comma_list1(X):
| separated_nonempty_list(COMMA, X) { $1 }
