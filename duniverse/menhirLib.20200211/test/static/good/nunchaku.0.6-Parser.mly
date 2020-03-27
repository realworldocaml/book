(* Original file: nunchaku.0.6/nunchaku-0.6/src/parsers/Parser.mly *)

(* This file is free software, part of nunchaku. See file "license" for more details. *)

(** {1 Parser for Nunchaku} *)

%{
  open Nunchaku_core

  module L = Location
  module A = UntypedAST
  module B = A.Builtin

%}


%token EOI

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET

%token WILDCARD
%token DOT
%token COLON
%token EQDEF
%token LET
%token IN
%token IF
%token THEN
%token ELSE
%token AND
%token AT
%token META_VAR

%token MATCH
%token WITH
%token END

%token LOGIC_TRUE
%token LOGIC_FALSE

%token LOGIC_AND
%token LOGIC_OR
%token LOGIC_NOT
%token LOGIC_IMPLY
%token LOGIC_FORALL
%token LOGIC_EXISTS
%token LOGIC_EQ
%token LOGIC_NEQ
%token ASSERTING

%token PROP
%token TYPE

%token SEMI_COLON
%token AXIOM
%token REC
%token SPEC
%token DATA
%token CODATA
%token PRED
%token COPRED
%token WF_ATTRIBUTE
%token VAL
%token GOAL
%token COPY
%token SUBSET
%token QUOTIENT
%token PARTIAL_QUOTIENT
%token ABSTRACT
%token CONCRETE

%token INCLUDE
%token<string> FILEPATH

%token ARROW
%token FUN
%token PI
%token VERTICAL_BAR

%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> INTEGER

%start <Nunchaku_core.UntypedAST.statement> parse_statement
%start <Nunchaku_core.UntypedAST.statement list> parse_statement_list
%start <Nunchaku_core.UntypedAST.term> parse_term
%start <Nunchaku_core.UntypedAST.ty> parse_ty

%%

parse_statement: s=statement EOI {s}
parse_term: t=term EOI {t}
parse_ty: t=term EOI {t}
parse_statement_list: l=list(statement) EOI { l }

/* variable without a location */
raw_var:
  | w=LOWER_WORD { w }
  | w=UPPER_WORD { w }

typed_var:
  | v=raw_var { v, None }
  | LEFT_PAREN v=raw_var COLON t=term RIGHT_PAREN { v, Some t }

typed_ty_var:
  | v=raw_var { v }
  | v=raw_var COLON TYPE { v }
  | LEFT_PAREN v=raw_var COLON TYPE RIGHT_PAREN { v }

var_or_wildcard:
  | WILDCARD { `Wildcard }
  | name=raw_var { `Var name }

var:
  | WILDCARD
    {
      let loc = L.mk_pos $startpos $endpos in
      A.wildcard ~loc ()
    }
  | name=raw_var
    {
      let loc = L.mk_pos $startpos $endpos in
      A.var ~loc name
    }

at_var:
  | AT v=raw_var
    {
      let loc = L.mk_pos $startpos $endpos in
      A.at_var ~loc v
    }

meta_var:
  | META_VAR v=raw_var
    {
      let loc = L.mk_pos $startpos $endpos in
      A.meta_var ~loc v
    }

const:
  | TYPE
    {
      let loc = L.mk_pos $startpos $endpos in
      A.builtin ~loc `Type
    }
  | PROP
    {
      let loc = L.mk_pos $startpos $endpos in
      A.builtin ~loc `Prop
    }
  | LOGIC_TRUE
    {
      let loc = L.mk_pos $startpos $endpos in
      A.builtin ~loc `True
    }
  | LOGIC_FALSE
    {
      let loc = L.mk_pos $startpos $endpos in
      A.builtin ~loc `False
    }

%public case(TERM):
  | v=raw_var l=var_or_wildcard* ARROW t=TERM { v, l, t }

%public cases(TERM):
  | VERTICAL_BAR? l=separated_nonempty_list(VERTICAL_BAR, case(TERM)) { l }

atomic_term:
  | v=var { v }
  | v=at_var { v }
  | v=meta_var { v }
  | t=const { t }
  | LEFT_PAREN t=term RIGHT_PAREN { t }
  | MATCH t=term WITH l=cases(term) END
    {
      let loc = L.mk_pos $startpos $endpos in
      A.match_with ~loc t l
    }

apply_term:
  | t=atomic_term { t }
  | t=atomic_term u=atomic_term+
    {
      let loc = L.mk_pos $startpos $endpos in
      A.app ~loc t u
    }
  | LOGIC_NOT t=apply_term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.app ~loc (A.builtin ~loc `Not) [t]
    }

eq_term:
  | t=apply_term { t }
  | t=apply_term LOGIC_EQ u=apply_term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.eq ~loc t u
    }
  | t=apply_term LOGIC_NEQ u=apply_term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.neq ~loc t u
    }

and_term:
  | t=eq_term { t }
  | t=eq_term LOGIC_AND u=and_term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.app ~loc (A.builtin ~loc `And) [t; u]
    }

or_term:
  | t=and_term { t }
  | t=and_term LOGIC_OR u=or_term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.app ~loc (A.builtin ~loc `Or) [t; u]
    }
  | t=and_term LOGIC_IMPLY u=or_term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.app ~loc (A.builtin ~loc `Imply) [t; u]
    }

term:
  | t=or_term { t }
  | t=apply_term ASSERTING g=term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.asserting ~loc t [g]
    }
  | FUN vars=typed_var+ DOT t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.fun_list ~loc vars t
    }
  | LET v=raw_var EQDEF t=term IN u=term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.let_ ~loc v t u
    }
  | IF a=term THEN b=term ELSE c=term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.ite ~loc a b c
    }
  | LOGIC_FORALL vars=typed_var+ DOT t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.forall_list ~loc vars t
    }
  | LOGIC_EXISTS vars=typed_var+ DOT t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.exists_list ~loc vars t
    }
  | t=apply_term ARROW u=term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.ty_arrow ~loc t u
    }
  | PI vars=typed_ty_var+ DOT t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.ty_forall_list ~loc vars t
    }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      raise (A.ParseError loc)
    }

defined_symbol:
  | v=raw_var COLON ty=term { v, ty }

rec_def:
  | d=defined_symbol EQDEF l=separated_nonempty_list(SEMI_COLON,term)
    {
      let v, ty = d in
      v, ty, l
    }

rec_defs:
  | l=separated_nonempty_list(AND, rec_def) { l }

spec_defs:
  | d=separated_nonempty_list(AND, defined_symbol)
    EQDEF
    l=separated_nonempty_list(SEMI_COLON,term)
    { d, l }

constructor:
  | v=raw_var l=atomic_term* { v, l }

constructors:
  | VERTICAL_BAR? l=separated_nonempty_list(VERTICAL_BAR, constructor) { l }

type_def:
  | t=raw_var vars=raw_var* EQDEF l=constructors
    { t, vars, l }

mutual_types:
  | l=separated_nonempty_list(AND, type_def) { l }

pred_def:
  | id=raw_var
    COLON ty=term
    EQDEF l=separated_nonempty_list(SEMI_COLON, term)
    { id, ty, l }

mutual_preds:
  | l=separated_nonempty_list(AND, pred_def) { l }

wf_attribute:
  | LEFT_BRACKET WF_ATTRIBUTE RIGHT_BRACKET { `Wf }
  | { `Not_wf }

decl_attribute_element:
  | w=UPPER_WORD { w }
  | w=LOWER_WORD { w }
  | w=INTEGER { w }

decl_attributes:
  | LEFT_BRACKET
    l=separated_nonempty_list(SEMI_COLON, decl_attribute_element+)
    RIGHT_BRACKET
    { l }
  | { [] }

copy_wrt:
  | { A.Wrt_nothing }
  | SUBSET t=atomic_term { A.Wrt_subset t }
  | QUOTIENT t=atomic_term { A.Wrt_quotient (`Total, t) }
  | PARTIAL_QUOTIENT t=atomic_term { A.Wrt_quotient (`Partial, t) }

statement:
  | VAL v=raw_var COLON t=term attrs=decl_attributes DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.decl ~loc ~attrs v t
    }
  | AXIOM l=separated_nonempty_list(SEMI_COLON, term) DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.axiom ~loc l
    }
  | SPEC l=spec_defs DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.spec ~loc l
    }
  | REC l=rec_defs DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.rec_ ~loc l
    }
  | DATA l=mutual_types DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.data ~loc l
    }
  | CODATA l=mutual_types DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.codata ~loc l
    }
  | PRED a=wf_attribute l=mutual_preds DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.pred ~loc ~wf:a l
    }
  | COPRED a=wf_attribute l=mutual_preds DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.copred ~loc ~wf:a l
    }
  | GOAL t=term DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.goal ~loc t
    }
  | INCLUDE f=FILEPATH DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.include_ ~loc f
    }
  | COPY id=raw_var vars=raw_var* EQDEF u=term
    wrt=copy_wrt
    ABSTRACT abs=raw_var
    CONCRETE conc=raw_var
    DOT
    {
      (* TODO: instead, parse list of "fields" and validate after parsing?
         better once we get more possible (optional) fields, in random order *)
      let loc = L.mk_pos $startpos $endpos in
      A.copy ~loc ~of_:u ~abstract:abs ~concrete:conc ~wrt id vars
    }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      raise (A.ParseError loc)
    }

%%
