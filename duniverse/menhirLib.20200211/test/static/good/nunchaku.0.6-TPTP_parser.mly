(* Original file: nunchaku.0.6/nunchaku-0.6/src/parsers/TPTP_parser.mly *)

(* This file is free software, part of nunchaku. See file "license" for more details. *)

(** {1 Parser for TPTP}

  This parser returns a verbatim representation of the file.
  You should use {!NunTPTPRecursiveParser} instead.
*)

%{

  open Nunchaku_core

  module L = Location
  module A = UntypedAST
  module B = A.Builtin

  (* remove quote from some symbols *)
  let remove_quotes s =
    assert (s.[0] = '\'' && s.[String.length s - 1] = '\'');
    String.sub s 1 (String.length s - 2)
%}

%token EOI

%token DOT
%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET

%token NOT

%token COLUMN
%token STAR
%token ARROW
%token FORALL_TY /* quantification on types */
%token WILDCARD /* $_ */
%token TY_PROP /* $o */
%token TY_TYPE /* $tType */
%token ITE_T
%token ITE_F
/*
%token LET_TF
%token LET_FF
*/

%token AT
%token HO_FORALL
%token HO_EXISTS
%token LAMBDA

%token AND
%token NOTAND
%token VLINE
%token NOTVLINE
%token IMPLY
%token LEFT_IMPLY
%token EQUIV
%token XOR
%token GENTZEN_ARROW
%token EQUAL
%token NOT_EQUAL

%token TRUE
%token FALSE

%token FORALL
%token EXISTS

%token UNDERSCORE

%token ROLE_CONJECTURE
%token ROLE_AXIOM
%token ROLE_TYPE
%token ROLE_DEFINITION

%token FOF
%token CNF
%token TFF
%token THF
%token INCLUDE

%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> SINGLE_QUOTED
%token <string> DISTINCT_OBJECT
%token <string> DOLLAR_WORD
%token <string> DOLLAR_DOLLAR_WORD
%token <string> REAL
%token <string> RATIONAL
%token <string> INTEGER

%start <Nunchaku_core.UntypedAST.statement> parse_statement
%start <Nunchaku_core.UntypedAST.statement list> parse_statement_list
%start <Nunchaku_core.UntypedAST.term> parse_term
%start <Nunchaku_core.UntypedAST.term> parse_fo_form
%start <Nunchaku_core.UntypedAST.term> parse_ho_form
%start <Nunchaku_core.UntypedAST.ty> parse_ty

%start <Nunchaku_core.UntypedAST.term list list> parse_answer_tuples


%%

parse_statement: s=statement EOI {s}
parse_term: t=term EOI {t}
parse_fo_form: t=fof_formula EOI {t}
parse_ho_form: t=thf_formula EOI {t}
parse_ty: t=term EOI {t}
parse_answer_tuples: l=answer_tuples EOI { l }
parse_statement_list: l=list(statement) EOI { l }

answer_tuples:
  | LEFT_BRACKET l=separated_nonempty_list(VLINE, answer_tuple) RIGHT_BRACKET
    { CCList.filter_map (fun x->x) l }

answer_tuple:
  | LEFT_BRACKET l=separated_nonempty_list(COMMA,term) RIGHT_BRACKET { Some l }
  | UNDERSCORE { None }

role_as_goal:
  | ROLE_CONJECTURE {}

role_as_axiom:
  | ROLE_AXIOM {}

role_as_def:
  | ROLE_DEFINITION {}

role_as_decl:
  | ROLE_TYPE {}

%public toplevel_form(ROLE):
  | TFF LEFT_PAREN name=name COMMA ROLE COMMA f=tff_formula annotations RIGHT_PAREN DOT {name,f}
  | THF LEFT_PAREN name=name COMMA ROLE COMMA f=thf_formula annotations RIGHT_PAREN DOT {name,f}
  | FOF LEFT_PAREN name=name COMMA ROLE COMMA f=fof_formula annotations RIGHT_PAREN DOT {name,f}
  | CNF LEFT_PAREN name=name COMMA ROLE COMMA f=cnf_formula annotations RIGHT_PAREN DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      name, A.or_ ~loc f }

statement:
  | f=toplevel_form(role_as_goal)
    {
      let name, f = f in
      let loc = L.mk_pos $startpos $endpos in
      A.goal ~name ~loc f
    }
  | f=toplevel_form(role_as_axiom)
    {
      let name, f = f in
      let loc = L.mk_pos $startpos $endpos in
      A.axiom ~name ~loc [f]
    }
  | THF LEFT_PAREN name=name COMMA role_as_def COMMA def=thf_def
    annotations RIGHT_PAREN DOT
    { (* parse a term definition *)
      let a, b = def in
      let loc = L.mk_pos $startpos $endpos in
      A.def ~name ~loc a b
    }
  | THF LEFT_PAREN name=name COMMA role_as_decl COMMA decl=type_decl(thf_type) annotations RIGHT_PAREN DOT
  | TFF LEFT_PAREN name=name COMMA role_as_decl COMMA decl=type_decl(tff_type) annotations RIGHT_PAREN DOT
    {
      let s, ty = decl in
      let loc = L.mk_pos $startpos $endpos in
      A.decl ~name ~attrs:[] ~loc s ty
    }
  | INCLUDE LEFT_PAREN x=SINGLE_QUOTED RIGHT_PAREN DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.include_ ~loc (remove_quotes x)
    }
  | INCLUDE LEFT_PAREN x=SINGLE_QUOTED COMMA names=name_list RIGHT_PAREN DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.include_ ~loc ~which:names (remove_quotes x)
    }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      Parsing_utils.parse_error_ ~loc "expected statement"
    }

%public type_decl(TY):
  | LEFT_PAREN tydecl=type_decl(TY) RIGHT_PAREN { tydecl }
  | s=atomic_word COLUMN ty=quantified_type(TY) { s, ty }
  | s=DOLLAR_WORD COLUMN ty=quantified_type(TY) { s, ty }

thf_def:
  | a=atomic_word EQUAL b=thf_formula {a,b}
  | LEFT_PAREN d=thf_def RIGHT_PAREN { d }

thf_formula:
  | t=thf_apply_term { t }
  | l=thf_formula o=infix_connective r=thf_unitary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      o loc l r }
  | l=thf_formula o=binary_connective r=thf_unitary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      o loc l r
    }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      Parsing_utils.parse_error_ ~loc "could not parse THF formula" }

thf_apply_term:
  | t=thf_unitary_formula { t }
  | l=thf_apply_term AT r=thf_unitary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      A.app ~loc l [r]
    }

thf_unitary_formula:
  | t=thf_unary_formula { t }
  | q=thf_quantifier
    LEFT_BRACKET
      vars=separated_nonempty_list(COMMA, raw_typed_variable(thf_type))
    RIGHT_BRACKET
    COLUMN
    f=thf_unitary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      q ~loc:loc vars f
    }

thf_quantifier:
  | FORALL { A.forall_list }
  | EXISTS { A.exists_list }
  | LAMBDA { A.fun_list }

thf_unary_formula:
  | t=thf_atomic_term { t }
  | o=unary_connective LEFT_PAREN t=thf_formula RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      o ~loc t
    }

thf_atomic_term:
  | t=thf_const { t }
  | thf_ite LEFT_PAREN a=thf_formula COMMA b=thf_formula COMMA c=thf_formula RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      A.ite ~loc a b c
    }
  | LEFT_PAREN t=thf_formula RIGHT_PAREN { t }

thf_ite:
  | ITE_T {}
  | ITE_F {}

thf_const:
  | TRUE {
      let loc = L.mk_pos $startpos $endpos in
      A.true_ ~loc }
  | FALSE {
      let loc = L.mk_pos $startpos $endpos in
      A.false_ ~loc }
  | HO_FORALL {
      let loc = L.mk_pos $startpos $endpos in
      A.forall_term ~loc }
  | HO_EXISTS {
      let loc = L.mk_pos $startpos $endpos in
      A.exists_term ~loc }
  | WILDCARD
    {
      let loc = L.mk_pos $startpos $endpos in
      A.wildcard ~loc () (* useful as argument to a term... *)
    }
  | s=atomic_word
    {
      let loc = L.mk_pos $startpos $endpos in
      A.at_var ~loc s
    }
  | v=raw_variable
    {
      let loc = L.mk_pos $startpos $endpos in
      A.var ~loc v
    }

thf_type:
  | ty=thf_unary_type { ty }
  | l=thf_unary_type ARROW r=thf_type
    {
      let loc = L.mk_pos $startpos $endpos in
      A.ty_arrow ~loc l r
    }

thf_unary_type:
  | t=thf_atom_type { t }
  | LEFT_PAREN t=thf_type RIGHT_PAREN { t }

thf_atom_type:
  | v=raw_ty_variable
    {
      let loc = L.mk_pos $startpos $endpos in
      A.var ~loc v
    }
  | v=type_const { v }

cnf_formula:
  | LEFT_PAREN c=disjunction RIGHT_PAREN { c }
  | c=disjunction { c }

disjunction:
  | l=separated_nonempty_list(VLINE, literal) { l }

literal:
  | f=atomic_formula { f }
  | NOT f=atomic_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      A.not_ ~loc f
    }

tff_formula:
  | f=fof_formula { f }

fof_formula:
  | fof_logic_formula { $1 }
  | fof_sequent { $1 }

fof_sequent:
  | l=fof_tuple GENTZEN_ARROW r=fof_tuple
    {
      let loc = L.mk_pos $startpos $endpos in
      A.imply ~loc (A.and_ ~loc l) (A.or_ ~loc r)
    }
  | LEFT_PAREN seq=fof_sequent RIGHT_PAREN { seq }

fof_tuple:
  LEFT_BRACKET l=separated_list(COMMA, fof_logic_formula) RIGHT_BRACKET { l }

fof_logic_formula:
  | f=fof_unitary_formula { f }
  | l=fof_logic_formula o=binary_connective r=fof_unitary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      o loc l r
    }

fof_unitary_formula:
  | f=fof_quantified_formula { f }
  | f=fof_unary_formula { f }
  | f=atomic_formula { f }
  | LEFT_PAREN f=fof_logic_formula RIGHT_PAREN { f }

fof_quantified_formula:
  | q=fol_quantifier
    LEFT_BRACKET
      vars=separated_nonempty_list(COMMA, raw_typed_variable(tff_unary_type))
    RIGHT_BRACKET
    COLUMN
    f=fof_unitary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      q ~loc vars f
    }

fof_unary_formula:
  | o=unary_connective f=fof_unitary_formula
    {
     let loc = L.mk_pos $startpos $endpos in
     o ~loc f
    }

binary_connective:
  | EQUIV { fun loc a b -> A.equiv ~loc a b }
  | IMPLY { fun loc a b -> A.imply ~loc a b }
  | LEFT_IMPLY { fun loc l r -> A.imply ~loc r l }
  | XOR { fun loc l r -> A.not_ ~loc (A.equiv ~loc l r) }
  | NOTVLINE { fun loc x y -> A.not_ ~loc (A.or_ ~loc [x; y]) }
  | NOTAND { fun loc x y -> A.not_ ~loc (A.and_ ~loc [x; y]) }
  | AND { fun loc x y -> A.and_ ~loc [x;y] }
  | VLINE { fun loc x y -> A.or_ ~loc [x;y] }
fol_quantifier:
  | FORALL { fun ~loc vars t -> A.forall_list ~loc vars t }
  | EXISTS { fun ~loc vars t -> A.exists_list ~loc vars t }
unary_connective:
  | NOT { fun ~loc t -> A.not_ ~loc t }

atomic_formula:
  | TRUE {
      let loc = L.mk_pos $startpos $endpos in
      A.true_ ~loc }
  | FALSE {
      let loc = L.mk_pos $startpos $endpos in
      A.false_ ~loc }
  | l=term o=infix_connective r=term
    {
      let loc = L.mk_pos $startpos $endpos in
      o loc l r
    }
  | t=function_term { t }

%inline infix_connective:
  | EQUAL { fun loc a b -> A.eq ~loc a b }
  | NOT_EQUAL { fun loc a b -> A.neq ~loc a b }

/* Terms */

term:
  | t=function_term { t }
  | v=raw_variable
    {
      let loc = L.mk_pos $startpos $endpos in
      A.var ~loc v
    }
  | t=conditional_term(fof_formula,term) { t }
  /* TODO: this is actually a rewrite rule, not a let... | t=let_term { t } */

function_term:
  | plain_term { $1 }
  | defined_term { $1 }
  | system_term { $1 }

%public conditional_term(FORM,RES):
  | ITE_T LEFT_PAREN a=FORM COMMA b=RES COMMA c=RES RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      A.ite ~loc a b c
    }

%public conditional_form(FORM):
  | ITE_F LEFT_PAREN a=FORM COMMA b=FORM COMMA c=FORM RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      A.ite_ ~loc a b c
    }

plain_term:
  | WILDCARD
    {
      let loc = L.mk_pos $startpos $endpos in
      A.wildcard ~loc () (* useful as argument to a term... *)
    }
  | s=atomic_word
    {
      let loc = L.mk_pos $startpos $endpos in
      A.at_var ~loc s
    }
  | f=functor_ LEFT_PAREN args=arguments RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      A.app ~loc f args
    }

functor_:
  | f=atomic_word
    {
      let loc = L.mk_pos $startpos $endpos in
      A.at_var ~loc f
    }

defined_term:
  | t=defined_atom
    {
      let loc = L.mk_pos $startpos $endpos in
      A.at_var ~loc t
    }
  | t=defined_atomic_term { t }

defined_atom:
  | INTEGER { Utils.not_implemented "TPTP: cannot handle integer" }
  | RATIONAL { Utils.not_implemented "TPTP: cannot handle rational" }
  | REAL {
      let loc = L.mk_pos $startpos $endpos in
      Parsing_utils.parse_error_ ~loc "TPTP: cannot handle real numbers"
    }
  | s=DISTINCT_OBJECT { s }

defined_atomic_term:
  | t=defined_plain_term { t }
  /* | defined_infix_term { $1 } */

defined_plain_term:
  | s=defined_constant
    {
      let loc = L.mk_pos $startpos $endpos in
      A.at_var ~loc s
    }
  | f=defined_functor LEFT_PAREN args=arguments RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      A.app ~loc (A.at_var ~loc f) args
    }

defined_constant: t=defined_functor { t }
defined_functor: s=atomic_defined_word { s }

system_term:
  | c=system_constant
    {
      let loc = L.mk_pos $startpos $endpos in
      A.at_var ~loc c
    }
  | f=system_functor LEFT_PAREN args=arguments RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      A.app ~loc (A.at_var ~loc f) args
    }

system_constant: t=system_functor { t }
system_functor: s=atomic_system_word { s }

/* prenex quantified type */
%public quantified_type(TY):
  | ty=TY { ty }
  | FORALL_TY
    LEFT_BRACKET vars=separated_nonempty_list(COMMA, raw_ty_variable) RIGHT_BRACKET
    COLUMN
    ty=quantified_type(TY)
    {
      let loc = L.mk_pos $startpos $endpos in
      A.ty_forall_list ~loc vars ty }

/* general type, without quantifiers */
tff_type:
  | ty=tff_unary_type { ty }
  | l=tff_unary_type ARROW r=tff_unary_type
    {
      let loc = L.mk_pos $startpos $endpos in
      A.ty_arrow ~loc l r
    }
  | LEFT_PAREN args=ty_args RIGHT_PAREN ARROW r=tff_atom_type
    {
      let loc = L.mk_pos $startpos $endpos in
      A.ty_arrow_list ~loc args r
    }

ty_args:
  | a=tff_unary_type STAR b=tff_unary_type { [a; b] }
  | a=tff_unary_type STAR b=ty_args { a :: b }

tff_unary_type:
  | t=tff_atom_type { t }
  | LEFT_PAREN t=tff_type RIGHT_PAREN { t }

tff_atom_type:
  | v=raw_ty_variable
    {
      let loc = L.mk_pos $startpos $endpos in
      A.var ~loc v
    }
  | v=type_const { v }
  | f=type_const LEFT_PAREN l=separated_nonempty_list(COMMA, tff_type) RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      A.app ~loc f l
    }

raw_ty_variable:
  | v=raw_variable COLUMN TY_TYPE { v }
  | v=raw_variable { v }

type_const:
  | WILDCARD
    {
      let loc = L.mk_pos $startpos $endpos in
      A.wildcard ~loc ()
    }
  | TY_TYPE { A.ty_type }
  | TY_PROP { A.ty_prop }
  | v=LOWER_WORD
  | v=DOLLAR_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      A.var ~loc v
    }

arguments: separated_nonempty_list(COMMA, term) { $1 }

%public raw_typed_variable(TY):
  | v=raw_variable { v, None }
  | v=raw_variable COLUMN ty=TY { v, Some ty }

raw_variable:
  | v=UPPER_WORD { v }

atomic_word:
  | s=SINGLE_QUOTED { remove_quotes s }
  | s=LOWER_WORD { s }

atomic_defined_word:
  | w=DOLLAR_WORD { w }

atomic_system_word:
  | w=DOLLAR_DOLLAR_WORD { w }

name_list:
  l=separated_list(COMMA, name) { l }

name:
  | w=atomic_word { w }
  | i=INTEGER { i }

annotations:
  | { [] }
  | COMMA l=separated_list(COMMA, general_term) { l }

general_term:
  | x=general_data { x }
  | l=general_data COLUMN r=general_term { A.TPTP.Column (l, r) }
  | l=general_list { l }

general_data:
  | w=atomic_word { A.TPTP.Var w }
  | f=general_function { f }
  | i=INTEGER { A.TPTP.Int (int_of_string i) }
  | v=UPPER_WORD { A.TPTP.Var v }
  | w=DISTINCT_OBJECT { A.TPTP.String w }

general_function:
  | f=atomic_word LEFT_PAREN l=separated_nonempty_list(COMMA, general_term) RIGHT_PAREN
    { A.TPTP.App (f, l) }

general_list:
  | LEFT_BRACKET l=separated_list(COMMA, general_term) RIGHT_BRACKET
    { A.TPTP.List l }

%%
