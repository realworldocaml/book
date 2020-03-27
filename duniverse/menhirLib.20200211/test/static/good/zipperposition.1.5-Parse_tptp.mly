
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 TPTP Parser} *)

%{
  open Logtk

  module L = ParseLocation
  module PT = STerm
  module A = Ast_tptp

  let remove_quotes s =
    assert (s.[0] = '\'' && s.[String.length s - 1] = '\'');
    String.sub s 1 (String.length s - 2)
%}

%token EOI

%token DOT
/* %token SEMICOLUMN */
%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET

%token FOF
%token CNF
%token TFF
%token THF
%token INCLUDE

%token NOT

%token COLUMN
%token STAR
%token ARROW
%token FORALL_TY  /* quantification on types */
%token TYPE_TY  /* tType */
%token WILDCARD  /* $_ */

%token AT /* @ */
%token LAMBDA /* ^ */

%token AND
%token NOTAND
%token VLINE
%token NOTVLINE
%token IMPLY
%token LEFT_IMPLY
%token EQUIV
%token XOR
%token EQUAL
%token NOT_EQUAL

%token TRUE
%token FALSE

%token FORALL
%token EXISTS

%token FORALLCONST
%token EXISTSCONST

%token UNDERSCORE

%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> SINGLE_QUOTED
%token <string> DISTINCT_OBJECT
%token <string> DOLLAR_WORD
%token <string> DOLLAR_DOLLAR_WORD
%token <string> REAL
%token <string> RATIONAL
%token <string> INTEGER

/*
%nonassoc VLINE
%nonassoc AND
%nonassoc EQUIV
%nonassoc XOR
%nonassoc IMPLY
%nonassoc LEFT_IMPLY
%nonassoc NOTVLINE
%nonassoc NOTAND
*/

%start <Logtk.STerm.t> parse_term
%start <Logtk.STerm.t> parse_formula
%start <Logtk.STerm.t Ast_tptp.declaration> parse_declaration
%start <Logtk.STerm.t Ast_tptp.declaration list> parse_declarations
%start <Logtk.STerm.t list list> parse_answer_tuple

%%

/* top-level */

parse_term: t=term EOI { t }
parse_formula: f=formula EOI { f }
parse_declaration: d=declaration EOI { d }
parse_declarations: l=declarations EOI { l }
parse_answer_tuple: t=answer_tuples EOI { t }

/* TPTP grammar */

declarations:
  | l=declaration* { l }

declaration:
  | FOF LEFT_PAREN name=name COMMA role=role COMMA f=formula info=annotations RIGHT_PAREN DOT
    { A.FOF (name, role, f, info) }
  | TFF LEFT_PAREN name=name COMMA role=role COMMA f=formula info=annotations RIGHT_PAREN DOT
    { A.TFF (name, role, f, info) }
  | THF LEFT_PAREN name=name COMMA role=role COMMA f=formula info=annotations RIGHT_PAREN DOT
    { A.THF (name, role, f, info) }
  | declaration_ty_header LEFT_PAREN name=name COMMA role COMMA tydecl=type_decl info=annotations RIGHT_PAREN DOT
    { let s, ty = tydecl in
      match ty.PT.term with
      | PT.AppBuiltin (Builtin.TType, [])
      | PT.AppBuiltin
          (Builtin.Arrow,
           {PT.term=PT.AppBuiltin (Builtin.TType,[]);_} :: _) ->
             (* declare a new type symbol *)
             A.NewType (name, s, ty, info)
      | _ -> A.TypeDecl (name, s, ty, info)
    }
  | CNF LEFT_PAREN name=name COMMA role=role COMMA c=cnf_formula info=annotations RIGHT_PAREN DOT
    { A.CNF (name, role, c, info) }
  | INCLUDE LEFT_PAREN x=SINGLE_QUOTED RIGHT_PAREN DOT
    { A.Include (remove_quotes x) }
  | INCLUDE LEFT_PAREN x=SINGLE_QUOTED COMMA names=name_list RIGHT_PAREN DOT
    { A.IncludeOnly (remove_quotes x, names) }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      UntypedAST.error loc "expected declaration"
    }

%inline
declaration_ty_header: TFF {} | THF {}

role: w=LOWER_WORD { Ast_tptp.role_of_string w }

answer_tuples:
  | LEFT_BRACKET l=separated_nonempty_list(VLINE,answer_tuple) RIGHT_BRACKET
    { List.fold_left  (* remove underscores *)
        (fun acc opt -> match opt with | None -> acc | Some tup -> tup :: acc)
        [] l  }

answer_tuple:
  | LEFT_BRACKET l=separated_nonempty_list(COMMA,term) RIGHT_BRACKET { Some l }
  | UNDERSCORE { None }

type_decl:
  | LEFT_PAREN tydecl=type_decl RIGHT_PAREN { tydecl }
  | s=atomic_word COLUMN ty=tff_quantified_type { s, ty }

cnf_formula:
  | LEFT_PAREN c=cnf_formula RIGHT_PAREN { c }
  | c=disjunction { c }

disjunction:
  | l=separated_nonempty_list(VLINE, literal) { l }

literal_atom:
  | f=atomic_formula { f }
  | l=atomic_formula op=infix_connective r=atomic_formula { op l r }

literal:
  | f=literal_atom { f }
  | NOT f=literal_atom
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.not_ ~loc f
    }

/* actually, any formula or term */
formula:
  | f=binary_formula { f }
  | f=unary_formula { f }
  | f=app_formula { f }

app_formula:
  | f=unitary_formula AT t=unary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.app ~loc f [t]
    }
  | f=app_formula AT t=unary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.app ~loc f [t]
    }

unitary_formula:
  | f=quantified_formula { f }
  | f=unitary_atomic_formula { f }

unitary_atomic_formula:
  | f=atomic_formula { f }
  | LEFT_PAREN f=formula RIGHT_PAREN { f }

quantified_formula:
  | q=quantifier LEFT_BRACKET vars=typed_vars RIGHT_BRACKET COLUMN f=unary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      q ?loc:(Some loc) vars f
    }

unitary_infix_formula:
  | f=unitary_formula { f }
  | l=unitary_atomic_formula op=infix_connective r=unitary_formula { op l r }

unary_formula:
  | f=unitary_infix_formula { f }
  | o=unary_connective AT f=unary_formula
    {
     let loc = L.mk_pos $startpos $endpos in
     o ?loc:(Some loc) f
    }
  | o=unary_connective f=unary_formula
    {
     let loc = L.mk_pos $startpos $endpos in
     o ?loc:(Some loc) f
    }

binary_formula:
  | f=nonassoc_binary_formula { f }
  | f=assoc_binary_formula { f }

nonassoc_binary_formula:
  | o=binary_connective AT l=unary_formula AT r=unary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      o ?loc:(Some loc) l r
    }
  | AND AT l=unary_formula AT r=unary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.and_ ?loc:(Some loc) [l; r]
    }
  | VLINE AT l=unary_formula AT r=unary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.or_ ?loc:(Some loc) [l; r]
    }
  | l=unary_formula o=binary_connective r=unary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      o ?loc:(Some loc) l r
    }

assoc_binary_formula:
  | f=and_formula { f }
  | f=or_formula { f }

assoc_binary_formula_aux(OP):
  | l=unary_formula
    OP
    r=separated_nonempty_list(OP,unary_formula)
    { l :: r }

and_formula: l=assoc_binary_formula_aux(AND) { PT.and_ l }
or_formula: l=assoc_binary_formula_aux(VLINE) { PT.or_ l }

%inline binary_connective:
  | EQUIV { PT.equiv }
  | IMPLY { PT.imply }
  | LEFT_IMPLY { fun ?loc l r -> PT.imply ?loc r l }
  | XOR { PT.xor }
  | NOTVLINE { fun ?loc x y -> PT.not_ ?loc (PT.or_ ?loc [x; y]) }
  | NOTAND { fun ?loc x y -> PT.not_ ?loc (PT.and_ ?loc [x; y]) }
  | ARROW { fun ?loc x y -> PT.fun_ty ?loc [x] y }
%inline quantifier:
  | FORALL { PT.forall }
  | EXISTS { PT.exists }
  | LAMBDA { PT.lambda }
%inline unary_connective:
  | NOT { PT.not_ }

atomic_formula:
  | TRUE { PT.true_ }
  | FALSE { PT.false_ }
  | EXISTSCONST { PT.builtin Builtin.ExistsConst }
  | FORALLCONST { PT.builtin Builtin.ForallConst }
  | t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.at_loc ~loc t
    }

%inline infix_connective:
  | EQUAL { PT.eq }
  | NOT_EQUAL { PT.neq }

/* Terms */

term:
  | t=function_term { t }
  | t=variable { t }
  /* | conditional_term { $1 }  for TFF */
  /* | let_term { $1 } */

function_term:
  | t=plain_term { t }
  | t=defined_term { t }
  | t=system_term { t }

plain_term:
  | t=atomic_term { t }
  | f=functor_ LEFT_PAREN args=term_arguments RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.app ~loc f args
    }

atomic_term:
  | s=atomic_word
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.const ~loc s
    }

term_arguments:
  | l=separated_nonempty_list(COMMA, formula) { l }

functor_:
  | f=atomic_word { PT.const f }

defined_term:
  | t=defined_atom { t }
  | t=defined_atomic_term { t }

defined_atom:
  | n=INTEGER { PT.int_ (Z.of_string n) }
  | n=RATIONAL { PT.rat (Q.of_string n) }
  | n=REAL { PT.real n }
  | s=DISTINCT_OBJECT
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.const ~loc s
    }

defined_atomic_term:
  | t=defined_plain_term { t }
  /* | defined_infix_term { $1 } */

defined_plain_term:
  | s=defined_functor { s }
  | s=DOLLAR_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      match Builtin.TPTP.of_string s with
      | None ->
          UntypedAST.errorf loc "unknown builtin `%s`" s
      | Some b -> PT.builtin ~loc b
    }
  | f=defined_functor LEFT_PAREN args=term_arguments RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.app ~loc f args
    }
  | s=DOLLAR_WORD LEFT_PAREN args=term_arguments RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      match Builtin.TPTP.of_string s with
      | None ->
          UntypedAST.errorf loc "unknown builtin function `%s`" s
      | Some b -> PT.app_builtin ~loc b args
    }

defined_functor: s=atomic_defined_word { s }

system_term:
  | c=system_constant { c }
  | f=system_functor LEFT_PAREN args=term_arguments RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.app ~loc f args
    }

system_constant: t=system_functor { t }
system_functor: s=atomic_system_word { s }

typed_var:
  | v=UPPER_WORD COLUMN ty=tff_type { PT.V v, Some ty }
  | v=UPPER_WORD { PT.V v, None }

typed_vars:
  | l=separated_nonempty_list(COMMA, typed_var) { l }

/* prenex quantified type */
tff_quantified_type:
  | ty=tff_toplevel_type { ty }
  | FORALL_TY LEFT_BRACKET vars=tff_ty_vars RIGHT_BRACKET COLUMN ty=tff_quantified_type
    { PT.forall_ty vars ty }

/* toplevel type, possibly with arrows, but without quantifier */
tff_toplevel_type:
  | ty=tff_type { ty  }
  | LEFT_PAREN args=tff_ty_star_list RIGHT_PAREN ARROW r=tff_app_type
    { PT.fun_ty args r }

/* general type that a variable can have */
tff_type:
  | ty=tff_app_type { ty }
  | l=tff_app_type ARROW r=tff_type
    { PT.fun_ty [l] r }

tff_app_type:
  | ty=tff_atomic_type { ty }
  | f=tff_app_type AT a=tff_atomic_type { PT.app f [a] }

tff_atomic_type:
  | v=variable { v }
  | w=defined_ty { w }
  | w=type_const { w }
  | w=type_const LEFT_PAREN l=separated_nonempty_list(COMMA, tff_type) RIGHT_PAREN
    { PT.app w l }
  | TYPE_TY { PT.tType }
  | LEFT_PAREN ty=tff_toplevel_type RIGHT_PAREN { ty }

tff_ty_star_list:
  | ty=tff_app_type
    STAR
    l=separated_nonempty_list(STAR,tff_app_type)
    { ty :: l }

tff_ty_vars:
  | l=separated_nonempty_list(COMMA, tff_ty_var) { l }

tff_ty_var:
  | v=UPPER_WORD COLUMN TYPE_TY { PT.V v, Some PT.tType }

type_const:
  | WILDCARD { PT.wildcard }
  | w=atomic_word { PT.const w }

variable:
  | x=UPPER_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.var ~loc x
    }

atomic_word:
  | s=SINGLE_QUOTED { remove_quotes s }
  | s=LOWER_WORD { s }

atomic_defined_word:
  | WILDCARD { PT.wildcard }

defined_ty:
  | w=DOLLAR_WORD
    { match w with
      | "$i" -> PT.term
      | "$o" -> PT.prop
      | "$tType" -> PT.tType
      | "$int" -> PT.ty_int
      | "$rat" -> PT.ty_rat
      | "$real" -> PT.ty_real
      | _ ->
          let loc = L.mk_pos $startpos $endpos in
          UntypedAST.errorf loc "expected defined_type, not `%s`" w
    }

atomic_system_word:
  | w=DOLLAR_DOLLAR_WORD { PT.const w }

name_list:
  l=separated_list(COMMA, name) { l }

name:
  | w=atomic_word { Ast_tptp.NameString w }
  | i=INTEGER { Ast_tptp.NameInt (int_of_string i) }

annotations:
  | { [] }
  | COMMA l=separated_list(COMMA, general_term) { l }

general_term:
  | g=general_data { g }
  | l=general_data COLUMN r=general_term { Ast_tptp.GColumn (l, r) }
  | g=general_list { g }

general_data:
  | w=atomic_word { Ast_tptp.GString w }
  | g=general_function { g }
  | i=INTEGER { Ast_tptp.GInt (int_of_string i) }
  | v=UPPER_WORD { Ast_tptp.GVar v }
  | w=DISTINCT_OBJECT { Ast_tptp.GString w }

general_function:
  | f=atomic_word LEFT_PAREN l=separated_nonempty_list(COMMA, general_term) RIGHT_PAREN
    { Ast_tptp.GNode (f, l) }

general_list:
  | LEFT_BRACKET l=separated_list(COMMA, general_term) RIGHT_BRACKET
    { Ast_tptp.GList l }

%%
