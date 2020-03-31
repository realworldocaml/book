(* Original file: zipperposition.1.5/zipperposition-1.5/src/parsers/Parse_zf.mly *)

(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Parser for Zipperposition Formulas} *)

%{
  open Logtk

  module L = ParseLocation
  module A = UntypedAST
  module T = A.T

  let unquote s =
    assert (s <> "");
    assert (s.[0] = '\'' || s.[0] = '"');
    assert (s.[String.length s-1] = '\'' || s.[String.length s-1] = '"');
    let s = String.sub s 1 (String.length s-2) in
    CCString.flat_map
      (function
        | '\\' -> ""
        | c -> String.make 1 c)
      s
%}


%token EOI

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET

%token WILDCARD
%token COMMA
%token DOT
%token SEMI_COLON
%token COLON
%token EQDEF
%token WHERE
%token AND

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
%token LOGIC_EQUIV

%token ARITH_PLUS
%token ARITH_MINUS
%token ARITH_PRODUCT
%token ARITH_LT
%token ARITH_LEQ
%token ARITH_GT
%token ARITH_GEQ

%token IF
%token THEN
%token ELSE

%token MATCH
%token WITH
%token END
%token FUN
%token LET
%token IN

%token INT
%token PROP
%token TYPE

%token ASSERT
%token DATA
%token DEF
%token VAL
%token GOAL
%token REWRITE
%token LEMMA
%token INCLUDE

%token ARROW
%token PI
%token VERTICAL_BAR

%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> QUOTED
%token <string> SINGLE_QUOTED
%token <string> INTEGER

%start <Logtk.UntypedAST.statement> parse_statement
%start <Logtk.UntypedAST.statement list> parse_statement_list
%start <Logtk.UntypedAST.term> parse_term
%start <Logtk.UntypedAST.ty> parse_ty


%%

parse_statement: s=statement EOI {s}
parse_term: t=term EOI {t}
parse_ty: t=term EOI {t}
parse_statement_list: l=list(statement) EOI { l }

/* variable without a location */
raw_var:
  | w=LOWER_WORD { w }
  | w=UPPER_WORD { w }
  | w=SINGLE_QUOTED { unquote w }

var_or_wildcard:
  | v=raw_var { T.V v }
  | WILDCARD { T.Wildcard }

typed_var_block:
  | v=raw_var { [T.V v, None] }
  | WILDCARD { [T.Wildcard, None] }
  | LEFT_PAREN v=raw_var+ COLON t=term RIGHT_PAREN
    { List.map (fun v -> T.V v, Some t) v }

typed_var_list:
  | l=typed_var_block { l }
  | l=typed_var_block l2=typed_var_list { l @ l2 }

typed_ty_var_block:
  | v=raw_var { [T.V v, None] }
  | v=raw_var COLON TYPE { [T.V v, Some T.tType] }
  | LEFT_PAREN v=raw_var+ COLON TYPE RIGHT_PAREN { List.map (fun v -> T.V v, Some T.tType) v }

typed_ty_var_list:
  | l=typed_ty_var_block { l }
  | l=typed_ty_var_block l2=typed_ty_var_list { l @ l2 }

mandatory_typed_var_block:
  | LEFT_PAREN v=raw_var+ COLON t=term RIGHT_PAREN
    { List.map (fun v -> v, t) v }

mandatory_typed_var_list:
  | l=mandatory_typed_var_block { l }
  | l=mandatory_typed_var_block l2=mandatory_typed_var_list { l @ l2 }

var:
  | WILDCARD { T.wildcard }
  | v=raw_var
    {
      let loc = L.mk_pos $startpos $endpos in
      T.var ~loc v
    }

const:
  | TYPE { T.tType }
  | PROP { T.prop }
  | INT { T.ty_int }
  | LOGIC_TRUE { T.true_ }
  | LOGIC_FALSE { T.false_ }

match_branch:
  | VERTICAL_BAR c=raw_var vars=var_or_wildcard* ARROW rhs=term
    { T.Match_case (c,vars,rhs) }

atomic_term:
  | v=var { v }
  | t=const { t }
  | i=INTEGER { T.int_ (Z.of_string i) }
  | LEFT_PAREN t=term RIGHT_PAREN { t }
  | MATCH t=term WITH l=match_branch+ END
    {
      let loc = L.mk_pos $startpos $endpos in
      T.match_ ~loc t l
    }

apply_term:
  | t=atomic_term { t }
  | t=atomic_term u=atomic_term+
    {
      let loc = L.mk_pos $startpos $endpos in
      T.app ~loc t u
    }
  | ARITH_MINUS t=apply_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.app_builtin ~loc Builtin.Uminus [t]
    }

mult_term:
  | t=apply_term { t }
  | a=apply_term ARITH_PRODUCT b=mult_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.app_builtin ~loc Builtin.Product [a;b]
    }

%inline PLUS_OP:
  | ARITH_PLUS { Builtin.Sum }
  | ARITH_MINUS { Builtin.Difference }

plus_term:
  | t=mult_term { t }
  | a=mult_term o=PLUS_OP b=plus_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.app_builtin ~loc o [a;b]
    }

%inline ARITH_OP:
  | ARITH_LT { Builtin.Less }
  | ARITH_LEQ { Builtin.Lesseq }
  | ARITH_GT { Builtin.Greater }
  | ARITH_GEQ { Builtin.Greatereq }

arith_op_term:
  | t=plus_term { t }
  | a=plus_term o=ARITH_OP b=plus_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.app_builtin ~loc o [a;b]
    }

not_term:
  | t=arith_op_term { t }
  | LOGIC_NOT t=arith_op_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.not_ ~loc t
    }

eq_term:
  | t=not_term { t }
  | t=not_term LOGIC_EQ u=not_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.eq ~loc t u
    }
  | t=not_term LOGIC_NEQ u=not_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.neq ~loc t u
    }

and_term:
  | t=eq_term { t }
  | t=eq_term LOGIC_AND u=and_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.and_ ~loc [t; u]
    }

or_term:
  | t=and_term { t }
  | t=and_term LOGIC_OR u=or_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.or_ ~loc [t; u]
    }
  | t=and_term LOGIC_IMPLY u=or_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.imply ~loc t u
    }
  | t=and_term LOGIC_EQUIV u=or_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.equiv ~loc t u
    }

term:
  | t=or_term { t }
  | LOGIC_FORALL vars=typed_var_list DOT t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.forall ~loc vars t
    }
  | LOGIC_EXISTS vars=typed_var_list DOT t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.exists ~loc vars t
    }
  | FUN vars=typed_var_list DOT t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.lambda ~loc vars t
    }
  | t=apply_term ARROW u=term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.fun_ty ~loc [t] u
    }
  | PI vars=typed_ty_var_list DOT t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.forall_ty ~loc vars t
    }
  | IF a=term THEN b=term ELSE c=term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.ite ~loc a b c
    }
  | LET x=raw_var EQDEF t=term IN u=term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.let_ ~loc [T.V x,t] u
    }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      UntypedAST.error loc "expected term"
    }

constructor_arg:
  | ty=atomic_term { None, ty }
  | LEFT_PAREN id=raw_var COLON ty=atomic_term RIGHT_PAREN { Some id, ty }

constructor:
  | v=raw_var l=constructor_arg* { v, l }

constructors:
  | VERTICAL_BAR? l=separated_nonempty_list(VERTICAL_BAR, constructor) { l }

type_def:
  | t=raw_var vars=raw_var* EQDEF l=constructors
    {
      {A. data_name=t; data_vars=vars; data_cstors=l; }
    }

mutual_types:
  | l=separated_nonempty_list(AND, type_def) { l }

attr:
  | a=atomic_attr { a }
  | s=raw_var l=atomic_attr+ { A.A_app (s, l) }
  | error {
      let loc = L.mk_pos $startpos $endpos in
      UntypedAST.error loc "expected attribute"
    }

atomic_attr:
  | s=raw_var { A.A_app (s, []) }
  | s=QUOTED { A.A_quoted (unquote s) }
  | LEFT_PAREN a=attr RIGHT_PAREN { a }
  | LEFT_BRACKET l=separated_list(COMMA, attr) RIGHT_BRACKET { A.A_list l }

attrs:
  | LEFT_BRACKET l=separated_nonempty_list(COMMA, attr) RIGHT_BRACKET
    { l }
  | { [] }

def:
 | v=raw_var COLON ty=term EQDEF t=term
   { A.mk_def v ty [T.eq (T.var v) t] }
 | v=raw_var COLON ty=term WHERE rules=separated_nonempty_list(SEMI_COLON,term)
   { A.mk_def v ty rules }
 | v=raw_var l=mandatory_typed_var_list COLON ty_ret=term EQDEF rhs=term
   {
      let ty_args = List.map snd l in
      let args = List.map (fun (v,_) -> T.var v) l in
      A.mk_def v (T.fun_ty ty_args ty_ret)
        [T.forall
          (List.map (fun (v,ty) -> T.V v, Some ty) l)
          (T.eq (T.app (T.var v) args) rhs)]
   }

statement:
  | INCLUDE s=QUOTED DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      let s = unquote s in
      A.include_ ~attrs:[] ~loc s
    }
  | VAL a=attrs v=raw_var COLON t=term DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.decl ~attrs:a ~loc v t
    }
  | DEF a=attrs l=separated_nonempty_list(AND,def) DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.def ~attrs:a ~loc l
    }
  | REWRITE a=attrs t=term DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.rewrite ~attrs:a ~loc t
    }
  | ASSERT a=attrs t=term DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.assert_ ~attrs:a ~loc t
    }
  | LEMMA a=attrs t=term DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.lemma ~attrs:a ~loc t
    }
  | GOAL a=attrs t=term DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.goal ~attrs:a ~loc t
    }
  | DATA a=attrs l=mutual_types DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.data ~attrs:a ~loc l
    }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      UntypedAST.error loc "expected statement"
    }

%%
