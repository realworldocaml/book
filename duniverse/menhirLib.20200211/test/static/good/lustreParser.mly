(* This file is part of the Kind verifier

 * Copyright (c) 2007-2009 by the Board of Trustees of the University of Iowa,
 * here after designated as the Copyright Holder.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the University of Iowa, nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

%{

module I = LustreIdent
module A = LustreAst

let mk_pos = Lib.position_of_lexing

%}

(*
(* Include directive, unfolded by the lexer *)
%token INCLUDE
%token <string>STRING
*)

(* Special characters *)
%token SEMICOLON
%token EQUALS
%token COLON
%token COMMA
%token LSQBRACKET
%token RSQBRACKET
%token LPAREN
%token RPAREN

(* Tokens for enumerated types *)
%token ENUM

(* Tokens for records *)
%token STRUCT
%token DOT
%token LCURLYBRACKET
%token RCURLYBRACKET

(* Tokens for decimals and numerals *)
%token <string>DECIMAL
%token <string>NUMERAL

(* Identifier token *)
%token <string>SYM

(* Tokens for types *)
%token TYPE
%token INT
%token REAL
%token BOOL
%token SUBRANGE
%token OF

(* Tokens for arrays *)
(* %token ARRAY *)
%token CARET
%token DOTDOT
(* %token LARRAYBRACKET *)
(* %token RARRAYBRACKET *)
%token PIPE

(* Token for constant declarations *)
%token CONST

(* Tokens for node declarations *)
%token NODE
%token LPARAMBRACKET
%token RPARAMBRACKET
%token FUNCTION
%token RETURNS
%token VAR
%token LET
%token TEL

(* Tokens for annotations *)
%token PROPERTY
%token MAIN
%token REQUIRES
%token ENSURES

(* Token for assertions *)
%token ASSERT

(* Tokens for Boolean operations *)
%token TRUE
%token FALSE
%token NOT
%token AND
%token XOR
%token OR
%token IF
%token WITH
%token THEN
%token ELSE
%token IMPL
%token HASH

(* Tokens for relations *)
%token LTE
%token GTE
%token LT
%token GT
%token NEQ

(* Tokens for arithmetic operators *)
%token MINUS
%token PLUS
%token DIV
%token MULT
%token INTDIV
%token MOD

(* Tokens for clocks *)
%token WHEN
%token CURRENT
%token CONDACT

(* Tokens for temporal operators *)
%token PRE
%token FBY
%token ARROW

(* Token for end of file marker *)
%token EOF

(* Priorities and associativity of operators, lowest first *)
%left PIPE
%nonassoc ELSE
%right ARROW
%right IMPL
%left OR XOR
%left AND
%left LT LTE EQUALS NEQ GTE GT
%left PLUS MINUS
%left MULT INTDIV MOD DIV
%nonassoc WHEN
%nonassoc NOT
%nonassoc CURRENT
%nonassoc PRE
%left CARET
%nonassoc INT REAL

(* Start token *)
%start <LustreAst.t> main

%%


(* A Lustre program is a list of declarations *)
main: p = list(decl) EOF { List.flatten p }


(* A declaration is a type, a constant, a node or a function declaration *)
decl:
  | d = const_decl { List.map
                       (function e -> A.ConstDecl (mk_pos $startpos, e))
                       d }
  | d = type_decl { List.map
                      (function e -> A.TypeDecl (mk_pos $startpos, e))
                      d }
  | d = node_decl { [A.NodeDecl (mk_pos $startpos, d)] }
  | d = func_decl { [A.FuncDecl (mk_pos $startpos, d)] }
  | d = node_param_inst { [A.NodeParamInst (mk_pos $startpos, d)] }


(* ********************************************************************** *)


(* A constant declaration *)
const_decl: CONST; l = nonempty_list(const_decl_body) { List.flatten l }

(* The body of a constant declaration *)
const_decl_body:

  (* Imported (free) constant

     Separate rule for singleton list to avoid shift/reduce conflict *)
  | h = ident; COLON; t = lustre_type; SEMICOLON
    { [A.FreeConst (mk_pos $startpos, h, t)] }

  (* Imported (free) constant *)
  | h = ident; COMMA; l = ident_list; COLON; t = lustre_type; SEMICOLON
    { List.map (function e -> A.FreeConst (mk_pos $startpos, e, t)) (h :: l) }

  (* Defined constant without a type *)
  | s = ident; EQUALS; e = expr; SEMICOLON
    { [A.UntypedConst (mk_pos $startpos, s, e)] }

  (* Defined constant with a type *)
  | c = typed_ident; EQUALS; e = expr; SEMICOLON
    { let (s, t) = c in [A.TypedConst (mk_pos $startpos, s, e, t)] }


(* ********************************************************************** *)


(* A type declaration *)
type_decl:

  (* A free type *)
  | TYPE; l = ident_list; SEMICOLON
     { List.map (function e -> A.FreeType (mk_pos $startpos, e)) l }

  (* A type alias *)
  | TYPE; l = ident_list; EQUALS; t = lustre_type; SEMICOLON
     { List.map (function e -> A.AliasType (mk_pos $startpos, e, t)) l }

  (* A record type, can only be defined as alias *)
  | TYPE; l = ident_list; EQUALS; t = record_type; SEMICOLON
     { List.map
         (function e ->
           A.AliasType (mk_pos $startpos,
                        e,
                        A.RecordType (mk_pos $startpos, t)))
         l }


(* A type *)
lustre_type:

  (* Predefined types *)
  | BOOL { A.Bool (mk_pos $startpos) }
  | INT { A.Int (mk_pos $startpos)}
  | REAL { A.Real (mk_pos $startpos)}

  | SUBRANGE;
    LSQBRACKET;
    l = expr;
    COMMA;
    u = expr;
    RSQBRACKET
    OF
    INT
    { A.IntRange (mk_pos $startpos, l, u)}

  (* User-defined type *)
  | s = ident { A.UserType (mk_pos $startpos, s) }

  (* Tuple type *)
  | t = tuple_type { A.TupleType (mk_pos $startpos, t) }

(* Record types can only be defined as aliases
  (* Record type (V6) *)
  | t = record_type { A.RecordType t }
*)

  (* Array type (V6) *)
  | t = array_type { A.ArrayType (mk_pos $startpos, t) }

  (* Enum type (V6) *)
  | t = enum_type { A.EnumType (mk_pos $startpos, t) }


(* A tuple type *)
tuple_type:

  (* Tuples are between square brackets *)
  | LSQBRACKET; l = lustre_type_list; RSQBRACKET { l }


(* A record type (V6) *)
record_type:

  (* Keyword "struct" is optional *)
  | option(STRUCT);
    f = tlist(LCURLYBRACKET, SEMICOLON, RCURLYBRACKET, typed_idents)

    { List.flatten f  }


(* An array type (V6) *)
array_type:
  | t = lustre_type; CARET; s = expr { t, s }

(*
  (* Alternate syntax: array [size] of type *)
  | ARRAY; s = expr; OF; LSQBRACKET; t = lustre_type; RSQBRACKET { t, s }
*)

(* An enum type (V6) *)
enum_type: ENUM LCURLYBRACKET; l = ident_list; RCURLYBRACKET { l }


(* ********************************************************************** *)


(* An uninterpreted function declaration *)
func_decl:
  | FUNCTION;
    n = ident;
    i = tlist(LPAREN, SEMICOLON, RPAREN, typed_idents);
    RETURNS;
    o = tlist(LPAREN, SEMICOLON, RPAREN, typed_idents);
    SEMICOLON;

    { (n, List.flatten i, List.flatten o)  }


(* A node declaration *)
node_decl:
  | NODE;
    n = ident;
    p = loption(static_params);
    i = tlist(LPAREN, SEMICOLON, RPAREN, const_clocked_typed_idents);
    RETURNS;
    o = tlist(LPAREN, SEMICOLON, RPAREN, clocked_typed_idents);
    SEMICOLON;
    r = contract;
    l = list(node_local_decl);
    LET;
    e = list(node_equation);
    TEL
    option(node_sep)

    { (n,
       p,
       List.flatten i,
       List.flatten o,
       (List.flatten l),
       e,
       r)  }


(* A node declaration as an instance of a paramterized node *)
node_param_inst:
  | NODE;
    n = ident;
    EQUALS;
    s = ident;
    p = tlist
         (LPARAMBRACKET, SEMICOLON, RPARAMBRACKET, node_call_static_param);
    SEMICOLON

    { (n, s, p) }


(* A node declaration is optionally terminated by a period or a semicolon *)
node_sep: DOT | SEMICOLON { }

(* A list of contract clauses *)
contract:
  | l = list(contract_clause) { l }


(* A requires or ensures annotation *)
contract_clause:
  | REQUIRES; e = expr; SEMICOLON { A.Requires (mk_pos $startpos, e) }
  | ENSURES; e = expr; SEMICOLON { A.Ensures (mk_pos $startpos, e) }


(* A static parameter is a type *)
static_param:
  | TYPE; t = ident { A.TypeParam t }


(* The static parameters of a node *)
static_params:
  | l = tlist (LPARAMBRACKET, SEMICOLON, RPARAMBRACKET, static_param)

    { l }


(* A node-local declaration of constants or variables *)
node_local_decl:
  | c = const_decl { List.map
                       (function e -> A.NodeConstDecl (mk_pos $startpos, e))
                       c }
  | v = var_decls { List.map
                      (function e -> A.NodeVarDecl (mk_pos $startpos, e))
                      v }


(* A variable declaration section of a node *)
var_decls:
  | VAR; l = nonempty_list(var_decl) { List.flatten l }


(* A declaration of variables *)
var_decl:
  | l = clocked_typed_idents; SEMICOLON { l }


(* An equations of a node *)
node_equation:

  (* An assertion *)
  | ASSERT; e = expr; SEMICOLON
    { A.Assert (mk_pos $startpos, e) }

  (* An equation, multiple (optionally parenthesized) identifiers on
     the left-hand side, an expression on the right *)
  | l = left_side; EQUALS; e = expr; SEMICOLON
    { A.Equation (mk_pos $startpos, l, e) }

  (* Node annotation *)
  | MAIN { A.AnnotMain }

  (* Property annotation *)
  | PROPERTY; e = expr; SEMICOLON { A.AnnotProperty (mk_pos $startpos, e) }


left_side:

  (* List without parentheses *)
  | l = struct_item_list { l }

  (* Parenthesized list *)
  | LPAREN; l = struct_item_list; RPAREN { l }

  (* Empty list *)
  | LPAREN; RPAREN { [] }


(* Item in a structured equation *)
struct_item:

  (* Single identifier *)
  | s = ident
     { A.SingleIdent (mk_pos $startpos, s) }

  | s = ident; l = nonempty_list(one_index)
     { A.IndexedIdent (mk_pos $startpos, s, l)}

(*
  (* Filter array values *)
  | LSQBRACKET; l = struct_item_list; RSQBRACKET
    { A.TupleStructItem (mk_pos $startpos, l) }

  (* Select from tuple *)
  | e = ident; LSQBRACKET; i = expr; RSQBRACKET
    { A.TupleSelection (mk_pos $startpos, e, i) }

  (* Select from record *)
  | e = ident; DOT; i = ident
    { A.FieldSelection (mk_pos $startpos, e, i) }

  | e = ident; LSQBRACKET; s = array_slice_list; RSQBRACKET
    { A.ArraySliceStructItem (mk_pos $startpos, e, s) }
*)

(* List of structured items *)
struct_item_list:
 | l = separated_nonempty_list(COMMA, struct_item) { l }


(* ********************************************************************** *)

(* An expression *)
expr:

  (* An identifier *)
  | s = ident { A.Ident (mk_pos $startpos, s) }

  (* A propositional constant *)
  | TRUE { A.True (mk_pos $startpos) }
  | FALSE { A.False (mk_pos $startpos) }

  (* An integer numeral or a floating-point decimal constant *)
  | s = NUMERAL { A.Num (mk_pos $startpos, s) }
  | s = DECIMAL { A.Dec (mk_pos $startpos, s) }

  (* Conversions *)
  | INT; e = expr { A.ToInt (mk_pos $startpos, e) }
  | REAL; e = expr { A.ToReal (mk_pos $startpos, e) }

  (* A parenthesized single expression *)
  | LPAREN; e = expr; RPAREN { e }

  (* An expression list

     Singleton list is in production above *)
  | LPAREN; h = expr; COMMA; l = expr_list; RPAREN
    { A.ExprList (mk_pos $startpos, h :: l) }

  (* A tuple expression *)
  | LSQBRACKET; l = expr_list; RSQBRACKET { A.TupleExpr (mk_pos $startpos, l) }

  (* An array constructor *)
  | e1 = expr; CARET; e2 = expr { A.ArrayConstr (mk_pos $startpos, e1, e2) }

  (* A tuple projection

     TODO: allow multiple projections, return a list: a[0][0][0] *)
  | e = ident; LSQBRACKET; i = expr ; RSQBRACKET
    { A.TupleProject (mk_pos $startpos, e, i) }

  (* A multidimensional array slice *)
  | e = ident; LSQBRACKET; l = array_slice_list; RSQBRACKET
    { A.ArraySlice (mk_pos $startpos, e, l) }

  (* A record field projection *)
  | s = ident; DOT; t = ident
    { A.RecordProject (mk_pos $startpos, s, I.index_of_ident t) }

  (* A record *)
  | t = ident;
    f = tlist(LCURLYBRACKET, SEMICOLON, RCURLYBRACKET, record_field_assign)
    { A.RecordConstruct (mk_pos $startpos, t, f) }

  (* An array concatenation *)
  | e1 = expr; PIPE; e2 = expr { A.ArrayConcat (mk_pos $startpos, e1, e2) }

  (* An arithmetic operation *)
  | e1 = expr; MINUS; e2 = expr { A.Minus (mk_pos $startpos, e1, e2) }
  | MINUS; e = expr { A.Uminus (mk_pos $startpos, e) }
  | e1 = expr; PLUS; e2 = expr { A.Plus (mk_pos $startpos, e1, e2) }
  | e1 = expr; MULT; e2 = expr { A.Times (mk_pos $startpos, e1, e2) }
  | e1 = expr; DIV; e2 = expr { A.Div (mk_pos $startpos, e1, e2) }
  | e1 = expr; INTDIV; e2 = expr { A.IntDiv (mk_pos $startpos, e1, e2) }
  | e1 = expr; MOD; e2 = expr { A.Mod (mk_pos $startpos, e1, e2) }

  (* A Boolean operation *)
  | NOT; e = expr { A.Not (mk_pos $startpos, e) }
  | e1 = expr; AND; e2 = expr { A.And (mk_pos $startpos, e1, e2) }
  | e1 = expr; OR; e2 = expr { A.Or (mk_pos $startpos, e1, e2) }
  | e1 = expr; XOR; e2 = expr { A.Xor (mk_pos $startpos, e1, e2) }
  | e1 = expr; IMPL; e2 = expr { A.Impl (mk_pos $startpos, e1, e2) }
  | HASH; LPAREN; e = expr_list; RPAREN { A.OneHot (mk_pos $startpos, e) }

  (* A relation *)
  | e1 = expr; LT; e2 = expr { A.Lt (mk_pos $startpos, e1, e2) }
  | e1 = expr; GT; e2 = expr { A.Gt (mk_pos $startpos, e1, e2) }
  | e1 = expr; LTE; e2 = expr { A.Lte (mk_pos $startpos, e1, e2) }
  | e1 = expr; GTE; e2 = expr { A.Gte (mk_pos $startpos, e1, e2) }
  | e1 = expr; EQUALS; e2 = expr { A.Eq (mk_pos $startpos, e1, e2) }
  | e1 = expr; NEQ; e2 = expr { A.Neq (mk_pos $startpos, e1, e2) }

  (* An if operation *)
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
    { A.Ite (mk_pos $startpos, e1, e2, e3) }

  (* Recursive node call *)
  | WITH; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
    { A.With (mk_pos $startpos, e1, e2, e3) }

  (* A clock operation *)
  | e1 = expr; WHEN; e2 = expr { A.When (mk_pos $startpos, e1, e2) }
  | CURRENT; e = expr { A.Current (mk_pos $startpos, e) }
  | CONDACT
    LPAREN;
    e1 = expr;
    COMMA;
    s = ident; LPAREN; a = separated_list(COMMA, expr); RPAREN;
    COMMA;
    v = expr_list
    RPAREN
    { A.Condact (mk_pos $startpos, e1, s, a, v) }

  (* A temporal operation *)
  | PRE; e = expr { A.Pre (mk_pos $startpos, e) }
  | FBY LPAREN; e1 = expr COMMA; s = NUMERAL; COMMA; e2 = expr RPAREN
    { A.Fby (mk_pos $startpos, e2, (int_of_string s), e2) }
  | e1 = expr; ARROW; e2 = expr { A.Arrow (mk_pos $startpos, e1, e2) }

  (* A node or function call *)
  | e = node_call { e }


(* Static parameters are only types *)
node_call_static_param:
  | t = lustre_type { t }


(* A node or function call *)
node_call:

  (* Call a node without static parameters *)
  | s = ident; LPAREN; a = separated_list(COMMA, expr); RPAREN
    { A.Call (mk_pos $startpos, s, a) }

  (* Call a node with static parameters *)
  | s = ident;
    p = tlist
         (LPARAMBRACKET, SEMICOLON, RPARAMBRACKET, node_call_static_param);
    LPAREN;
    a = separated_list(COMMA, expr);
    RPAREN
    { A.CallParam (mk_pos $startpos, s, p, a) }


(* A list of expressions *)
expr_list: l = separated_nonempty_list(COMMA, expr) { l }


(* An array slice *)
array_slice: il = expr; DOTDOT; iu = expr { il, iu }

(* A list of array slices *)
array_slice_list: l = separated_nonempty_list(COMMA, array_slice) { l }


(* An assignment to a record field *)
record_field_assign: s = ident; EQUALS; e = expr { (s, e) }


(* ********************************************************************** *)


clock_expr:
  | c = ident { A.ClockPos c }
  | NOT; c = ident { A.ClockNeg c }
  | NOT; LPAREN; c = ident; RPAREN { A.ClockNeg c }
  | TRUE { A.ClockTrue }


(* ********************************************************************** *)


(* An identifier *)
ident: s = SYM { I.mk_string_ident s }


(* An identifier with a type *)
typed_ident: s = ident; COLON; t = lustre_type { (s, t) }


(* A comma-separated list of identifiers *)
ident_list:
  | l = separated_nonempty_list(COMMA, ident) { l }


(* A comma-separated list of types *)
lustre_type_list:
  | l = separated_nonempty_list(COMMA, lustre_type) { l }


(* A list of comma-separated identifiers with a type *)
typed_idents:
  | l = separated_nonempty_list(COMMA, ident); COLON; t = lustre_type

    (* Pair each identifier with the type *)
    { List.map (function e -> (e, t)) l }

(*
(* A list of lists of typed identifiers *)
typed_idents_list:
  | a = separated_list(SEMICOLON, typed_idents)

    (* Return a flat list *)
    { List.flatten a }
*)

(* Typed identifiers that may be constant *)
const_typed_idents:
  | o = boption(CONST); l = typed_idents

    (* Pair each typed identifier with a flag *)
    { List.map (function (e, t) -> (e, t, o)) l }

(*
(* A list of lists of typed identifiers that may be constant *)
const_typed_idents_list:
  | a = separated_list(SEMICOLON, const_typed_idents)

    (* Return a flat list *)
    { List.flatten a }
*)

(* A list of comma-separated identifiers with a type *)
clocked_typed_idents:

  (* Unclocked typed identifiers *)
  | l = typed_idents

    (* Pair each typed identifier with the base clock *)
    { List.map (function (e, t) -> (mk_pos $startpos, e, t, A.ClockTrue)) l }

  (* Clocked typed identifiers *)
  | l = typed_idents; WHEN; c = clock_expr
  | LPAREN; l = typed_idents; RPAREN; WHEN; c = clock_expr

    (* Pair each types identifier the given clock *)
    { List.map (function (e, t) -> (mk_pos $startpos, e, t, c)) l }

  (* Separate rule for non-singleton list to avoid shift/reduce conflict *)
  | LPAREN;
    h = typed_idents;
    SEMICOLON;
    l = tlist_tail(SEMICOLON, RPAREN, typed_idents);
    WHEN;
    c = clock_expr

    (* Pair each types identifier the given clock *)
    { List.map
        (function (e, t) -> (mk_pos $startpos, e, t, c))
        (h @ (List.flatten l)) }


(*
(* A list of lists of typed and clocked identifiers *)
clocked_typed_idents_list:
  | a = separated_list(SEMICOLON, clocked_typed_idents)

    (* Return a flat list *)
    { List.flatten a }
*)

(* A list of comma-separated identifiers with a type and a clock that may be constant *)
const_clocked_typed_idents:

  (* Unclocked typed identifiers *)
  | l = const_typed_idents

    (* Pair each typed identifier with the base clock *)
    { List.map
        (function (e, t, o) -> (mk_pos $startpos, e, t, A.ClockTrue, o))
        l }

  (* Clocked typed identifiers *)
  | l = const_typed_idents; WHEN; c = clock_expr
  | LPAREN; l = const_typed_idents; RPAREN; WHEN; c = clock_expr

    (* Pair each types identifier the given clock *)
    { List.map
        (function (e, t, o) -> (mk_pos $startpos, e, t, c, o))
        l }

  (* Separate rule for non-singleton list to avaoid shift/reduce conflict *)
  | LPAREN;
    h = const_typed_idents;
    SEMICOLON;
    l = tlist_tail(SEMICOLON, RPAREN, const_typed_idents);
    WHEN;
    c = clock_expr

    (* Pair each types identifier the given clock *)
    { List.map (function (e, t, o) -> (mk_pos $startpos, e, t, c, o)) (h @ (List.flatten l)) }

(*
(* A list of lists of typed and clocked identifiers that may be constant *)
const_clocked_typed_idents_list:
  | a = separated_list(SEMICOLON, const_clocked_typed_idents)

      { List.flatten a }
*)

(* The tail of a list of X *)
tlist_tail(separator, closing, X):
  | x = X; option(separator); closing { [ x ] }
  | x = X; separator; xs = tlist_tail(separator, closing, X)
    { x :: xs }

(* A list of elements between opening and closing, separated by separator,
   the separator may occur at the end of the list *)
tlist(opening, separator, closing, X):
  | opening; l = tlist_tail(separator, closing, X) { l }
  | opening; closing { [ ] }

(* ********************************************************************** *)

(* An index *)
one_index:

  (* An index into a record *)
  | DOT; i = ident
     { A.FieldIndex (mk_pos $startpos, i) }

  (* An index into an array with a variable or constant *)
  | LSQBRACKET; i = ident; RSQBRACKET
     { A.VarIndex (mk_pos $startpos, i) }

  (* An index into an array with a numeral *)
  | LSQBRACKET; s = NUMERAL; RSQBRACKET
     { A.NumIndex (mk_pos $startpos, int_of_string s) }




(*
   Local Variables:
   compile-command: "make -k"
   indent-tabs-mode: nil
   End:
*)

