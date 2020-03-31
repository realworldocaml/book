/* Parser for naml
 * Dylan Simon
 * vim:sts=4:sw=4
 */
%{

open Naml_ast
open Naml_parse_misc
open Naml_ast_util
open Naml_ast_exn

let make_infix_expr op e1 e2 =
  let (op,p) = op in
  let up = union_pos (pos_of_expr e1) (pos_of_expr e2) in
    ApplyExpr (VarExpr (([],op,p),p), [e1; e2], up)

let make_prefix_expr op e =
  let (op,p) = op in
    ApplyExpr (VarExpr (([],op,p),p), [e], union_pos p (pos_of_expr e))

let make_function_nest p a w e =
  let p = union_pos p (pos_of_expr e) in
  let e = match w with
      None -> e
    | Some _ ->
        let u = CConsConst (([],unit_sym,p),p) in
          MatchExpr (ConstExpr (u,p), [ConstPat (u,p), w, e], p) in
  let e, a = Mc_list_util.fold_map
    (fun e -> function
         IdPat (s, _) -> e, s
       | pt ->
           let p = pos_of_pattern pt in
           let s = Symbol.new_symbol_string "x" in
             MatchExpr (VarExpr (([],s,p),p), [pt, None, e], p), s) e a
  in
    FunExpr (a, e, p)

let make_list_nil p = CConsConst (([],list_nil_sym,p), p)

let make_list_cons_expr_aux cp h t p =
  ConsExpr (([], list_cons_sym, cp), TupleExpr ([h; t], p), p)

let make_list_cons_expr cp h t =
  make_list_cons_expr_aux cp h t (union_pos (pos_of_expr h) (pos_of_expr t))

let rec make_list_expr = function
    SeqExpr (e1, e2, p) ->
	ConsExpr (([], list_cons_sym, p), TupleExpr ([e1; make_list_expr e2], p), p)
  | e -> let p = pos_of_expr e in
	ConsExpr (([], list_cons_sym, p), TupleExpr ([e; ConstExpr (CConsConst (([], list_nil_sym, p), p) ,p)], p), p)

let make_array_expr p e =
    let rec make_array_list = function
	SeqExpr (e1, e2, _) ->
	    e1 :: make_array_list e2
      | e ->
	    [e] in
    ArrayExpr (make_array_list e, p)

let make_list_cons_pat_aux cp h t p =
  ConsPat (([], list_cons_sym, cp), TuplePat ([h; t], p), p)

let make_list_cons_pat cp h t =
  make_list_cons_pat_aux cp h t (union_pos (pos_of_pattern h) (pos_of_pattern t))

let rec make_list_pat p = function
    [] -> ConstPat (make_list_nil p, p)
  | h :: t -> make_list_cons_pat_aux p h (make_list_pat p t) p

let rec make_functor_expr al e p =
  match al with
      [] -> e
    | (s, t) :: al -> FunctorMod (s, t, make_functor_expr al e p, p)

let rec make_functor_type al mt p =
  match al with
      [] -> mt
    | (s, t) :: al -> FunctorModtype (s, t, make_functor_type al mt p, p)

let make_constrain_expr e t p =
  match t with
      None -> e
    | Some t -> ConstrainMod (e, t, p)

let string_to_list s =
  let rec tol i l =
    let i = i - 1 in
      if i < 0 then
        l
      else
        tol i (s.[i] :: l)
  in
    tol (String.length s) ['\000']

%}

/* Tokens */
%token <char * Naml_ast.pos> TokChar
%token <int * Naml_ast.pos> TokInt
%token <float * Naml_ast.pos> TokFloat
%token <string * Naml_ast.pos> TokString

%token <Naml_ast.pos> TokAlt
%token <Naml_ast.pos> TokRArrow
%token <Naml_ast.pos> TokIs
%token <Naml_ast.pos> TokSemi
%token <Naml_ast.pos> TokSemiSemi
%token <Naml_ast.pos> TokComma
%token <Naml_ast.pos> TokMeta
%token <Naml_ast.pos> TokAny

%token <Naml_ast.pos> TokLParen
%token <Naml_ast.pos> TokRParen
%token <Naml_ast.pos> TokLList
%token <Naml_ast.pos> TokRList
%token <Naml_ast.pos> TokLArray
%token <Naml_ast.pos> TokRArray
/*%token TokLStream*/
/*%token TokRStream*/
%token <Naml_ast.pos> TokLRecord
%token <Naml_ast.pos> TokRRecord
/*%token TokLSelf*/
/*%token TokRSelf*/

%token <Naml_ast.pos> TokDot
%token <Naml_ast.pos> TokCons
%token <Naml_ast.pos> TokLArrow
%token <Naml_ast.pos> TokDotDot

/*%token TokPoly*/
/*%token TokHash*/
/*%token TokLabel*/
/*%token TokOptLabel*/
/*%token TokWhatWhat*/


%token <Symbol.symbol * Naml_ast.pos> TokOpPreN1
%token <Symbol.symbol * Naml_ast.pos> TokOpInR6
%token <Naml_ast.pos> TokStar
%token <Symbol.symbol * Naml_ast.pos> TokOpInL7
%token <Naml_ast.pos> TokOpMinus
%token <Naml_ast.pos> TokOpMinusDot
%token <Symbol.symbol * Naml_ast.pos> TokOpInL8
%token <Symbol.symbol * Naml_ast.pos> TokOpInR10
%token <Naml_ast.pos> TokEq
%token <Symbol.symbol * Naml_ast.pos> TokOpInL11
%token <Symbol.symbol * Naml_ast.pos> TokOpInN12
%token <Symbol.symbol * Naml_ast.pos> TokOpInL13
%token <Symbol.symbol * Naml_ast.pos> TokOpInL14
%token <Symbol.symbol * Naml_ast.pos> TokOpInR16
%token <Naml_ast.pos> TokSSAnd
%token <Naml_ast.pos> TokSSOr

%token <Naml_ast.pos> TokOpen
%token <Naml_ast.pos> TokInclude
%token <Naml_ast.pos> TokAssert
%token <Naml_ast.pos> TokBegin
%token <Naml_ast.pos> TokEnd
%token <Naml_ast.pos> TokType
%token <Naml_ast.pos> TokException
%token <Naml_ast.pos> TokOf
%token <Naml_ast.pos> TokVal
%token <Naml_ast.pos> TokExternal
/*%token TokConstraint*/
%token <Naml_ast.pos> TokLet
%token <Naml_ast.pos> TokIn
%token <Naml_ast.pos> TokFun
%token <Naml_ast.pos> TokFunction
%token <Naml_ast.pos> TokRec
%token <Naml_ast.pos> TokAnd
%token <Naml_ast.pos> TokIf
%token <Naml_ast.pos> TokThen
%token <Naml_ast.pos> TokElse
%token <Naml_ast.pos> TokFor
%token <Naml_ast.pos> TokTo
%token <Naml_ast.pos> TokDownto
%token <Naml_ast.pos> TokWhile
%token <Naml_ast.pos> TokDo
%token <Naml_ast.pos> TokDone
%token <Naml_ast.pos> TokMatch
%token <Naml_ast.pos> TokTry
%token <Naml_ast.pos> TokWith
%token <Naml_ast.pos> TokAs
%token <Naml_ast.pos> TokWhen
%token <Naml_ast.pos> TokStruct
%token <Naml_ast.pos> TokModule
%token <Naml_ast.pos> TokMutable
%token <Naml_ast.pos> TokSig
%token <Naml_ast.pos> TokFunctor
/*%token TokLazy*/
/*%token TokParser*/
/*%token TokClass*/
/*%token TokNew*/
/*%token TokInherit*/
/*%token TokMethod*/
/*%token TokVirtual*/
/*%token TokPrivate*/
/*%token TokClosed*/

%token <Symbol.symbol * Naml_ast.pos> TokCapId
%token <Symbol.symbol * Naml_ast.pos> TokId

%token <Naml_ast.pos> TokEOF

/********************************************************** Precedences */


%nonassoc   prec_functor
%nonassoc   TokWith
%nonassoc   prec_expr_const TokInt TokChar TokFloat TokString TokId
%nonassoc   prec_expr_n19   TokLet TokMatch TokFun TokFunction TokTry /* let match fun function try */
%right	    prec_expr_r18   TokSemi /* ; */
%nonassoc   prec_expr_if    TokIf /* if */
%nonassoc   prec_expr_ifelse TokElse /* if .. else */
%nonassoc   prec_as	    TokAs
%left	    prec_pat_alt    TokAlt
%right	    prec_ty_fun	    TokRArrow
%right	    prec_expr_r16   TokOpInR16 TokLArrow /* <- := */
%right	    prec_expr_n15 prec_pat_tuple TokComma /* , */
%left	    prec_expr_l14   TokOpInL14 TokSSOr /* or || */
%left	    prec_expr_l13   TokOpInL13 TokSSAnd /* & && */
%nonassoc   prec_expr_n12   TokOpInN12 /* not */
%left	    prec_expr_l11   TokOpInL11 TokEq /* comparisons, misc */
%right	    prec_expr_r10   TokOpInR10 /* @ ^ */
%right	    prec_expr_r9 prec_pat_listcons TokCons /* :: */
%left	    prec_expr_l8    TokOpInL8 TokOpMinus TokOpMinusDot /* + - */
%left	    prec_expr_l7    TokOpInL7 TokStar /* * / % mod */
%nonassoc   prec_ty_tuple   /* TokStar */
%right	    prec_expr_r6    TokOpInR6 /* ** */
%nonassoc   prec_expr_n5    /* TokOpMinus TokOpMinusDot */ /* - -. (prefix) */
%nonassoc   prec_expr_n4 prec_pat_cons prec_ty_cons /* constructors */
%left	    prec_expr_l3    /* functions */
%nonassoc   prec_expr_n2    TokDot /* TokRArrow */ /* projections/extractions */
%nonassoc   prec_expr_n1    TokOpPreN1 /* prefix misc */
%nonassoc   TokCapId
%nonassoc   TokLParen TokBegin TokLList TokLRecord TokLArray

/*
 * Declare the "start" productions.
 * These declarations define functions for parsing
 * the productions.  You can define as many as you want.
 */
%start prog
%start interface
%start topstmt
%type <Naml_ast.def list> prog
%type <Naml_ast.spec list> interface
%type <Naml_ast.def option> topstmt

%%

topstmt:
      def TokSemiSemi		    { Some $1 }
    | TokEOF			    { None }
    ;

prog:
      def_list TokEOF		    { $1 }
    ;

interface:
      spec_list TokEOF		    { $1 }
    ;

/********************************************************** Idents */

/* symbol * pos */
ident:
      TokId			    { $1 }
    | TokLParen operator TokRParen  { $2 }
    ;

cap_ident:
      TokCapId		    { $1 }
    | TokLParen TokRParen   { unit_sym, union_pos $1 $2 }
    | TokLList TokRList	    { list_nil_sym, union_pos $1 $2 }
    ;

/* mpath = (symbol * pos) list */
mpath:
      TokCapId			{ [$1] }
    | TokCapId TokDot mpath	{ $1 :: $3 }
    ;

/* vpath = mpath * symbol * pos */
vpath:
      ident			{ let (s, p) = $1 in [], s, p }
    | TokCapId TokDot vpath	{ let (s, p) = $1 and (m, s2, p2) = $3 in ((s,p)::m, s2, union_pos p p2) }
    ;

pure_vpath:
      TokId                     { let (s, p) = $1 in [], s, p }
    | TokCapId TokDot vpath     { let (s, p) = $1 and (m, s2, p2) = $3 in ((s,p)::m, s2, union_pos p p2) }
    ;

/* vpath = mpath * symbol * pos */
vpath_cap:
      cap_ident			 { let (s, p) = $1 in [], s, p }
    | TokCapId TokDot vpath_cap { let (s, p) = $1 and (m, s2, p2) = $3 in ((s,p)::m, s2, union_pos p p2) }
    ;

/* symbol * pos */
operator:
      TokOpPreN1		    { $1 }
    | TokOpMinus		    { Symbol.add "-", $1 }
    | TokOpMinusDot		    { Symbol.add "-.", $1 }
    | TokOpInR6			    { $1 }
    | TokOpInL7			    { $1 }
    | TokOpInL8			    { $1 }
    | TokOpInR10		    { $1 }
    | TokOpInL11		    { $1 }
    | TokOpInN12		    { $1 }
    | TokOpInL13		    { $1 }
    | TokOpInL14		    { $1 }
    | TokOpInR16		    { $1 }
    | TokEq			    { Symbol.add "=", $1 }
    | TokStar			    { Symbol.add "*", $1 }
    ;

/********************************************************** Modules */
ext_mpath:
      TokCapId					{ let (s,p) = $1 in VarEmpath (s,p) }
    | ext_mpath TokDot ext_mpath		{ ProjEmpath ($1, $3, union_pos (pos_of_empath $1) (pos_of_empath $3)) }
    | ext_mpath TokLParen ext_mpath TokRParen	{ ApplyEmpath ($1, $3, union_pos (pos_of_empath $1) $4) }
    ;

module_type:
      mpath				{ VarModtype ($1, pos_of_mpath $1) }
    | TokLParen module_type TokRParen   { $2 }
    | TokSig spec_list TokEnd		{ SigModtype ($2, union_pos $1 $3) }
    | module_type_constraints		{ $1 }
    | TokFunctor TokLParen TokCapId TokIs module_type TokRParen TokRArrow module_type	%prec prec_functor
        { FunctorModtype (fst $3, $5, $8, union_pos $1 (pos_of_modtype $8)) }
    ;

module_type_constraints:
      module_type TokWith TokType type_params pure_vpath TokEq type_expr
        { ConstraintTypeModtype ($1, fst $4, $5, $7, union_pos (pos_of_modtype $1) (pos_of_type $7)) }
    | module_type TokWith mpath TokEq ext_mpath
	{ ConstraintModModtype ($1, $3, $5, union_pos (pos_of_modtype $1) (pos_of_empath $5)) }
    | module_type_constraints TokAnd TokType type_params pure_vpath TokEq type_expr
        { ConstraintTypeModtype ($1, fst $4, $5, $7, union_pos (pos_of_modtype $1) (pos_of_type $7)) }
    | module_type_constraints TokAnd mpath TokEq ext_mpath
        { ConstraintModModtype ($1, $3, $5, union_pos (pos_of_modtype $1) (pos_of_empath $5)) }
    ;

spec_list:
				    { [] }
    | spec_list spec		    { $1 @ [$2] }
    | spec_list TokSemiSemi spec    { $1 @ [$3] }
    ;

spec:
      TokVal ident TokIs type_expr			{ ValSpec (fst $2, $4, union_pos $1 (pos_of_type $4)) }
    | TokExternal ident TokIs type_expr TokEq TokString	{ let  (s, p) = $6 in ExternSpec (fst $2, $4, s, union_pos $1 p) }
    | TokExternal TokType type_params TokId TokEq TokString
        { let (s, p) = $6 in ExternTypeSpec (fst $3, fst $4, s, union_pos $1 p) }
    | type_definition					{ let (l, p) = $1 in TypeSpec (l, p) }
    | exception_def_simp				{ let (s, t, p) = $1 in ExnSpec (s, t, p) }
    | TokModule TokCapId module_args TokIs module_type
        { let p = union_pos $1 (pos_of_modtype $5) in
            ModSpec (fst $2, make_functor_type $3 $5 p, p) }
    | TokModule TokType TokCapId			{ let (s, p) = $3 in ModtypeSpec (s, None, union_pos $1 p) }
    | TokModule TokType TokCapId TokEq module_type	{ ModtypeSpec (fst $3, Some $5, union_pos $1 (pos_of_modtype $5)) }
    | TokOpen mpath					{ OpenSpec ($2, union_pos $1 (pos_of_mpath $2)) }
    | TokInclude ext_mpath				{ IncludeSpec ($2, union_pos $1 (pos_of_empath $2)) }
    ;

module_args:
								    { [] }
    | module_args TokLParen TokCapId TokIs module_type TokRParen   { $1 @ [fst $3, $5] }
    ;

/* modexp */
module_expr:
      mpath						{ VarMod ($1, pos_of_mpath $1) }
    | TokLParen module_expr TokRParen			{ $2 }
    | TokStruct def_list TokEnd				{ StructMod ($2, union_pos $1 $3) }
    | module_expr TokLParen module_expr TokRParen	{ ApplyMod ($1, $3, union_pos (pos_of_modexp $1) $4) }
    | TokLParen module_expr TokIs module_type TokRParen { ConstrainMod ($2, $4, union_pos $1  $5) }
    | TokFunctor TokLParen TokCapId TokIs module_type TokRParen TokRArrow module_expr	%prec prec_functor
        { FunctorMod (fst $3, $5, $8, union_pos $1 (pos_of_modexp $8)) }
    ;

/* def list */
def_list:
				    {[]}
    | def def_list		    { $1 :: $2 }
    | def TokSemiSemi def_list	    { $1 :: $3 }
    ;

/* def */
def:
      TokLet let_rec_opt let_binding_list		{ let (_,_,p) = list_last $3 in LetMod ($2, $3, union_pos $1 p) }
    | TokExternal ident TokIs type_expr TokEq TokString	{ let (s, p) = $6 in ExternMod (fst $2, $4, s, union_pos $1 p) }
    | TokExternal TokType type_params TokId TokEq TokString
        { let (s, p) = $6 in ExternTypeMod (fst $3, fst $4, s, union_pos $1 p) }
    | type_definition					{ let (l, p) = $1 in TypeMod (l, p) }
    | exception_def					{ $1 }
    | TokOpen mpath					{ OpenMod ($2, union_pos $1 (pos_of_mpath $2)) }
    | TokInclude module_expr				{ IncludeMod ($2, union_pos $1 (pos_of_modexp $2)) }
    | TokModule TokType TokCapId TokEq module_type	{ ModtypeMod (fst $3, $5, union_pos $1 (pos_of_modtype $5)) }
    | TokModule TokCapId module_args module_contrain_opt TokEq module_expr
        { let p = union_pos $1 (pos_of_modexp $6) in
            ModMod (fst $2, make_functor_expr $3 (make_constrain_expr $6 $4 p) p, p) }
    ;

module_contrain_opt:
			    { None }
    | TokIs module_type	    { Some $2 }

type_definition:
      TokType type_def_list { let (_, _, _, _, p) = list_last $2 in $2, union_pos $1 p }
    ;

/* tydef list */
type_def_list:
      type_def			    { [$1] }
    | type_def TokAnd type_def_list { $1::$3 }
    ;

/* tydef = symbol list * symbol * ty option * tyrep option * pos */
type_def:
      type_params TokId type_info
	{
	    let (pl, p) = $1 in
	    let (s, _) = $2 in
	    let (t, r, p2) = $3 in
		pl, s, t, r, union_pos p p2
	}
    ;

/* symbol list * pos */
type_params:
			    { [], Naml_parse_state.current_position () }
    | TokMeta TokId	    { let (s, p) = $2 in [s], union_pos $1 p }
    | TokLParen type_param_list TokRParen { $2, union_pos $1 $3 }
    ;

/* symbol list */
type_param_list:
    | TokMeta TokId				{ [fst $2] }
    | type_param_list TokComma TokMeta TokId	{ $1 @ [fst $4] }
    ;

/* ty option * tyrep option * pos */
type_info:
								    { None, None, Naml_parse_state.current_position () }
    | TokEq type_expr						    { Some $2, None, pos_of_type $2 }
    | TokEq constr_decl_list					    { let l,p = $2 in None, Some (TypeCons (l,p)), p }
    | TokEq type_expr TokEq constr_decl_list			    { let l,p = $4 in Some $2, Some (TypeCons (l,p)), p }
    | TokEq TokLRecord field_decl_list TokRRecord	    	    { let p = union_pos $2 $4 in None, Some (TypeRecord ($3, p)), p }
    | TokEq type_expr TokEq TokLRecord field_decl_list TokRRecord   { let p = union_pos $4 $6 in (Some $2, Some (TypeRecord ($5, p)), p) }
    ;

/* (symbol * ty option) list * pos */
constr_decl_list:
      constr_decl	    { let (s, t, p) = $1 in [s, t], p }
    | constr_decl_list TokAlt constr_decl
	{
	    let (s, t, p) = $3 in
            let l, p2 = $1 in
	      l @ [s, t], union_pos p p2
	}
    ;

field_decl_list:
      field_decl_list_aux	    { $1 }
    | field_decl_list_aux TokSemi   { $1 }

/* (bool * symbol * ty) list */
field_decl_list_aux:
      mutable_opt TokId TokIs type_expr				    { [$1, fst $2, $4] }
    | field_decl_list_aux TokSemi mutable_opt TokId TokIs type_expr { $1 @ [$3, fst $4, $6] }
    ;

/* symbol * ty option * pos */
constr_decl:
      cap_ident
        { let (s, p) = $1 in s, [], p }
    | constr_cap_ident TokOf type_tuple_aux
        { let (s, p) = $1 in s, $3, union_pos p (pos_of_type (list_last $3)) }
    ;

constr_cap_ident:
      cap_ident			{ $1 }
    | TokCons			{ list_cons_sym, $1 }
    ;

mutable_opt:
                    { false }
    | TokMutable    { true }
    ;

/* exndef */
exception_def:
      exception_def_simp    { let (s, t, p) = $1 in ExnMod (ExnCons (s, t, p), p) }
    | TokException cap_ident TokEq vpath_cap
	{
	    let (id, _) = $2 in
	    let (_, _, p) = $4 in
	    let p = union_pos $1 p in
		ExnMod (ExnVar (id, $4, p), p)
	}
    ;

exception_def_simp:
      TokException cap_ident
        {
            let (id, p) = $2 in
            let p = union_pos $1 p in
                id, [], p
        }
    | TokException cap_ident TokOf type_tuple_aux
        {
            let (id, _) = $2 in
            let p = union_pos $1 (pos_of_type (list_last $4)) in
                id, $4, p
        }


/* bool */
let_rec_opt:
			{ false }
    | TokRec		{ true }
    ;

/********************************************************** Type expressions */
/* ty */
type_expr:
      type_tuple TokRArrow type_expr { TypeFun ($1, $3, union_pos (pos_of_type $1) (pos_of_type $3)) }
    | type_tuple		     { $1 }
    ;

type_tuple:
      type_tuple_aux TokStar type_expr_limited	{ TypeTuple ($1 @ [$3], union_pos (pos_of_type (List.hd $1)) (pos_of_type $3)) }
    | type_expr_limited				{ $1 }

type_tuple_aux:
      type_tuple_aux TokStar type_expr_limited	{ $1 @ [$3] }
    | type_expr_limited				{ [$1] }
    ;

/* ty */
type_expr_limited:
      TokMeta TokId						    { let (s, p) = $2 in TypeVar (s, union_pos $1 p) }
    | TokAny							    { TypeAny $1 }
    | vpath							    { let (_, _, p) = $1 in TypeId ([], $1, p) }
    | type_expr_limited vpath					    { let (_, _, p) = $2 in TypeId ([$1], $2, union_pos (pos_of_type $1) p) }
    | TokLParen type_expr TokRParen				    { $2 }
    | TokLParen type_expr_list TokComma type_expr TokRParen vpath   { TypeId ($2 @ [$4], $6, union_pos $1 $5) }
    ;

/* ty list */
type_expr_list:
      type_expr				{ [$1] }
    | type_expr_list TokComma type_expr { $1 @ [$3] }
    ;

/********************************************************** Patterns */
/* pattern */
pattern:
      pattern_no_tuple				{ $1 }
    | pattern_no_tuple TokComma pattern_tuple	%prec prec_pat_tuple	{ TuplePat ($1 :: $3, union_pos (pos_of_pattern $1) (pos_of_pattern (list_last $3))) }
    | pattern TokAlt pattern			%prec prec_pat_alt	{ AltPat ($1, $3, union_pos (pos_of_pattern $1) (pos_of_pattern $3)) }
    | pattern TokAs ident			%prec prec_as		{ let s,p = $3 in AsPat ($1, s, union_pos (pos_of_pattern $1) p) }
    | pattern TokAs ident TokComma pattern_tuple	%prec prec_as
        { let s,p = $3 in
          let ap = AsPat ($1, s, union_pos (pos_of_pattern $1) p) in
            TuplePat (ap :: $5, union_pos p (pos_of_pattern (list_last $5))) }
    | pattern TokAs ident TokCons pattern_no_tuple	%prec prec_as
        { let s,p = $3 in
          let ap = AsPat ($1, s, union_pos (pos_of_pattern $1) p) in
          let p2 = union_pos p (pos_of_pattern $5) in
            ConsPat (([],list_cons_sym,$4), TuplePat ([ap; $5], p2), p2) }
    ;

pattern_no_tuple:
      pattern_limited				{ $1 }
    | vpath_cap pattern_no_tuple		%prec prec_pat_cons	{ let (_,_,p) = $1 in ConsPat ($1, $2, union_pos p (pos_of_pattern $2)) }
    | pattern_no_tuple TokCons pattern_no_tuple	%prec prec_pat_listcons
        { let p = union_pos (pos_of_pattern $1) (pos_of_pattern $3) in
            ConsPat (([],list_cons_sym,$2), TuplePat ([$1; $3], p), p) }
    ;

pattern_limited:
      ident						{ let s,p = $1 in IdPat (s, p) }
    | TokAny						{ AnyPat $1 }
    | const_expr					{ ConstPat ($1, pos_of_const $1) }
    | const_expr TokDotDot const_expr			{ RangePat ($1, $3, union_pos (pos_of_const $1) (pos_of_const $3)) }
    | TokLParen pattern TokRParen			{ $2 }
    | TokLParen pattern TokIs type_expr	TokRParen	{ TySpecPat ($2, $4, union_pos $1 $5) }
    | TokLRecord pattern_record TokRRecord		{ RecordPat ($2, union_pos $1 $3) }
    | TokLList pattern_list TokRList			{ make_list_pat (union_pos $1 $3) $2 }
    | TokString
        { let s,p = $1 in ArrayPat (List.map (fun c -> ConstPat (CharConst (c,p),p)) (string_to_list s), p) }
    ;

/* pattern list */
pattern_tuple:
      pattern_no_tuple				{ [$1] }
    | pattern_no_tuple TokComma pattern_tuple	{ $1 :: $3 }
    ;

/* (vpath * pattern) list */
pattern_record:
    | vpath TokEq pattern			    { [$1, $3] }
    | vpath TokEq pattern TokSemi		    { [$1, $3] }
    | vpath TokEq pattern TokSemi pattern_record    { ($1, $3) :: $5 }
    ;

/* pattern list */
pattern_list:
    | pattern				{ [$1] }
    | pattern TokSemi pattern_list	{ ($1) :: $3 }
    ;

/* (binding = pattern * expr * pos) list */
let_binding_list:
      let_binding			    { [$1] }
    | let_binding TokAnd let_binding_list   { $1 :: $3 }
    ;

/* binding */
let_binding:
      pattern TokEq expr		    { $1, $3, union_pos (pos_of_pattern $1) (pos_of_expr $3) }
    | pattern TokIs type_expr TokEq expr    { let p = pos_of_pattern $1 in (TySpecPat ($1, $3, union_pos p (pos_of_type $3)), $5, union_pos p (pos_of_expr $5)) }
    | ident param_list TokEq expr
        { let (s,p) = $1 in IdPat (s,p), make_function_nest p $2 None $4, union_pos p (pos_of_expr $4) }
    | ident param_list TokIs type_expr TokEq expr
        { let (s,p) = $1 in
          let p2 = union_pos p (pos_of_expr $6) in
            IdPat (s,p), make_function_nest p $2 None (TySpecExpr ($6, $4, p)), p }

    ;

/* pattern list */
param_list:
      pattern_limited			{ [$1] }
    | pattern_limited param_list	{ ($1) :: $2 }
    ;

/* pmatch * pos */
pattern_match_list:
      pattern_match				%prec prec_expr_n19 { let (m, p) = $1 in [m], p }
    | pattern_match TokAlt pattern_match_list	%prec prec_expr_n19 { let (m, p) = $1 and (l, p2) = $3 in m :: l, union_pos p p2 }
    ;

/* (pattern * expr option * expr) * pos */
pattern_match:
      pattern when_opt TokRArrow expr		%prec prec_expr_n19 { ($1, $2, $4), union_pos (pos_of_pattern $1) (pos_of_expr $4) }
    ;

/* expr option */
when_opt:
						{ None }
    | TokWhen expr				{ Some $2 }
    ;

/********************************************************** Expressions */
/* constexpr */
const_expr:
      vpath_cap				{ let (_,_,p) = $1 in CConsConst ($1, p) }
    | const_expr_not_cons		{ $1 }
    ;

const_expr_not_cons:
      TokInt				{ let (v, p) = $1 in IntConst (v, p) }
    | TokFloat				{ let (v, p) = $1 in FloatConst (v, p) }
    | TokChar				{ let (v, p) = $1 in CharConst (v, p) }
    ;

expr:
      expr TokCons expr
	{ let p = union_pos (pos_of_expr $1) (pos_of_expr $3) in
            ConsExpr (([],list_cons_sym,$2), TupleExpr ([$1; $3], p), p) }
    | TokOpMinus      expr	%prec prec_expr_n5  { make_prefix_expr (Symbol.add "~-",$1) $2 }
    | TokOpMinusDot   expr	%prec prec_expr_n5  { make_prefix_expr (Symbol.add "~-.",$1) $2 }
    | expr TokOpInR6  expr			    { make_infix_expr $2 $1 $3 }
    | expr TokOpInL7  expr			    { make_infix_expr $2 $1 $3 }
    | expr TokStar    expr			    { make_infix_expr (Symbol.add "*",$2) $1 $3 }
    | expr TokOpInL8  expr			    { make_infix_expr $2 $1 $3 }
    | expr TokOpMinus expr			    { make_infix_expr (Symbol.add "-",$2) $1 $3 }
    | expr TokOpMinusDot expr			    { make_infix_expr (Symbol.add "-.",$2) $1 $3 }
    | expr TokOpInR10 expr			    { make_infix_expr $2 $1 $3 }
    | expr TokOpInL11 expr			    { make_infix_expr $2 $1 $3 }
    | expr TokEq      expr			    { make_infix_expr (Symbol.add "=",$2) $1 $3 }
    | expr TokOpInN12 expr			    { make_infix_expr $2 $1 $3 }
    | expr TokOpInL13 expr			    { make_infix_expr $2 $1 $3 }
    | expr TokOpInL14 expr			    { make_infix_expr $2 $1 $3 }
    | expr TokOpInR16 expr			    { make_infix_expr $2 $1 $3 }

    | expr TokSSOr expr				    { IfExpr ($1, ConstExpr (CConsConst (([], Naml_parse_misc.true_sym,  $2), $2), $2), Some ($3), union_pos (pos_of_expr $1) (pos_of_expr $3)) }
    | expr TokSSAnd expr			    { IfExpr ($1, $3, Some (ConstExpr (CConsConst (([], Naml_parse_misc.false_sym, $2), $2), $2)), union_pos (pos_of_expr $1) (pos_of_expr $3)) }

    | expr_simp TokDot vpath TokLArrow expr			%prec prec_expr_r16
	{ AssignProjExpr ($1, $3, $5, union_pos (pos_of_expr $1) (pos_of_expr $5)) }
    | expr_simp TokDot TokLParen expr TokRParen TokLArrow expr	%prec prec_expr_r16
	{ AssignArrayEltExpr ($1, $4, $7, union_pos (pos_of_expr $1) (pos_of_expr $7)) }
    | expr_simp TokDot TokLList expr TokRList TokLArrow expr	%prec prec_expr_r16
	{ AssignArrayEltExpr ($1, $4, $7, union_pos (pos_of_expr $1) (pos_of_expr $7)) }

    | TokIf expr TokThen expr TokElse expr	%prec prec_expr_ifelse
	{ IfExpr ($2, $4, Some $6, union_pos $1 (pos_of_expr $6)) }
    | TokIf expr TokThen expr			%prec prec_expr_if
	{ IfExpr ($2, $4, None, union_pos $1 (pos_of_expr $4)) }

    | TokMatch expr TokWith pattern_match_list	%prec prec_expr_n19
	{ let (m, p) = $4 in MatchExpr ($2, m, union_pos $1 p) }
    | TokFunction pattern_match_list		%prec prec_expr_n19
	{ let (m, p) = $2 in
          let s = Symbol.new_symbol_string "x" in
          let e = MatchExpr (VarExpr (([],s,p),p), m, p) in
            FunExpr ([s], e, union_pos $1 p) }
    | TokTry expr TokWith pattern_match_list	%prec prec_expr_n19
	{ let (m, p) = $4 in TryExpr ($2, m, union_pos $1 p) }
    | TokFun param_list when_opt TokRArrow expr	%prec prec_expr_n19
	{ make_function_nest $1 $2 $3 $5 }
    | TokLet let_rec_opt let_binding_list TokIn expr	%prec prec_expr_n19
	{ LetExpr ($2, $3, $5, union_pos $1 (pos_of_expr $5)) }

    | expr TokSemi expr
	{ SeqExpr ($1, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }
    | expr TokComma expr
	{ match $3 with
	      TupleExpr (l, p) ->
		TupleExpr ($1 :: l, union_pos (pos_of_expr $1) p)
	    | _ ->
		TupleExpr ([$1; $3], union_pos (pos_of_expr $1) (pos_of_expr $3)) }

    | expr_fun { $1 }
    | vpath_cap				{ let (_,_,p) = $1 in ConstExpr (CConsConst ($1, p), p) }
    ;

expr_fun:
      expr_fun expr_arg
	{ let p = union_pos (pos_of_expr $1) (pos_of_expr $2) in
          match $1 with
              ApplyExpr (f, al, _) -> ApplyExpr (f, al @ [$2], p)
            | _ -> ApplyExpr ($1, [$2], p) }
    | vpath_cap	expr_arg
	{ let (_,_,p) = $1 in ConsExpr ($1, $2, union_pos p (pos_of_expr $2)) }
    | expr_simp				{ $1 }
    ;

expr_arg:
      expr_simp { $1 }
    | vpath_cap { let (_,_,p) = $1 in ConstExpr (CConsConst ($1, p), p) }
    ;

expr_simp:
      const_expr_not_cons				    %prec prec_expr_const
	{ ConstExpr ($1, pos_of_const $1) }
    | vpath						    	{ let (_,_,p) = $1 in VarExpr ($1, p) }
    | TokOpPreN1 expr_simp
	{ make_prefix_expr $1 $2 }
    | TokLParen expr TokRParen					{ GroupExpr ($2, union_pos $1 $3) }
    | TokBegin expr TokEnd					{ GroupExpr ($2, union_pos $1 $3) }
    | TokLParen expr TokIs type_expr TokRParen			{ TySpecExpr ($2, $4, union_pos $1 $5) }
    | TokLList expr TokRList					{ make_list_expr $2 }
    | TokLArray expr TokRArray					{ make_array_expr (union_pos $1 $3) $2 }
    | TokLArray TokRArray					{ ArrayExpr ([], union_pos $1 $2) }
    | TokLRecord expr_record TokRRecord				{ RecordExpr (None, $2, union_pos $1 $3) }
    | TokLRecord expr_simp TokWith expr_record TokRRecord	{ RecordExpr (Some $2, $4, union_pos $1 $5) }
    | TokString
        { let s,p = $1 in ArrayExpr (List.map (fun c -> ConstExpr (CharConst (c,p),p)) (string_to_list s), p) }

    | TokWhile expr TokDo expr TokDone
	{ WhileExpr ($2, $4, union_pos $1 $5) }
    | TokFor ident TokEq expr TokTo expr TokDo expr TokDone
	{ let (id, _) = $2 in ForExpr (id, $4, true, $6, $8, union_pos $1 $9) }
    | TokFor ident TokEq expr TokDownto expr TokDo expr TokDone
	{ let (id, _) = $2 in ForExpr (id, $4, false, $6, $8, union_pos $1 $9) }

    | expr_simp TokDot vpath
        { let (_,_,p) = $3 in ProjExpr ($1, $3, union_pos (pos_of_expr $1) p) }
    | expr_simp TokDot TokLParen expr TokRParen
        { ArrayEltExpr ($1, $4, union_pos (pos_of_expr $1) $5) }
    | expr_simp TokDot TokLList expr TokRList
        { ArrayEltExpr ($1, $4, union_pos (pos_of_expr $1) $5) }
    ;


/* (vpath * expr) list */
expr_record_aux:
      vpath TokEq expr				    { [$1, $3] }
    | expr_record_aux TokSemi vpath TokEq expr	    { ($3, $5) :: $1 }
    ;

expr_record:
      expr_record_aux		{ $1 }
    | expr_record_aux TokSemi	{ $1 }
    ;
