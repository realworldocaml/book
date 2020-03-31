/*
 * Parser for FC.
 * There should be exactly 2 shift/reduce conflicts,
 * one for if statements, and the other for try statements.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000 Jason Hickey, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * jyh@cs.caltech.edu
 */
%{
open Symbol

open Fc_config
open Fc_parse_type
open Fc_parse_util
open Fc_parse_state
open Fc_parse_exn

(*
 * Struct declarations have three cases:
 * structs, unions, or union enums
 *)
type struct_case =
   StructCase
 | UnionCase

(*
 * Type modifiers.
 *)
type spec =
   AutoSpec
 | RegisterSpec
 | StaticSpec
 | ExternSpec
 | ConstSpec
 | VolatileSpec
 | ShortSpec
 | LongSpec
 | SignedSpec
 | UnsignedSpec

(*
 * Builtin types.
 *)
type builtin =
   VoidType
 | CharType
 | IntType
 | FloatType
 | DoubleType
 | PolyType
 | VarType of symbol
 | ApplyType of symbol * ty list

(*
 * Well-known symbols.
 *)
let comma_sym     = Symbol.add ","
let star_sym      = Symbol.add "*"
let addr_of_sym   = Symbol.add "&"
let sizeof_sym    = Symbol.add "sizeof"
let eq_sym        = Symbol.add "="
let bang_sym      = Symbol.add "!"
let or_sym        = Symbol.add "||"
let and_sym       = Symbol.add "&&"
let subscript_sym = Symbol.add "[]"
let apply_sym     = Symbol.add "()"

let zero_expr pos =
   IntExpr (pos, Rawint.of_int Rawint.Int32 true 0)

let one_expr pos =
   IntExpr (pos, Rawint.of_int Rawint.Int32 true 1)

(*
 * Var name in a declaration.
 *)
type var_name =
   VarNameId      of pos * symbol option
 | VarNamePattern of pos * pattern
 | VarNameApply   of pos * symbol * ty list
 | VarNameArray   of pos * (pos * spec) list * var_name * expr
 | VarNamePointer of pos * (pos * spec) list * var_name
 | VarNameRef     of pos * (pos * spec) list * var_name
 | VarNameFun     of pos * var_name * (pattern option * ty) list

let pos_of_var_name = function
   VarNameId      (pos, _) -> pos
 | VarNamePattern (pos, _) -> pos
 | VarNameApply   (pos, _, _) -> pos
 | VarNameArray   (pos, _, _, _) -> pos
 | VarNamePointer (pos, _, _) -> pos
 | VarNameRef     (pos, _, _) -> pos
 | VarNameFun     (pos, _, _) -> pos

(*
 * Extract the status from the specifiers.
 *)
let status_of_spec spec =
   let collect status (_, spec) =
      match spec with
         ConstSpec -> StatusConst
       | VolatileSpec -> StatusVolatile
       | AutoSpec
       | RegisterSpec
       | StaticSpec
       | ExternSpec
       | ShortSpec
       | LongSpec
       | SignedSpec
       | UnsignedSpec -> status
   in
      List.fold_left collect StatusNormal spec

let storage_class_of_spec spec =
   let collect store (_, spec) =
      match spec with
         AutoSpec -> StoreAuto
       | RegisterSpec -> StoreRegister
       | StaticSpec -> StoreStatic
       | ExternSpec -> StoreExtern
       | ConstSpec
       | VolatileSpec
       | ShortSpec
       | LongSpec
       | SignedSpec
       | UnsignedSpec  -> store
   in
      List.fold_left collect StoreAuto spec

(*
 * Function parameter declarations.
 *)
let rec make_param_decl ty = function
   VarNameId (pos, Some id) ->
      Some (VarPattern (pos, id, id, Some ty)), ty
 | VarNameId (_, None) ->
      None, ty
 | VarNamePattern (_, p) ->
      Some p, ty
 | VarNameApply (pos, _, _) ->
      raise (ParseError (pos, "illegal parameter definition"))
 | VarNameArray (pos, spec, v, e) ->
      make_param_decl (TypeArray (pos, status_of_spec spec, ty, zero_expr pos, e)) v
 | VarNamePointer (pos, spec, v) ->
      make_param_decl (TypePointer (pos, status_of_spec spec, ty)) v
 | VarNameRef (pos, spec, v) ->
      make_param_decl (TypeRef (pos, status_of_spec spec, ty)) v
 | VarNameFun (pos, v, args) ->
      make_param_decl (TypeFun (pos, StatusNormal, make_param_decls args, ty)) v

and make_param_decls args =
   List.map snd args

let make_param_decl ty p =
   match p with
      VarNameId (pos, Some id) ->
         Some (VarPattern (pos, id, id, Some ty)), ty
    | _ ->
         make_param_decl ty p

(*
 * Build a variable declaration from the syntax.
 *)
let make_var_init_decls store ty defs =
   (* Build the declaration with an initializer *)
   let rec make_def ty e = function
      VarNameId (pos, Some n) ->
         pos, store, n, ty, e
    | VarNamePattern (pos, _)
    | VarNameId (pos, None)
    | VarNameApply (pos, _, _) ->
         raise (ParseError (pos, "illegal variable declaration"))
    | VarNameArray (pos, spec, v, e') ->
         make_def (TypeArray (pos, status_of_spec spec, ty, zero_expr pos, e')) e v
    | VarNamePointer (pos, spec, v) ->
         make_def (TypePointer (pos, status_of_spec spec, ty)) e v
    | VarNameRef (pos, spec, v) ->
         make_def (TypeRef (pos, status_of_spec spec, ty)) e v
    | VarNameFun (pos, v, args) ->
         make_def (TypeFun (pos, StatusNormal, make_param_decls args, ty)) e v
   in

   (* Initial type *)
   let make_init_def (v, e) =
      make_def ty e v
   in
      List.map make_init_def defs

let make_patt_init_decls store ty defs =
   (* Build the declaration with an initializer *)
   let rec make_def ty e = function
      VarNameId (pos, Some n) ->
         pos, store, VarPattern (pos, n, n, Some ty), ty, e
    | VarNamePattern (pos, p) ->
         pos, store, p, ty, e
    | VarNameId (pos, None)
    | VarNameApply (pos, _, _) ->
         raise (ParseError (pos, "illegal variable declaration"))
    | VarNameArray (pos, spec, v, e') ->
         make_def (TypeArray (pos, status_of_spec spec, ty, zero_expr pos, e')) e v
    | VarNamePointer (pos, spec, v) ->
         make_def (TypePointer (pos, status_of_spec spec, ty)) e v
    | VarNameRef (pos, spec, v) ->
         make_def (TypeRef (pos, status_of_spec spec, ty)) e v
    | VarNameFun (pos, v, args) ->
         make_def (TypeFun (pos, StatusNormal, make_param_decls args, ty)) e v
   in

   (* Initial type *)
   let make_init_def (v, e) =
      make_def ty e v
   in
      List.map make_init_def defs

(*
 * Build a variable declaration from the syntax.
 *)
let rec make_var_decl ty = function
   VarNameId (pos, Some n) ->
      pos, n, ty
 | VarNamePattern (pos, _)
 | VarNameId (pos, None)
 | VarNameApply (pos, _, _) ->
      raise (ParseError (pos, "illegal variable declaration"))
 | VarNameArray (pos, spec, v, e) ->
      make_var_decl (TypeArray (pos, status_of_spec spec, ty, zero_expr pos, e)) v
 | VarNamePointer (pos, spec, v) ->
      make_var_decl (TypePointer (pos, status_of_spec spec, ty)) v
 | VarNameRef (pos, spec, v) ->
      make_var_decl (TypeRef (pos, status_of_spec spec, ty)) v
 | VarNameFun (pos, v, args) ->
      make_var_decl (TypeFun (pos, StatusNormal, make_param_decls args, ty)) v

let make_var_decls ty decls =
   List.map (make_var_decl ty) decls

(*
 * Struct decls have an optional width.
 *)
let make_struct_decls (store, ty) decls =
   List.map (fun (decl, width) ->
      let pos, n, ty = make_var_decl ty decl in
         pos, n, ty, width) decls

let make_uenum_ty_decls (store, ty) decls =
   List.map (fun decl ->
      let pos, n, ty = make_var_decl ty decl in
         pos, n, Some ty) decls

(*
 * Basic enumeration declaration.
 *)
let make_enum_decl v e_opt pos =
    pos, v, e_opt

(*
 * Add some type definitions.
 *)
let make_type_lambda pos tyl ty =
   let vars =
      List.map (function
         TypeVar (_, _, v) -> v
       | _ -> raise (ParseError (pos, "not a type variable"))) tyl
   in
      TypeLambda (pos, StatusNormal, vars, ty)

let rec make_type_decl ty = function
   VarNameId (pos, Some n) ->
      pos, n, ty, []
 | VarNameId (pos, None)
 | VarNamePattern (pos, _) ->
      raise (ParseError (pos, "illegal type declaration"))
 | VarNameApply (pos, n, tyl) ->
      pos, n, ty, tyl
 | VarNameArray (pos, spec, v, e) ->
      make_type_decl (TypeArray (pos, status_of_spec spec, ty, zero_expr pos, e)) v
 | VarNamePointer (pos, spec, v) ->
      make_type_decl (TypePointer (pos, status_of_spec spec, ty)) v
 | VarNameRef (pos, spec, v) ->
      make_type_decl (TypeRef (pos, status_of_spec spec, ty)) v
 | VarNameFun (pos, v, args) ->
      make_type_decl (TypeFun (pos, StatusNormal, make_param_decls args, ty)) v

let make_type_decl ty decl =
   let pos, n, ty, tyl = make_type_decl ty decl in
   let ty = make_type_lambda pos tyl ty in
      pos, n, n, ty

let make_type_decls ty decls =
   List.map (make_type_decl ty) decls

(*
 * A function definition.
 *)
let get_fun_var (p, ty) =
   let pos = pos_of_type ty in
   let p =
      match p with
         Some p ->
            p
       | None ->
            let v = new_symbol_string "unnamed_parameter" in
               VarPattern (pos, v, v, Some ty)
   in
      pos, p, ty

let rec make_fun_def (store, ty) decl body =
   let pos = union_pos (pos_of_type ty) (pos_of_expr body) in
      match decl with
         VarNameFun (_, res, vars) ->
            let vars = List.map get_fun_var vars in
            let _, f, ty = make_var_decl ty res in
               FunDef (pos, store, f, f, vars, ty, body)
       | VarNameArray (pos', spec, decl, e) ->
            make_fun_def (store, TypeArray (pos', status_of_spec spec, ty, zero_expr pos, e)) decl body
       | VarNamePointer (pos', spec, decl) ->
            make_fun_def (store, TypePointer (pos', status_of_spec spec, ty)) decl body
       | VarNameRef (pos', spec, decl) ->
            make_fun_def (store, TypeRef (pos', status_of_spec spec, ty)) decl body
       | VarNameId _
       | VarNamePattern _
       | VarNameApply _ ->
            raise (ParseError (pos, "not a function"))

(*
 * Operators.
 *)
let make_op pos op_class v el =
   OpExpr (pos, op_class, v, v, el)

let make_unop op_class (pos, v) expr =
   make_op (union_pos pos (pos_of_expr expr)) op_class v [expr]

let make_binop (pos, v) expr1 expr2 =
   let pos = union_pos pos (pos_of_expr expr1) in
   let pos = union_pos pos (pos_of_expr expr2) in
      make_op pos PreOp v [expr1; expr2]

let make_project (pos1, v) e (pos2, label) =
   let pos = union_pos (pos_of_expr e) pos2 in
      ProjectExpr (pos, OpExpr (pos, PreOp, addr_of_sym, addr_of_sym, [e]), label)

(*
 * Boolean expressions.
 *)
let make_and_op expr1 expr2 =
   let pos = union_pos (pos_of_expr expr1) (pos_of_expr expr2) in
   let v' = new_symbol_string "and" in
   let p = VarPattern (pos, v', v', None) in
   let step1 = VarDefs (pos, [pos, StoreAuto, p, TypePoly (pos, StatusNormal), InitExpr (pos, expr1)]) in
   let test_expr = make_op pos PreOp bang_sym [VarExpr (pos, v', v')] in
   let true_expr = make_op pos PreOp and_sym [VarExpr (pos, v', v'); expr2] in
   let step2 = IfExpr (pos, test_expr, zero_expr pos, Some true_expr) in
      SeqExpr (pos, [step1; step2])

let make_or_op expr1 expr2 =
   let pos = union_pos (pos_of_expr expr1) (pos_of_expr expr2) in
   let v' = new_symbol_string "or" in
   let p = VarPattern (pos, v', v', None) in
   let step1 = VarDefs (pos, [pos, StoreAuto, p, TypePoly (pos, StatusNormal), InitExpr (pos, expr1)]) in
   let test_expr = make_op pos PreOp bang_sym [VarExpr (pos, v', v')] in
   let true_expr = make_op pos PreOp or_sym [VarExpr (pos, v', v'); expr2] in
   let step2 = IfExpr (pos, test_expr, true_expr, Some (one_expr pos)) in
      SeqExpr (pos, [step1; step2])

(*
 * Optional expression.
 *)
let make_opt_expr opt_expr def_expr =
   match opt_expr with
      Some expr -> expr
    | None -> def_expr

(*
 * Types.
 *)
let make_ty_lambda pos vars ty =
   let vars = List.map (fun (_, v) -> v) vars in
      TypeLambda (pos, StatusNormal, vars, ty)

let make_struct_or_union struct_flag fields pos =
   match struct_flag with
      StructCase ->
         TypeStruct (pos, StatusNormal, Fields fields)
    | UnionCase ->
         TypeUnion (pos, StatusNormal, Fields fields)

let make_enum fields pos =
   TypeEnum (pos, StatusConst, Fields fields)

let make_uenum label fields pos =
   TypeUEnum (pos, StatusConst, Fields (label, fields))

let make_tuple fields pos =
   TypeProduct (pos, StatusNormal, fields)

(*
 * Parse the type specification.
 *)
let parse_builtin_type spec pos =
   let pos, store, status, pre, signed =
      List.fold_left (fun (pos, store, status, pre, signed) (pos', spec) ->
         let pos =
            match pos with
               Some pos -> Some (union_pos pos pos')
             | None -> Some pos'
         in
            match spec with
               AutoSpec ->
                  pos, StoreAuto, status, pre, signed
             | RegisterSpec ->
                  pos, StoreRegister, status, pre, signed
             | StaticSpec ->
                  pos, StoreStatic, status, pre, signed
             | ExternSpec ->
                  pos, StoreExtern, status, pre, signed
             | ConstSpec ->
                  pos, store, StatusConst, pre, signed
             | VolatileSpec ->
                  pos, store, StatusVolatile, pre, signed
             | ShortSpec ->
                  pos, store, status, ShortPrecision, signed
             | LongSpec ->
                  pos, store, status, LongPrecision, signed
             | SignedSpec ->
                  pos, store, status, pre, Some true
             | UnsignedSpec ->
                  pos, store, status, pre, Some false) (pos, StoreAuto, StatusNormal, NormalPrecision, None) spec
   in
   let pos =
      match pos with
         Some pos -> pos
       | None -> current_position ()
   in
      pos, store, status, pre, signed

(*
 * Construct a builtin integer type.
 *)
let make_int_type spec =
   let pos, store, status, pre, signed = parse_builtin_type spec None in
   let pre = FCParam.int_precision pre in
   let signed =
      match signed with
         Some signed -> signed
       | None -> true
   in
      store, TypeInt (pos, status, pre, signed)

(*
 * Construct a polymorphic type.
 *)
let make_poly_type spec =
   let pos, store, status, pre, signed = parse_builtin_type spec None in
      store, TypePoly (pos, status)

(*
 * Construct a generic builtin type.
 *)
let rec make_defined_type spec (pos, ty) =
   let pos, store, status, pre, signed = parse_builtin_type spec (Some pos) in
   let ty =
      match ty with
         VoidType ->
          TypeUnit (pos, status, 1)
       | CharType ->
          let pre = FCParam.char_precision pre in
          let signed =
             match signed with
                Some signed -> signed
              | None -> false
          in
             TypeChar (pos, status, pre, signed)
       | IntType ->
          let pre = FCParam.int_precision pre in
          let signed =
             match signed with
                Some signed -> signed
              | None -> true
          in
             TypeInt (pos, status, pre, signed)
       | FloatType ->
            let pre =
               match pre with
                  ShortPrecision
                | NormalPrecision ->
                   ShortPrecision
                | LongPrecision ->
                   NormalPrecision
            in
            let pre = FCParam.float_precision pre in
               TypeFloat (pos, status, pre)
       | DoubleType ->
            let pre = FCParam.float_precision pre in
               TypeFloat (pos, status, pre)
       | VarType v ->
            TypeVar (pos, status, v)
       | ApplyType (v, args) ->
            TypeApply (pos, status, v, args)
       | PolyType ->
            TypePoly (pos, status)
   in
      store, ty

(*
 * Collect the switch cases.
 * This doesn't do any translation, it just does
 * the search.  Note, we have to be a little careful
 * because labels may be interleaved with cases.
 *)
let rec normalize_cases pos e =
   match e with
      SeqExpr (pos, el) ->
         collect_cases pos el
    | _ ->
         raise (ParseError (pos, "switch body is not a compound block"))

and collect_cases pos el =
   if el = [] then
      []
   else
      let case, el = collect_case pos el in
      let cases = collect_cases pos el in
         case :: cases

and collect_case pos el =
   match el with
      CaseExpr (_, p) :: el ->
         let el1, el2 = collect_case_list el in
            (p, el1), el2
    | DefaultExpr pos' :: el ->
         let v = new_symbol_string "_" in
         let p = VarPattern (pos', v, v, None) in
         let el1, el2 = collect_case_list el in
            (p, el1), el2
    | LabelExpr _ as e :: el ->
         let case, el = collect_case pos el in
         let p, el' = case in
         let case = p, e :: el' in
            case, el
    | _ ->
         raise (ParseError (pos, "switch body does not start with a case statement"))

and collect_case_list el =
   match el with
      CaseExpr _ :: _
    | DefaultExpr _ :: _ ->
         [], el
    | e :: el ->
         let el1, el2 = collect_case_list el in
            e :: el1, el2
    | [] ->
         [], []

(*
 * Add unit to the stmt.
 *)
let wrap_stmt e =
   let pos = pos_of_expr e in
      SeqExpr (pos, [e; UnitExpr(pos, 1, 0)])
%}

/*
 * End-of-file is a token.
 */
%token TokEof

/*
 * Binary operators return position.
 */
%token <Fc_parse_type.pos> TokLeftParen
%token <Fc_parse_type.pos> TokRightParen
%token <Fc_parse_type.pos> TokLeftBrack
%token <Fc_parse_type.pos> TokRightBrack
%token <Fc_parse_type.pos> TokLeftBrace
%token <Fc_parse_type.pos> TokRightBrace
%token <Fc_parse_type.pos> TokElide

%token <Fc_parse_type.pos> TokSemi
%token <Fc_parse_type.pos> TokComma
%token <Fc_parse_type.pos> TokQuest
%token <Fc_parse_type.pos> TokColon
%token <Fc_parse_type.pos> TokStar
%token <Fc_parse_type.pos> TokEq
%token <Fc_parse_type.pos> TokAnd
%token <Fc_parse_type.pos> TokOr

%token <Fc_parse_type.pos * Symbol.symbol> TokOp1
%token <Fc_parse_type.pos * Symbol.symbol> TokOp4
%token <Fc_parse_type.pos * Symbol.symbol> TokOp5
%token <Fc_parse_type.pos * Symbol.symbol> TokAddr
%token <Fc_parse_type.pos * Symbol.symbol> TokOp7
%token <Fc_parse_type.pos * Symbol.symbol> TokOp8
%token <Fc_parse_type.pos * Symbol.symbol> TokOp9
%token <Fc_parse_type.pos * Symbol.symbol> TokOp10
%token <Fc_parse_type.pos * Symbol.symbol> TokOp11
%token <Fc_parse_type.pos * Symbol.symbol> TokPreOp12
%token <Fc_parse_type.pos * Symbol.symbol> TokPrePostOp12
%token <Fc_parse_type.pos * Symbol.symbol> TokOp13
%token <Fc_parse_type.pos * Symbol.symbol> TokDot

/*
 * Keywords.
 */
%token <Fc_parse_type.pos> TokAuto
%token <Fc_parse_type.pos> TokBreak
%token <Fc_parse_type.pos> TokCase
%token <Fc_parse_type.pos> TokTypeChar
%token <Fc_parse_type.pos> TokConst
%token <Fc_parse_type.pos> TokContinue
%token <Fc_parse_type.pos> TokDefault
%token <Fc_parse_type.pos> TokDo
%token <Fc_parse_type.pos> TokTypeDouble
%token <Fc_parse_type.pos> TokElse
%token <Fc_parse_type.pos> TokEnum
%token <Fc_parse_type.pos> TokExtern
%token <Fc_parse_type.pos> TokTypeFloat
%token <Fc_parse_type.pos> TokFor
%token <Fc_parse_type.pos> TokGoto
%token <Fc_parse_type.pos> TokIf
%token <Fc_parse_type.pos> TokTypeInt
%token <Fc_parse_type.pos> TokLong
%token <Fc_parse_type.pos> TokRegister
%token <Fc_parse_type.pos> TokReturn
%token <Fc_parse_type.pos> TokRaise
%token <Fc_parse_type.pos> TokShort
%token <Fc_parse_type.pos> TokSigned
%token <Fc_parse_type.pos> TokSizeof
%token <Fc_parse_type.pos> TokStatic
%token <Fc_parse_type.pos> TokStruct
%token <Fc_parse_type.pos> TokTuple
%token <Fc_parse_type.pos> TokSwitch
%token <Fc_parse_type.pos> TokTypedef
%token <Fc_parse_type.pos> TokUnion
%token <Fc_parse_type.pos> TokUnsigned
%token <Fc_parse_type.pos> TokTypeVoid
%token <Fc_parse_type.pos> TokVolatile
%token <Fc_parse_type.pos> TokWhile

%token <Fc_parse_type.pos> TokPoly
%token <Fc_parse_type.pos> TokTry
%token <Fc_parse_type.pos> TokCatch
%token <Fc_parse_type.pos> TokFinally
%token <Fc_parse_type.pos> TokOperator


/*
 * Terminal tokens.
 */
%token <Fc_parse_type.pos * Symbol.symbol> TokId
%token <Fc_parse_type.pos * Symbol.symbol> TokTypeId
%token <Fc_parse_type.pos * Symbol.symbol> TokTypeVar
%token <Fc_parse_type.pos * Fc_config.precision * int array> TokString
%token <Fc_parse_type.pos * Rawint.rawint> TokChar
%token <Fc_parse_type.pos * Rawint.rawint> TokInt
%token <Fc_parse_type.pos * Rawfloat.rawfloat> TokFloat

/*
 * Precedences.
 */
%left TokComma
%right TokOp1 TokEq
%right TokQuest TokColon
%left TokOp2
%left TokAnd TokOr
%left TokOp4
%left TokOp5
%left TokAddr
%left TokOp7
%left TokOp8
%left TokOp9
%left TokOp10
%left TokStar TokOp11
%right prec_unary prec_cast TokPreOp12 TokPrePostOp12
%left prec_apply prec_subscript TokOp13 TokDot TokLeftParen TokRightParen TokLeftBrack TokRightBrack

/* Annihilating the if/then shift reduce conflict :) */
%nonassoc   prec_ifthen
%nonassoc   TokElse prec_ifthenelse
%left       TokCatch TokFinally
%nonassoc   TokTry

/*
 * A complete program.
 */
%start prog
%type <Fc_parse_type.expr list> prog

%%

/************************************************************************
 * TOPLEVEL PRODUCTIONS
 ************************************************************************/

/*
 * A program is a sequence of str_items.
 */
prog:     all_defs TokEof
          { let defs = pop_tenv () in
            let name = current_file () in
            let pos = name, 1, 0, 1, 0 in
               match defs with
                  [] -> $1
                | _ -> TypeDefs (pos, List.rev defs) :: $1
          }
        ;

/************************************************************************
 * DECLARATIONS AND DEFINITIONS
 ************************************************************************/

/*
 * Definitions.
 */
all_defs:
          rev_all_defs
          { List.rev $1 }
        ;

rev_all_defs:
          /* empty */
          { [] }
        | rev_all_defs all_def
          { $2 :: $1 }
        ;

all_def:  var_defs
          { $1 }
        | fun_def
          { $1 }
        | type_defs
          { $1 }
        ;

/************************************************************************
 * TYPE SPECIFIERS
 ************************************************************************/

/*
 * Type parameter lists.
 */
opt_type_params:
          /* empty */
          { [] }
        | type_params
          { $1 }
        ;

type_params:
          TokTypeVar
          { [$1] }
        | TokLeftParen type_param_list TokRightParen
          { List.rev $2 }
        ;

type_param_list:
          TokTypeVar
          { [$1] }
        | type_param_list TokComma TokTypeVar
          { $3 :: $1 }
        ;

/*
 * General declaration specifier.
 */
decl_spec:
          simp_decl_spec
          { $1 }
        | struct_decl_spec
          { $1 }
        ;

simp_decl_spec:
          decl_specifiers
          { make_int_type $1 }
        | decl_specifiers_opt type_builtin decl_specifiers_opt
          { make_defined_type ($1 @ $3) $2 }
        | simp_type_spec decl_type_specifiers_opt
          { make_defined_type $2 $1 }
        | decl_type_specifiers simp_type_spec decl_type_specifiers_opt
          { make_defined_type ($1 @ $3) $2 }
        ;

struct_decl_spec:
          struct_type_spec decl_type_specifiers_opt
          { make_defined_type $2 $1 }
        | decl_type_specifiers struct_type_spec decl_type_specifiers_opt
          { make_defined_type ($1 @ $3) $2 }
        ;

/*
 * Modifiers for builtin types.
 */
decl_specifiers_opt:
          /* empty */
          { [] }
        | decl_specifiers
          { $1 }
        | decl_type_specifiers
          { $1 }
        ;

decl_specifiers:
          type_mod
          { [$1] }
        | decl_type_specifiers type_mod
          { $2 :: $1 }
        | decl_specifiers decl_specifier
          { $2 :: $1 }
        ;

decl_specifier:
          storage_class_spec
          { $1 }
        | type_qual
          { $1 }
        | type_mod
          { $1 }
        ;

/*
 * Modifiers for defined types.
 */
decl_type_specifiers_opt:
          /* empty */
          { [] }
        | decl_type_specifiers
          { $1 }
        ;

decl_type_specifiers:
          decl_type_specifier
          { [$1] }
        | decl_type_specifiers decl_type_specifier
          { $2 :: $1 }
        ;

decl_type_specifier:
          storage_class_spec
          { $1 }
        | type_qual
          { $1 }
        ;

/*
 * Modifiers for defined types.
 */
decl_struct_type_specifiers_opt:
          /* empty */
          { [] }
        | decl_struct_type_specifiers
          { $1 }
        ;

decl_struct_type_specifiers:
          decl_struct_type_specifier
          { [$1] }
        | decl_struct_type_specifiers decl_struct_type_specifier
          { $2 :: $1 }
        ;

decl_struct_type_specifier:
          type_qual
          { $1 }
        ;

/*
 * Storage class specification.
 */
storage_class_spec:
          TokAuto
          { $1, AutoSpec }
        | TokRegister
          { $1, RegisterSpec }
        | TokStatic
          { $1, StaticSpec }
        | TokExtern
          { $1, ExternSpec }
        ;

/*
 * Type qualifier.
 */
type_qual:
          TokConst
          { $1, ConstSpec }
        | TokVolatile
          { $1, VolatileSpec }
        ;

/*
 * Type modifiers.
 */
type_mod:
          TokShort
          { $1, ShortSpec }
        | TokLong
          { $1, LongSpec }
        | TokSigned
          { $1, SignedSpec }
        | TokUnsigned
          { $1, UnsignedSpec }
        ;

/*
 * Builtin types.
 */
type_builtin:
          TokTypeVoid
          { $1, VoidType }
        | TokTypeChar
          { $1, CharType }
        | TokTypeInt
          { $1, IntType }
        | TokTypeFloat
          { $1, FloatType }
        | TokTypeDouble
          { $1, DoubleType }
        ;

/*
 * Monmorphic type specification.
 */
simp_type_spec:
          TokTypeVar
          { let pos, id = $1 in
               pos, VarType id
          }
        | TokTypeId
          { let pos, id = $1 in
               pos, ApplyType (id, [])
          }
        | TokPoly
          { let pos = $1 in
               pos, PolyType
          }
        | TokPoly TokTypeId TokLeftParen opt_param_list TokRightParen
          { let _, id = $2 in
            let pos = union_pos $1 $5 in
            let args = List.map snd $4 in
               pos, ApplyType (id, args)
          }
        ;

struct_type_spec:
          struct_spec
          { let pos, id = $1 in
               pos, ApplyType (id, [])
          }
        | enum_spec
          { let pos, id = $1 in
               pos, ApplyType (id, [])
          }
        | uenum_spec
          { let pos, id = $1 in
               pos, ApplyType (id, [])
          }
        | tuple_spec
          { let pos, id = $1 in
               pos, ApplyType (id, [])
          }
        ;

/*
 * Tuple specification.
 */
tuple_spec:
          TokTuple opt_type_params tuple_declaration_list
          { let pos2, fields = $3 in
            let pos = union_pos $1 pos2 in
            let id = new_symbol_string "tuple" in
            let ty = make_ty_lambda pos $2 (make_tuple fields pos) in
               Fc_parse_state.add_type id pos ty;
               pos, id
          }
        | tuple_spec_id tuple_declaration_list
          { let pos1, args, id = $1 in
            let pos2, fields = $2 in
            let pos = union_pos pos1 pos2 in
            let ty = make_ty_lambda pos args (make_tuple fields pos) in
               Fc_parse_state.add_type id pos ty;
               pos, id
          }
        | tuple_spec_id
          { let pos, _, id = $1 in
               pos, id
          }
        ;

tuple_spec_id:
          TokTuple opt_type_params id
          { let pos2, id = $3 in
            let pos = union_pos $1 pos2 in
               pos, $2, id
          }
        ;

/*
 * Enumeration specification.
 */
enum_spec:
          TokEnum opt_type_params enum_declaration_list
          { let pos2, fields = $3 in
            let pos = union_pos $1 pos2 in
            let id = new_symbol_string "enum" in
            let ty = make_ty_lambda pos $2 (make_enum fields pos) in
               Fc_parse_state.add_type id pos ty;
               pos, id
          }
        | enum_spec_id enum_declaration_list
          { let pos1, args, id = $1 in
            let pos2, fields = $2 in
            let pos = union_pos pos1 pos2 in
            let ty = make_ty_lambda pos args (make_enum fields pos) in
               Fc_parse_state.add_type id pos ty;
               pos, id
          }
        | enum_spec_id
          { let pos, _, id = $1 in
               pos, id
          }
        ;

enum_spec_id:
          TokEnum opt_type_params id
          { let pos2, id = $3 in
            let pos = union_pos $1 pos2 in
               pos, $2, id
          }
        ;

/*
 * Monomorphic struct specification.
 */
uenum_spec:
          union_enum opt_type_params uenum_declaration_list
          { let pos1 = $1 in
            let pos2, fields = $3 in
            let pos = union_pos pos1 pos2 in
            let id = new_symbol_string "struct" in
            let ty = make_ty_lambda pos $2 (make_uenum id fields pos) in
               Fc_parse_state.add_type id pos ty;
               pos, id
          }
        | uenum_spec_id uenum_declaration_list
          { let pos1, vars, id = $1 in
            let pos2, fields = $2 in
            let pos = union_pos pos1 pos2 in
            let ty = make_ty_lambda pos vars (make_uenum id fields pos) in
            let name = String.lowercase (Symbol.to_string id) in
	       Fc_parse_state.add_type id pos ty;
               pos, id
          }
        | uenum_spec_id
          { let pos, vars, id = $1 in
               if vars <> [] then
                  raise (ParseError (pos, "illegal union enum parameters"));
               pos, id
          }
        ;

union_enum:
          TokEnum TokUnion
          { union_pos $1 $2 }
        | TokUnion TokEnum
          { union_pos $1 $2 }
        ;

uenum_spec_id:
          union_enum opt_type_params id
          { let pos1 = $1 in
            let pos2, id = $3 in
            let pos = union_pos pos1 pos2 in
               Fc_parse_state.add_type id pos (TypeUEnum (pos, StatusNormal, Label (new_symbol id, id)));
               pos, $2, id
          }
        ;

/*
 * Monomorphic struct specification.
 */
struct_spec:
          struct_or_union opt_type_params struct_declaration_list
          { let pos1, struct_flag = $1 in
            let pos2, fields = $3 in
            let pos = union_pos pos1 pos2 in
            let id = new_symbol_string "struct" in
            let ty = make_ty_lambda pos $2 (make_struct_or_union struct_flag fields pos) in
               Fc_parse_state.add_type id pos ty;
               pos, id
          }
        | struct_spec_id struct_declaration_list
          { let pos1, struct_flag, vars, id = $1 in
            let pos2, fields = $2 in
            let pos = union_pos pos1 pos2 in
            let ty = make_ty_lambda pos vars (make_struct_or_union struct_flag fields pos) in
               Fc_parse_state.add_type id pos ty;
               pos, id
          }
        | struct_spec_id
          { let pos, struct_flag, vars, id = $1 in
               if vars <> [] then
                  raise (ParseError (pos, "illegal struct parameters"));
               pos, id
          }
        ;

struct_or_union:
          TokStruct
          { $1, StructCase }
        | TokUnion
          { $1, UnionCase }
        ;

struct_spec_id:
          struct_or_union opt_type_params id
          { let pos1, struct_flag = $1 in
            let pos2, id = $3 in
            let pos = union_pos pos1 pos2 in
            let ty =
               match struct_flag with
                  StructCase -> TypeStruct (pos, StatusNormal, Label (new_symbol id, id))
                | UnionCase -> TypeUnion (pos, StatusNormal, Label (new_symbol id, id))
            in
               Fc_parse_state.add_type id pos ty;
               pos, struct_flag, $2, id
          }
        ;

/************************************************************************
 * DECLARATORS
 ************************************************************************/

/*
 * Tuple field declarations.
 */
tuple_declaration_list:
          TokLeftBrace rev_tuple_decl_list TokRightBrace
          { let pos = union_pos $1 $3 in
            let fields = List.rev $2 in
               pos, fields
          }
        ;

rev_tuple_decl_list:
          tuple_decl
          { [$1] }
        | rev_tuple_decl_list tuple_decl
          { $2 :: $1 }
        ;

tuple_decl:
          simp_decl_spec TokSemi
          { snd $1 }
        | struct_decl_spec TokSemi
          { snd $1 }
        ;

/*
 * Enumeration field declarations.
 */
enum_declaration_list:
          TokLeftBrace TokRightBrace
          { union_pos $1 $2, [] }
        | TokLeftBrace rev_enum_decl_list opt_comma TokRightBrace
          { union_pos $1 $4, List.rev $2 }
        ;

rev_enum_decl_list:
          enum_decl
          { [$1] }
        | rev_enum_decl_list TokComma enum_decl
          { $3 :: $1 }
        ;

enum_decl:
          TokId
          { let pos, id = $1 in
               make_enum_decl id None pos
          }
        | TokId TokEq nc_expr
          { let pos, id = $1 in
            let pos = union_pos pos (pos_of_expr $3) in
               make_enum_decl id (Some $3) pos
          }
        ;

/*
 * Union enum field declarations.
 */
uenum_declaration_list:
          TokLeftBrace uenum_declaration_list_cat TokRightBrace
          { let pos = union_pos $1 $3 in
               pos, $2
          }
        ;

uenum_declaration_list_cat:
          /* empty */
          { [] }
        | uenum_declaration_list_cat uenum_declaration
          { $1 @ $2 }
        ;

uenum_declaration:
          simp_decl_spec uenum_decl_list TokSemi
          { make_uenum_ty_decls $1 $2 }
        | struct_decl_spec uenum_decl_list TokSemi
          { make_uenum_ty_decls $1 $2 }
        | TokId TokSemi
          { let pos, id = $1 in
               [pos, id, None]
          }
        ;

uenum_decl_list:
          rev_uenum_decl_list
          { List.rev $1 }
        ;

rev_uenum_decl_list:
          id_decl
          { [$1] }
        | rev_uenum_decl_list TokComma id_decl
          { $3 :: $1 }
        ;

/*
 * Structure field declarations.
 */
struct_declaration_list:
          TokLeftBrace struct_declaration_list_cat TokRightBrace
          { let pos = union_pos $1 $3 in
               pos, $2
          }
        ;

struct_declaration_list_cat:
          /* empty */
          { [] }
        | struct_declaration_list_cat struct_declaration
          { $1 @ $2  }
        ;

struct_declaration:
          simp_decl_spec struct_decl_list TokSemi
          { make_struct_decls $1 $2 }
        | struct_decl_spec struct_decl_list TokSemi
          { make_struct_decls $1 $2 }
        ;

struct_decl_list:
          rev_struct_decl_list
          { List.rev $1 }
        ;

rev_struct_decl_list:
          struct_decl
          { [$1] }
        | rev_struct_decl_list TokComma struct_decl
          { $3 :: $1 }
        ;

struct_decl:
          id_decl
          { $1, None }
        | TokColon nc_expr
          { let id = new_symbol_string "pad" in
               VarNameId ($1, Some id), Some $2
          }
        | id_decl TokColon nc_expr
          { $1, Some $3 }
        ;

/*
 * Variable definitions.
 */
var_defs:
          struct_decl_spec id_init_decl_list TokSemi
          { let store, ty = $1 in
            let pos = union_pos (pos_of_type ty) $3 in
               VarDefs (pos, make_patt_init_decls store ty $2)
          }
        | simp_decl_spec patt_init_decl_list TokSemi
          { let store, ty = $1 in
            let pos = union_pos (pos_of_type ty) $3 in
               VarDefs (pos, make_patt_init_decls store ty $2)
          }
        ;

/*
 * Declarator with an optional initializer.
 */
id_init_decl_list:
          rev_id_init_decl_list
          { List.rev $1 }
        ;

rev_id_init_decl_list:
          id_init_decl
          { [$1] }
        | rev_id_init_decl_list TokComma id_init_decl
          { $3 :: $1 }
        ;

id_init_decl:
          id_decl
          { $1, InitNone }
        | id_decl TokEq init
          { $1, $3 }
        ;

/*
 * Pattern declarator with an optional initializer.
 */
patt_init_decl_list:
          rev_patt_init_decl_list
          { List.rev $1 }
        ;

rev_patt_init_decl_list:
          patt_init_decl
          { [$1] }
        | rev_patt_init_decl_list TokComma patt_init_decl
          { $3 :: $1 }
        ;

patt_init_decl:
          patt_decl
          { $1, InitNone }
        | patt_decl TokEq init
          { $1, $3 }
        ;

/*
 * Type declarators may be quantified.
 */
type_decl_list:
          rev_type_decl_list
          { List.rev $1 }
        ;

rev_type_decl_list:
          type_decl
          { [$1] }
        | rev_type_decl_list TokComma type_decl
          { $3 :: $1 }
        ;

type_decl:
          type_direct_decl
          { $1 }
        | TokStar decl_struct_type_specifiers_opt type_decl
          { let pos = union_pos $1 (pos_of_var_name $3) in
               VarNamePointer (pos, $2, $3)
          }
        | TokAddr decl_struct_type_specifiers_opt type_decl
          { let pos, _ = $1 in
            let pos = union_pos pos (pos_of_var_name $3) in
               VarNameRef (pos, $2, $3)
          }
        ;

type_direct_decl:
          TokId
          { let pos, id = $1 in
               VarNameId (pos, Some id)
          }
        | TokPoly TokTypeId TokLeftParen param_list TokRightParen
          { let pos, id = $2 in
            let pos = union_pos $1 $5 in
            let args = List.map snd $4 in
               VarNameApply (pos, id, args)
          }
        | TokLeftParen type_decl TokRightParen
          { $2 }
        | type_direct_decl TokLeftBrack opt_expr TokRightBrack %prec prec_subscript
          { let pos = union_pos (pos_of_var_name $1) $4 in
               match $3 with
                  Some e -> VarNameArray (pos, [], $1, e)
                | None -> VarNamePointer (pos, [], $1)
          }
        | type_direct_decl TokLeftParen opt_param_list TokRightParen %prec prec_apply
          { let pos = union_pos (pos_of_var_name $1) $4 in
               VarNameFun (pos, $1, $3)
          }
        ;

/*
 * Pattern declarations.
 */
patt_decl:
          pattern
          { VarNamePattern (pos_of_pattern $1, $1) }
        | patt_decl2
          { $1 }
        ;

patt_decl2:
          patt_direct_decl
          { $1 }
        | TokStar decl_struct_type_specifiers_opt patt_decl2
          { let pos = union_pos $1 (pos_of_var_name $3) in
               VarNamePointer (pos, $2, $3)
          }
        | TokStar decl_struct_type_specifiers_opt TokId
          { let pos, id = $3 in
            let id = VarNameId (pos, Some id) in
            let pos = union_pos $1 pos in
               VarNamePointer (pos, $2, id)
          }
        | TokAddr decl_struct_type_specifiers_opt patt_decl2
          { let pos, _ = $1 in
            let pos = union_pos pos (pos_of_var_name $3) in
               VarNameRef (pos, $2, $3)
          }
        | TokAddr decl_struct_type_specifiers_opt TokId
          { let pos1, _ = $1 in
            let pos2, id = $3 in
            let id = VarNameId (pos2, Some id) in
            let pos = union_pos pos1 pos2 in
               VarNameRef (pos, $2, id)
          }
        ;

patt_direct_decl:
          TokLeftParen patt_decl2 TokRightParen
          { $2 }
        | patt_direct_decl TokLeftBrack opt_expr TokRightBrack %prec prec_subscript
          { let pos = union_pos (pos_of_var_name $1) $4 in
               match $3 with
                  Some e -> VarNameArray (pos, [], $1, e)
                | None -> VarNamePointer (pos, [], $1)
          }
        | patt_direct_decl TokLeftParen opt_param_list TokRightParen %prec prec_apply
          { let pos = union_pos (pos_of_var_name $1) $4 in
               VarNameFun (pos, $1, $3)
          }
        | TokId TokLeftBrack opt_expr TokRightBrack %prec prec_subscript
          { let pos, id = $1 in
            let id = VarNameId (pos, Some id) in
            let pos = union_pos pos $4 in
               match $3 with
                  Some e -> VarNameArray (pos, [], id, e)
                | None -> VarNamePointer (pos, [], id)
          }
        | TokId TokLeftParen opt_param_list TokRightParen %prec prec_apply
          { let pos, id = $1 in
            let id = VarNameId (pos, Some id) in
            let pos = union_pos pos $4 in
               VarNameFun (pos, id, $3)
          }
        ;

/*
 * Declarations that require an identifier.
 */
id_decl:  direct_decl
          { $1 }
        | TokStar decl_struct_type_specifiers_opt id_decl
          { let pos = union_pos $1 (pos_of_var_name $3) in
               VarNamePointer (pos, $2, $3)
          }
        | TokAddr decl_struct_type_specifiers_opt id_decl
          { let pos, _ = $1 in
            let pos = union_pos pos (pos_of_var_name $3) in
               VarNameRef (pos, $2, $3)
          }
        ;

direct_decl:
          TokId
          { let pos, id = $1 in
               VarNameId (pos, Some id)
          }
        | TokLeftParen id_decl TokRightParen
          { $2 }
        | direct_decl TokLeftBrack opt_expr TokRightBrack %prec prec_subscript
          { let pos = union_pos (pos_of_var_name $1) $4 in
               match $3 with
                  Some e -> VarNameArray (pos, [], $1, e)
                | None -> VarNamePointer (pos, [], $1)
          }
        | direct_decl TokLeftParen opt_param_list TokRightParen %prec prec_apply
          { let pos = union_pos (pos_of_var_name $1) $4 in
               VarNameFun (pos, $1, $3)
          }
        ;

/*
 * Function parameters.
 */
opt_param_list:
          /* empty */
          { [] }
        | param_list
          { $1 }
        ;

param_list:
          rev_param_list
          { List.rev $1 }
        ;

rev_param_list:
          param_decl
          { [$1] }
        | rev_param_list TokComma param_decl
          { $3 :: $1 }
        ;

param_decl:
          simp_decl_spec patt_decl
          { make_param_decl (snd $1) $2 }
        | struct_decl_spec id_decl
          { make_param_decl (snd $1) $2 }
        | simp_decl_spec abstract_decl
          { make_param_decl (snd $1) $2 }
        | struct_decl_spec abstract_decl
          { make_param_decl (snd $1) $2 }
        | simp_decl_spec
          { None, snd $1 }
        | struct_decl_spec
          { None, snd $1 }
        | TokElide
          { None, TypeElide $1 }
        ;

cast_decl:
          simp_decl_spec abstract_decl
          { make_param_decl (snd $1) $2 }
        | struct_decl_spec abstract_decl
          { make_param_decl (snd $1) $2 }
        | decl_spec
          { None, snd $1 }
        ;

fun_param_list:
          TokLeftParen opt_param_list TokRightParen
          { let pos = union_pos $1 $3 in
               pos, $2
          }
        ;

abstract_decl:
          direct_abstract_decl
          { $1 }
        | TokStar decl_struct_type_specifiers_opt
          { let pos = $1 in
               VarNamePointer (pos, $2, VarNameId (pos, None))
          }
        | TokStar decl_struct_type_specifiers_opt abstract_decl
          { let pos = union_pos $1 (pos_of_var_name $3) in
               VarNamePointer (pos, $2, $3)
          }
        | TokAddr decl_struct_type_specifiers_opt
          { let pos, _ = $1 in
               VarNameRef (pos, $2, VarNameId (pos, None))
          }
        | TokAddr decl_struct_type_specifiers_opt abstract_decl
          { let pos, _ = $1 in
            let pos = union_pos pos (pos_of_var_name $3) in
               VarNameRef (pos, $2, $3)
          }
        ;

direct_abstract_decl:
          TokLeftParen abstract_decl TokRightParen
          { $2 }
        | direct_abstract_decl TokLeftBrack opt_expr TokRightBrack %prec prec_subscript
          { let pos = union_pos (pos_of_var_name $1) $4 in
               match $3 with
                  Some e -> VarNameArray (pos, [], $1, e)
                | None -> VarNamePointer (pos, [], $1)
          }
        | TokLeftBrack opt_expr TokRightBrack %prec prec_subscript
          { let pos = union_pos $1 $3 in
               match $2 with
                  Some e -> VarNameArray (pos, [], VarNameId (pos, None), e)
                | None -> VarNamePointer (pos, [], VarNameId (pos, None))
          }
        | direct_abstract_decl fun_param_list %prec prec_apply
          { let pos, params = $2 in
            let pos = union_pos (pos_of_var_name $1) pos in
               VarNameFun (pos, $1, params)
          }
        | fun_param_list %prec prec_apply
          { let pos, params = $1 in
               VarNameFun (pos, VarNameId (pos, None), params)
          }
        ;

/*
 * Declare mutually recursive functions.
 */
fun_def:  simp_decl_spec TokId TokLeftParen opt_param_list TokRightParen TokLeftBrace stmt_list TokRightBrace
          { let pos, id = $2 in
            let id = VarNameId (pos, Some id) in
            let pos = union_pos pos $5 in
            let id = VarNameFun (pos, id, $4) in
            let pos = union_pos $6 $8 in
            let body = wrap_stmt (SeqExpr (pos, $7)) in
               make_fun_def $1 id body
          }
        | struct_decl_spec id_decl TokLeftBrace stmt_list TokRightBrace
          { let pos = union_pos $3 $5 in
            let body = wrap_stmt (SeqExpr (pos, $4)) in
               make_fun_def $1 $2 body
          }
        ;

/*
 * Declare some mutually recursive types.
 */
type_defs:
          TokTypedef decl_spec type_decl_list TokSemi
          { let pos = union_pos $1 $4 in
            let types = make_type_decls (snd $2) $3 in
               List.iter (fun (pos, v, _, ty) ->
                   Fc_parse_state.add_type v pos ty;
	           Fc_parse_state.add_typedef v) types;
               TypeDefs (pos, [])
          }
        | simp_decl_spec TokSemi
          { (* Ignore them *)
            TypeDefs (pos_of_type (snd $1), [])
          }
        | struct_decl_spec TokSemi
          { (* Ignore them *)
            TypeDefs (pos_of_type (snd $1), [])
          }
        ;

/************************************************************************
 * EXPRESSIONS
 ************************************************************************/

/*
 * Initialization expr.
 * This _constructs_ the data.
 */
init:
         nc_expr
         { InitExpr (pos_of_expr $1, $1) }
       | TokLeftBrace rev_init_list opt_comma TokRightBrace
         { let pos = union_pos $1 $4 in
              InitArray (pos, List.rev $2)
         }
       ;

init_field:
         init
         { None, $1 }
       | id TokColon init
         { Some (snd $1), $3 }
       ;

rev_init_list:
         init_field
         { [$1] }
       | rev_init_list TokComma init_field
         { $3 :: $1 }
       ;

/*
 * Patterns.
 * This _destructs_ the data.
 * Patterns can be used in switch statements.
 */
pattern:
          TokChar
          { let pos, c = $1 in
               CharPattern (pos, c)
          }
        | TokInt
          { let pos, i = $1 in
               IntPattern (pos, i)
          }
        | TokFloat
          { let pos, x = $1 in
               FloatPattern (pos, x)
          }
        | TokString
          { let pos, pre, s = $1 in
               StringPattern (pos, pre, s)
          }
        | TokLeftBrace rev_struct_field_pattern_list opt_comma TokRightBrace
          { let pos = union_pos $1 $4 in
               StructPattern (pos, List.rev $2)
          }
        | TokId
          { let pos, v = $1 in
               VarPattern (pos, v, v, None)
          }
        | TokId pattern
          { let pos, id = $1 in
            let pos = union_pos pos (pos_of_pattern $2) in
               EnumPattern (pos, id, $2)
          }
        | TokLeftParen pattern TokRightParen
          { $2 }
        ;

rev_struct_field_pattern_list:
          struct_field_pattern
          { [$1] }
        | rev_struct_field_pattern_list TokComma struct_field_pattern
          { $3 :: $1 }
        ;

struct_field_pattern:
          pattern
          { None, $1 }
        | id TokColon pattern
          { Some (snd $1), $3 }
        ;

/*
 * Expressions.
 * Note:
 *   To avoid a shift/reduce conflict between
 *   function application and array creation, we allow
 *   arbitrary function names
 *       expr ( arglist )
 *   Since the grammar allows only
 *       id ( arglist )
 *   we check in the semantic action for the error.
 */
expr:
          nc_expr
          { $1 }
        | expr TokComma nc_expr
          { make_binop ($2, comma_sym) $1 $3 }
        ;

nc_expr:  TokChar
          { let pos, c = $1 in
               CharExpr (pos, c)
          }
        | TokInt
          { let pos, i = $1 in
               IntExpr (pos, i)
          }
        | TokFloat
          { let pos, x = $1 in
               FloatExpr (pos, x)
          }
        | TokString
          { let pos, pre, s = $1 in
               StringExpr (pos, pre, s)
          }
        | TokId
          { let pos, v = $1 in
               VarExpr (pos, v, v)
          }
        | TokOperator operator
          { let pos, v = $2 in
            let pos = union_pos $1 pos in
               VarExpr (pos, v, v)
          }

          /* Unary operations */
        | TokAddr nc_expr %prec prec_unary
          { make_unop PreOp $1 $2 }
        | TokOp10 nc_expr %prec prec_unary
          { make_unop PreOp $1 $2 }
        | TokStar nc_expr %prec prec_unary
          { make_unop PreOp ($1, star_sym) $2 }
        | TokPreOp12 nc_expr %prec prec_unary
          { make_unop PreOp $1 $2 }
        | TokPrePostOp12 nc_expr %prec prec_unary
          { make_unop PreOp $1 $2 }
        | nc_expr TokPrePostOp12 %prec prec_unary
          { make_unop PostOp $2 $1 }

          /* Binary operations */
        | nc_expr TokEq nc_expr
          { make_binop ($2, eq_sym) $1 $3 }
        | nc_expr TokOp1 nc_expr
          { make_binop $2 $1 $3 }
        | nc_expr TokAnd nc_expr
          { make_and_op $1 $3 }
        | nc_expr TokOr nc_expr
          { make_or_op $1 $3 }
        | nc_expr TokOp4 nc_expr
          { make_binop $2 $1 $3 }
        | nc_expr TokOp5 nc_expr
          { make_binop $2 $1 $3 }
        | nc_expr TokAddr nc_expr
          { make_binop $2 $1 $3 }
        | nc_expr TokOp7 nc_expr
          { make_binop $2 $1 $3 }
        | nc_expr TokOp8 nc_expr
          { make_binop $2 $1 $3 }
        | nc_expr TokOp9 nc_expr
          { make_binop $2 $1 $3 }
        | nc_expr TokOp10 nc_expr
          { make_binop $2 $1 $3 }
        | nc_expr TokStar nc_expr
          { make_binop ($2, star_sym) $1 $3 }
        | nc_expr TokOp11 nc_expr
          { make_binop $2 $1 $3 }
        | nc_expr TokDot id
          { make_project $2 $1 $3 }
        | nc_expr TokOp13 id
          { make_project $2 (make_unop PreOp $2 $1) $3 }

          /* Parens */
        | TokLeftParen expr TokRightParen
          { $2 }
        | TokLeftParen stmt TokRightParen
          { $2 }

          /* Other misc operations */
        | TokSizeof nc_expr %prec prec_unary
          { let pos = union_pos $1 (pos_of_expr $2) in
               SizeofExpr (pos, $2)
          }
        | TokSizeof TokLeftParen cast_decl TokRightParen %prec prec_unary
          { let pos = union_pos $1 $4 in
            let _, ty = $3 in
               SizeofType (pos, ty)
          }
        | TokLeftParen cast_decl TokRightParen nc_expr %prec prec_cast
          { let pos = union_pos $1 (pos_of_expr $4) in
            let _, ty = $2 in
               CastExpr (pos, ty, $4)
          }
        | nc_expr TokLeftBrack nc_expr TokRightBrack %prec prec_subscript
          { let pos = union_pos (pos_of_expr $1) $4 in
               make_binop ($2, subscript_sym) $1 $3
          }
        | nc_expr TokLeftParen args TokRightParen %prec prec_apply
          { let pos = union_pos (pos_of_expr $1) $4 in
               make_op pos PreOp apply_sym ($1 :: $1 :: $3)
          }
        | nc_expr TokQuest nc_expr TokColon nc_expr
          { let pos = union_pos (pos_of_expr $1) (pos_of_expr $5) in
               IfExpr (pos, $1, $3, Some $5)
          }
        ;

/*
 * An optional expression.
 */
opt_expr: /* empty */
          { None }
        | expr
          { Some $1 }
        ;

/*
 * A statement is a terminated expression.
 */
stmt:     TokSemi
          { SeqExpr ($1, []) }
        | expr TokSemi
          { $1 }
        | TokIf TokLeftParen expr TokRightParen stmt %prec prec_ifthen
          { let pos = union_pos $1 (pos_of_expr $5) in
            let stmt = wrap_stmt $5 in
               IfExpr (pos, $3, stmt, None)
          }
        | TokIf TokLeftParen expr TokRightParen stmt TokElse stmt %prec prec_ifthenelse
          { let pos = union_pos $1 (pos_of_expr $7) in
            let stmt1 = wrap_stmt $5 in
            let stmt2 = wrap_stmt $7 in
               IfExpr (pos, $3, stmt1, Some stmt2)
          }
        | TokSwitch TokLeftParen expr TokRightParen stmt
          { let pos = union_pos $1 (pos_of_expr $5) in
            let cases = normalize_cases pos $5 in
               SwitchExpr (pos, $3, cases)
          }
        | TokFor TokLeftParen opt_expr TokSemi opt_expr TokSemi opt_expr TokRightParen stmt
          { let pos = union_pos $1 (pos_of_expr $9) in
            let init = make_opt_expr $3 (one_expr $4) in
            let test = make_opt_expr $5 (one_expr $6) in
            let step = make_opt_expr $7 (one_expr $8) in
            let body = wrap_stmt $9 in
               ForExpr (pos, init, test, step, body)
          }
        | TokWhile TokLeftParen expr TokRightParen stmt
          { let pos = union_pos $1 (pos_of_expr $5) in
            let body = wrap_stmt $5 in
               WhileExpr (pos, $3, body)
          }
        | TokDo stmt TokWhile TokLeftParen expr TokRightParen TokSemi
          { let pos = union_pos $1 $7 in
            let body = wrap_stmt $2 in
               DoExpr (pos, body, $5)
          }
        | TokReturn expr TokSemi
          { let pos = union_pos $1 (pos_of_expr $2) in
               ReturnExpr (pos, $2)
          }
        | TokReturn TokSemi
          { let pos = union_pos $1 $2 in
            let e = SeqExpr (pos, []) in
               ReturnExpr (pos, e)
          }
        | TokRaise expr TokSemi
          { let pos = union_pos $1 (pos_of_expr $2) in
               RaiseExpr (pos, $2)
          }
        | TokBreak TokSemi
          { let pos = union_pos $1 $2 in
               BreakExpr pos
          }
        | TokContinue TokSemi
          { let pos = union_pos $1 $2 in
               ContinueExpr pos
          }
        | TokGoto id TokSemi
          { let pos2, label = $2 in
            let pos = union_pos $1 pos2 in
               GotoExpr (pos, label)
          }
        | open_block stmt_list close_block
          { let pos, defs = $3 in
            let pos = union_pos $1 pos in
            let body =
               match defs with
                  [] -> $2
                | _ -> TypeDefs (pos, defs) :: $2
            in
               SeqExpr (pos, body)
          }
        | all_def
          { $1 }
        | id TokColon
          { let pos, id = $1 in
            let pos = union_pos pos $2 in
               LabelExpr (pos, id)
          }
        | TokCase pattern TokColon
          { let pos = union_pos $1 $3 in
               CaseExpr (pos, $2)
          }
        | TokDefault TokColon
          { let pos = union_pos $1 $2 in
               DefaultExpr pos
          }
        | TokTry stmt
          { let pos = union_pos $1 (pos_of_expr $2) in
               TryExpr (pos, $2, [], None)
          }
        | stmt TokCatch TokLeftParen pattern TokRightParen stmt %prec TokCatch
          { let pos = union_pos (pos_of_expr $1) (pos_of_expr $6) in
               match $1 with
                  TryExpr (pos, e, cases, finally) ->
                     let cases = cases @ [$4, [$6; BreakExpr pos]] in
                        TryExpr (pos, e, cases, finally)
                | _ ->
                     raise (ParseExpError (pos, "illegal catch", $1))
          }
        | stmt TokFinally stmt
          { let pos = union_pos (pos_of_expr $1) (pos_of_expr $3) in
               match $1 with
                  TryExpr (pos, e, cases, finally) ->
                     let finally =
                        match finally with
                           Some _ -> raise (ParseError (pos, "duplicate finally"))
                         | None -> Some $3
                     in
                        TryExpr (pos, e, cases, finally)
                | _ ->
                     raise (ParseExpError (pos, "illegal finally", $1))
          }
        ;

stmt_list:
          rev_stmt_list
          { List.rev $1 }
        ;

rev_stmt_list:
          /* empty */
          { [] }
        | rev_stmt_list stmt
          { $2 :: $1 }
        ;

/*
 * Environment pushing.
 */
open_block:
          TokLeftBrace
          { Fc_parse_state.push_tenv ();
            $1
          }
        ;

close_block:
          TokRightBrace
          { $1, Fc_parse_state.pop_tenv () }
        ;

/*
 * Arguments are comma-separated list of expressions.
 */
args:     /* empty */
          { [] }
        | rev_args
          { List.rev $1 }
        ;

rev_args: nc_expr
          { [$1] }
        | rev_args TokComma nc_expr
          { $3 :: $1 }
        ;

/*
 * Identifiers.
 */
id:       TokId
          { $1 }
        | TokTypeId
          { $1 }
        ;

/*
 * Generic operators.
 */
operator:
          TokComma
          { $1, comma_sym }
        | TokEq
          { $1, eq_sym }
        | TokOp1
          { $1 }
        | TokAnd
          { $1, and_sym }
        | TokOr
          { $1, or_sym }
        | TokOp4
          { $1 }
        | TokOp5
          { $1 }
        | TokAddr
          { $1 }
        | TokOp7
          { $1 }
        | TokOp8
          { $1 }
        | TokOp9
          { $1 }
        | TokOp10
          { $1 }
        | TokStar
          { $1, star_sym }
        | TokOp11
          { $1 }
        | TokPreOp12
          { $1 }
        | TokPrePostOp12
          { $1 }
        | TokOp13
          { $1 }
        ;

/*
 * Optional comma.
 */
opt_comma:
          /* empty */
          { () }
        | TokComma
          { () }
        ;
