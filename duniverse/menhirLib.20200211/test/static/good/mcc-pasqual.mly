/***********************************************************************
 *
 * Parser for the Pasqual grammar.
 * Produces FC parse trees.
 *
 * Contains 1 shift/reduce conflict for dangling-else.
 *
 * Pascal allows (but warns against) the following:
 *  - try/raise
 *  - ref/mutable
 *
 * Doesn't allow:
 *  - psq-style function/procedure types
 *  - return
 * ----------------------------------------------------------------
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
 * Author: Adam Granicz
 * granicz@cs.caltech.edu
 *
 ***********************************************************************/

%{
open Symbol
open Fc_parse_type
open Fc_config
open Fc_parse_util
open Fc_parse_state
open Fc_parse_exn
open Fc_frontends
open Constants

(*
 * Adam: this was not implemented right.
 * Exceptions are not static...
 *)
let pop_exceptions pos =
   TypeDefs (pos, [])

let add_exception _ =
   raise (Invalid_argument "add_exception: needs fixing")


let rawint_of_int i = Rawint.of_int Rawint.Int32 true i
let one_int pos = IntExpr (pos, rawint_of_int 1)

let void_expr pos = UnitExpr (pos, 0, 0)
let int_expr pos i = IntExpr (pos, Rawint.of_int Rawint.Int32 true i)

let make_unop sym expr =
   OpExpr (pos_of_expr expr, PreOp, sym, sym, [expr])

let make_binop expr1 sym expr2 =
   let pos = union_pos (pos_of_expr expr1) (pos_of_expr expr2) in
      OpExpr (pos, PreOp, sym, sym, [expr1; expr2])

let halt_if_not_pasqual pos msg =
    if not (FrontEnd.pasqual_program ()) then
	raise (ParseError (pos, ("Pasqual feature: " ^ msg)))

let halt_if_not_pascal pos msg =
    if FrontEnd.pasqual_program () then
	raise (ParseError (pos, ("Pascal feature: " ^ msg)))

let warn_if_not_pasqual pos msg =
    if not (FrontEnd.pasqual_program ()) then begin
	print_pos pos;
	Format.print_string ("warning: Pasqual feature: " ^ msg);
	Format.print_newline ()
    end

let warn_if_not_pascal pos msg =
    if FrontEnd.pasqual_program () then begin
	print_pos pos;
	Format.print_string ("warning: Pascal feature: " ^ msg);
	Format.print_newline ()
    end

let exn_symbol		= Symbol.add "exn"

let subscript_sym 	= Symbol.add "[]"
let apply_sym		= Symbol.add "()"

let assign_sym		= Symbol.add "="
let plusOp_sym		= Symbol.add "+"
let minusOp_sym		= Symbol.add "-"
let divOp_sym		= Symbol.add "pasq_/"
let intDivideOp_sym	= Symbol.add "/"
let timesOp_sym		= Symbol.add "*"
let modOp_sym		= Symbol.add "%"

let commaOp_sym		= Symbol.add ","

let uNotOp_sym		= Symbol.add "~"
let uMinusOp_sym	= Symbol.add "-"
let uAddrOfOp_sym	= Symbol.add "&"
let uDerefOp_sym	= Symbol.add "*"

let eqOp_sym		= Symbol.add "=="
let notEqOp_sym		= Symbol.add "!="
let leOp_sym		= Symbol.add "<="
let ltOp_sym		= Symbol.add "<"
let gtOp_sym		= Symbol.add ">"
let geOp_sym		= Symbol.add ">="

let andOp_sym		= Symbol.add "pasq_&&"
let orOp_sym		= Symbol.add "pasq_||"
let xorOp_sym		= Symbol.add "^"
let lShiftOp_sym	= Symbol.add "<<"
let rShiftOp_sym	= Symbol.add ">>"

let main_sym		= Symbol.add main_string
let argc_sym		= Symbol.add "argc"
let argv_sym		= Symbol.add "argv"

let type_void pos 		= TypeUnit(pos, StatusNormal, 1)
let type_char pos 		= TypeChar(pos, StatusNormal, Rawint.Int8, false)
let type_int pos prec signed 	= TypeInt(pos, StatusNormal, prec, signed)
let type_int32 pos 		= TypeInt(pos, StatusNormal, Rawint.Int32, true)
let type_float pos prec 	= TypeFloat(pos, StatusNormal, prec)
let type_float32 pos 		= TypeFloat(pos, StatusNormal, Rawfloat.Single)
let type_pointer pos ty 	= TypePointer(pos, StatusNormal, ty)
let type_mutable pos ty 	= TypeRef(pos, StatusNormal, ty)
let type_poly pos sym 		= TypePoly(pos, StatusNormal)
let type_apply pos ty_list sym 	= TypeApply(pos, StatusNormal, sym, ty_list)

let type_boolean pos 		= TypeUnit(pos, StatusNormal, 2)
let type_cstring pos 		= type_pointer pos (type_char pos)
let type_fun pos ty_list ty 	= TypeFun(pos, StatusNormal, ty_list, ty)
let type_var pos sym 		= TypeVar(pos, StatusNormal, sym)
let type_array pos ty e1 e2 	= TypeArray(pos, StatusNormal, ty, e1, e2)
let type_conf_array pos ty v1 v2 v_ty =
				  TypeConfArray(pos, StatusNormal, ty, v1, v2, v_ty)
let type_enum pos fields	= TypeEnum(pos, StatusNormal, Fields fields)
let type_uenum pos fields	= TypeUEnum(pos, StatusNormal, Fields (exn_symbol, fields))

(*
 * Constant types
 *)
let ctype_int pos		= TypeInt (pos, StatusConst, Rawint.Int32, true)
let ctype_float pos		= TypeFloat (pos, StatusConst, Rawfloat.Double)
let ctype_cstring pos		= TypePointer (pos, StatusConst, TypeChar (pos, StatusConst, Rawint.Int8, false))

(*
 * Build expressions.
 *)
let wrap_expr e =
    let pos = pos_of_expr e in
    SeqExpr (pos, [e; UnitExpr (pos, 1, 0)])

let wrap_expr_list pos elist =
    elist @ [UnitExpr (pos, 1, 0)]

let make_apply_expr pos e el =
    OpExpr (pos, PreOp, apply_sym, apply_sym, e :: e :: el)

let make_string_out_of_char_list char_list =
    let chars = List.map (fun c -> Char.code c) char_list in
	Array.of_list chars

let sym_of_var_expr = function
      VarExpr (_, sym, _) ->
        sym
    | _ -> raise (Invalid_argument "sym_of_var_expr: not a VarExpr")

let rec make_array_type pos conformant indices ty =
    let indices = List.rev indices in
    List.fold_left (fun ty (e1, e2) ->
	match conformant with
	      true ->
		type_conf_array pos ty (sym_of_var_expr e1) (sym_of_var_expr e2) (type_int32 pos)
	    | false ->
		type_array pos ty e1 e2) ty indices

let make_assign_expr pos e1 e2 =
    OpExpr (pos, PreOp, assign_sym, assign_sym, [e1; e2])

let rec make_case_statement expr1 case_expr_list else_expr pos =
   match case_expr_list with
      (head_case, head_what) :: l -> IfExpr (pos, make_binop expr1 eqOp_sym head_case, head_what, Some (make_case_statement expr1 l else_expr pos))
    | _ -> SeqExpr (pos, [else_expr])

let make_subscript_expr expr1 expr_list pos =
   let res = List.fold_left (fun e1 e2 -> make_binop e1 subscript_sym e2) expr1 expr_list in
      res

let extract_expressions expr_option_list=
    let exprs = List.filter (fun element ->
	match element with
	      Some e -> true
	    | None -> false) expr_option_list in
    let expr_list_list = List.map (fun a ->
	match a with
	      Some a -> a
	    | None -> raise (Invalid_argument "extract_expressions")) exprs in
	List.flatten expr_list_list

let rec make_enum_pattern ids =
    match ids with
	  id :: [] ->
	    VarPattern (snd id, fst id, fst id, None)
	| id :: rest ->
	    EnumPattern (snd id, fst id, make_enum_pattern rest)
	| [] ->
	    raise (Invalid_argument "make_enum_pattern")

let goto_symbol_of_int (i, pos) =
    Symbol.add ("goto_sym" ^ string_of_int (Rawint.to_int i))

let make_function_definition heading body pos =
    let (sym, p1), params, (ty, p2) = heading in
    let pos = union_pos p1 pos in
	FunDef(pos, StoreAuto, sym, sym, params, ty, SeqExpr (snd body, fst body)), pos

let make_proc_definition heading body pos =
    let (sym, p1), params = heading in
    let pos = union_pos p1 pos in
	FunDef(pos, StoreAuto, sym, sym, params, type_void p1, SeqExpr (snd body, fst body)), pos

let make_program exps pos body ty =
    let exns = pop_exceptions pos in
    let body = wrap_expr_list pos (fst body), snd body in
	exns :: exps @ [FunDef(pos, StoreAuto, main_sym, main_sym,
            [(pos, VarPattern (pos, argc_sym, argc_sym, None), type_int32 pos);
	     (pos, VarPattern (pos, argv_sym, argv_sym, None), type_pointer pos (type_cstring pos))],
	     ty pos, SeqExpr (snd body, fst body))]

let make_program_without_body exps pos =
    let exns = pop_exceptions pos in
	exns :: exps

let make_type_definition pos tydefs =
    let types = List.map (fun (pos, sym, ty, _) ->
	Fc_parse_state.add_type sym pos ty;
        Fc_parse_state.add_typedef sym;
	pos, sym, sym, ty) (fst tydefs) in
    let expr_option_list = List.map (fun (_, _, _, expro) -> expro) (fst tydefs) in
    let methods = extract_expressions expr_option_list in
	TypeDefs(pos, types) :: methods, pos

let make_for pos cv b1 relop b2 binop body =
    let sym, p = cv in
    let init = make_assign_expr pos (VarExpr (p, sym, sym)) b1 in
    let test = make_binop (VarExpr (p, sym, sym)) relop b2 in
    let step = make_assign_expr pos (VarExpr (p, sym, sym)) (make_binop (VarExpr (p, sym, sym)) binop (one_int pos)) in
        ForExpr (pos, init, test, step, body)

let make_function_decl heading ie f_pos =
    let (sym, pos), params, (ty, pos2) = heading in
    let tpos = union_pos pos f_pos in
    let param_ty_list = List.map (fun (_, _, ty) -> ty) params in
    let fun_ty = TypeFun(pos, StatusNormal, param_ty_list, ty) in
    let sym = VarPattern (pos, sym, sym, None) in
	match ie with
	      None ->
	        VarDefs(tpos, [pos, StoreStatic, sym, fun_ty, InitNone]), tpos
	    | Some (prec, ia, spos) ->
	        VarDefs(tpos, [pos, StoreStatic, sym, fun_ty, InitExpr (spos, StringExpr (spos, prec, ia))]), tpos

let make_proc_decl heading ie f_pos =
    let (sym, pos), params = heading in
    let tpos = union_pos pos f_pos in
    let param_ty_list = List.map (fun (_, _, ty) -> ty) params in
    let fun_ty = TypeFun(pos, StatusNormal, param_ty_list, type_void pos) in
    let sym = VarPattern (pos, sym, sym, None) in
	match ie with
	      None ->
	        VarDefs(tpos, [pos, StoreStatic, sym, fun_ty, InitNone]), tpos
	    | Some (prec, ia, spos) ->
	        VarDefs(tpos, [pos, StoreStatic, sym, fun_ty, InitExpr (spos, StringExpr (spos, prec, ia))]), tpos

let make_case pos e elist else_expr =
    let else_expr =
	match else_expr with
	      None ->
		SeqExpr (pos, [])
	    | Some ee ->
		ee
    in
    let exp = make_case_statement e elist else_expr pos in
    match exp with
         SeqExpr (_, []) ->
	    raise (ParseError (pos, "No case specified in 'case' statement"))
       | _ ->
    	    exp

let make_matched_vardecl patt e pos =
    let var = pos, StoreAuto, patt, TypePoly (pos, StatusNormal), InitExpr (pos, e) in
	VarDefs (pos, [var])

let make_matched_vardecl_aggregate patt elist pos =
    let init_e =
	let elist = List.map (fun e -> None, InitExpr (pos_of_expr e, e)) elist in
	    InitArray (pos, elist)
    in
    let var = pos, StoreAuto, patt, TypePoly (pos, StatusNormal), init_e in
	VarDefs (pos, [var])

let make_enum_type pos id_expr_list =
    let fields = List.map (fun ((sym, pos), eo) ->
	let _ = add_enum_label sym pos in
	    pos, sym, eo) id_expr_list in
	type_enum pos fields

let rec make_string_aux s i = function
      current :: rest ->
        s.[i] <- Char.chr current;
	make_string_aux s (i+1) rest
    | [] ->
	s

let string_of_int_array ia =
    let int_list = (Array.to_list ia) in
	make_string_aux (String.create (List.length int_list)) 0 int_list

%}
%token TokEof

/*
 * Operators, special characters
 */
%token <Fc_parse_type.pos> TokNil
%token <Fc_parse_type.pos> TokTrue
%token <Fc_parse_type.pos> TokFalse

%token <Fc_parse_type.pos> TokPlus
%token <Fc_parse_type.pos> TokMinus
%token <Fc_parse_type.pos> TokStar
%token <Fc_parse_type.pos> TokSlash
%token <Fc_parse_type.pos> TokLShift
%token <Fc_parse_type.pos> TokRShift
%token <Fc_parse_type.pos> TokEq
%token <Fc_parse_type.pos> TokAssignEq
%token <Fc_parse_type.pos> TokNotEq
%token <Fc_parse_type.pos> TokLt
%token <Fc_parse_type.pos> TokLe
%token <Fc_parse_type.pos> TokGt
%token <Fc_parse_type.pos> TokGe
%token <Fc_parse_type.pos> TokAmp
%token <Fc_parse_type.pos> TokHat

%token <Fc_parse_type.pos> TokArrow
%token <Fc_parse_type.pos> TokAt

%token <Fc_parse_type.pos> TokColon
%token <Fc_parse_type.pos> TokSemi
%token <Fc_parse_type.pos> TokComma
%token <Fc_parse_type.pos> TokDot
%token <Fc_parse_type.pos> TokTwoDots
%token <Fc_parse_type.pos> TokLeftParen
%token <Fc_parse_type.pos> TokRightParen
%token <Fc_parse_type.pos> TokLeftBrack
%token <Fc_parse_type.pos> TokRightBrack
%token <Fc_parse_type.pos> TokLeftBrace
%token <Fc_parse_type.pos> TokRightBrace
%token <Fc_parse_type.pos> TokLeftTuple
%token <Fc_parse_type.pos> TokRightTuple

%token <Fc_parse_type.pos> TokBrackets

/*
 * Keywords
 */
%token <Fc_parse_type.pos> TokMod
%token <Fc_parse_type.pos> TokDiv
%token <Fc_parse_type.pos> TokAnd
%token <Fc_parse_type.pos> TokOr
%token <Fc_parse_type.pos> TokXor
%token <Fc_parse_type.pos> TokNot
%token <Fc_parse_type.pos> TokShl
%token <Fc_parse_type.pos> TokShr

%token <Fc_parse_type.pos> TokBegin
%token <Fc_parse_type.pos> TokEnd
%token <Fc_parse_type.pos> TokBreak
%token <Fc_parse_type.pos> TokContinue
%token <Fc_parse_type.pos> TokGoto
%token <Fc_parse_type.pos> TokLabel
%token <Fc_parse_type.pos> TokSizeOf

%token <Fc_parse_type.pos> TokCase

%token <Fc_parse_type.pos> TokWhile
%token <Fc_parse_type.pos> TokFor
%token <Fc_parse_type.pos> TokRepeat
%token <Fc_parse_type.pos> TokUntil
%token <Fc_parse_type.pos> TokDo
%token <Fc_parse_type.pos> TokTry
%token <Fc_parse_type.pos> TokExcept
%token <Fc_parse_type.pos> TokFinally
%token <Fc_parse_type.pos> TokRaise
%token <Fc_parse_type.pos> TokException
%token <Fc_parse_type.pos> TokWith

%token <Fc_parse_type.pos> TokType
%token <Fc_parse_type.pos> TokClass
%token <Fc_parse_type.pos> TokVar
%token <Fc_parse_type.pos> TokLet
%token <Fc_parse_type.pos> TokConst
%token <Fc_parse_type.pos> TokProcedure
%token <Fc_parse_type.pos> TokFunction
%token <Fc_parse_type.pos> TokProgram
%token <Fc_parse_type.pos> TokArray
%token <Fc_parse_type.pos> TokRecord
%token <Fc_parse_type.pos> TokTuple
%token <Fc_parse_type.pos> TokUnit
%token <Fc_parse_type.pos> TokImplementation
%token <Fc_parse_type.pos> TokInterface
%token <Fc_parse_type.pos> TokUses
%token <Fc_parse_type.pos> TokOperator

%token <Fc_parse_type.pos> TokOf
%token <Fc_parse_type.pos> TokIf
%token <Fc_parse_type.pos> TokThen
%token <Fc_parse_type.pos> TokElse

%token <Fc_parse_type.pos> TokForward
%token <Fc_parse_type.pos> TokExternal
%token <Fc_parse_type.pos> TokTo
%token <Fc_parse_type.pos> TokDownTo
%token <Fc_parse_type.pos> TokPrivate
%token <Fc_parse_type.pos> TokPublic

%token <Fc_parse_type.pos> TokReturn
%token <Fc_parse_type.pos> TokMutable

/*
 * Types
 */
%token <Fc_parse_type.pos> TokTypeByte
%token <Fc_parse_type.pos> TokTypeChar
%token <Fc_parse_type.pos> TokTypeShort
%token <Fc_parse_type.pos> TokTypeWord
%token <Fc_parse_type.pos> TokTypeInteger
%token <Fc_parse_type.pos> TokTypeDWord
%token <Fc_parse_type.pos> TokTypeInt64
%token <Fc_parse_type.pos> TokTypeUInt64
%token <Fc_parse_type.pos> TokTypeReal
%token <Fc_parse_type.pos> TokTypeDouble
%token <Fc_parse_type.pos> TokTypeLongDouble
%token <Fc_parse_type.pos> TokTypeBoolean
%token <Fc_parse_type.pos> TokPoly
%token <Fc_parse_type.pos> TokTypeCString

%token <Fc_parse_type.pos> TokRef
%token <Fc_parse_type.pos> TokDeref

/*
 * Terminal tokens
 */
%token <Rawint.rawint * Fc_parse_type.pos> TokInt
%token <Rawfloat.rawfloat * Fc_parse_type.pos> TokFloat
%token <char * Fc_parse_type.pos> TokChar
%token <Fc_config.precision * int array * Fc_parse_type.pos> TokString
%token <Symbol.symbol * Fc_parse_type.pos> TokId

/*
 * Precedences
 */
%left TokComma
%right TokEq
%left TokAmp
%left TokHat
%left TokMod
%left TokNotEq
%left TokLe TokLt TokGe TokGt
%left TokLShift TokRShift
%left TokPlus TokMinus
%left TokStart TokSlash
%right prec_unary TokNot
%left prec_apply prec_subscript TokDot TokLeftPrent TokLeftBrack

%nonassoc prec_ifthen
%nonassoc TokElse prec_ifthenelse

/*
 * Program
 */
%start console
%type <Fc_parse_type.prog option> console

%start program
%type <Fc_parse_type.prog> program

%start bootstrap
%type <Fc_parse_type.expr list> bootstrap

%start unit_body
%type <Fc_parse_type.prog> unit_body

%%

console:
      statement 		{ Some [$1] }
    | program 			{ Some $1 }
    | unit_body 		{ Some $1 }
    | TokEof 			{ None }

program:
      program_header uses_clause program_body
				{ [$2] @ $3 }
    | program_header program_body
				{ $2 }

bootstrap:
      block_list TokEof 	{ let exns = pop_exceptions (snd $1) in
				  let _ = Fc_parse_state.pop_tenv() in
				    exns :: fst $1
				}
    | program_body		{ $1 }
    | TokEof 			{ [] }

unit_body:
      unit_header uses_clause interface_body implementation_body statements TokDot
    				{ (([$2] @ $3) @ $4) @ (fst $5) }
    | unit_header interface_body implementation_body statements TokDot
				{ ($2 @ $3) @ (fst $4) }
    | unit_header uses_clause interface_body implementation_body TokEnd TokDot
				{ [$2] @ $3 @ $4 }
    | unit_header interface_body implementation_body TokEnd TokDot
				{ $2 @ $3 }

/* HACK: ignore UnitsUsed for now */
uses_clause:
      TokUses plain_identifier_list TokSemi
				{ void_expr (union_pos $1 $3) }

unit_header:
      TokUnit identifier TokSemi
    				{ $2 }

function_declaration:
      function_heading TokSemi TokForward TokSemi
				{ make_function_decl $1 None $4 }
    | function_heading TokSemi TokExternal TokEq TokString TokSemi
				{ make_function_decl $1 (Some $5) $6 }

function_definition:
      function_heading TokSemi function_body TokSemi
    				{ make_function_definition $1 $3 $4 }
    | function_heading TokEq expression TokSemi
				{ let pos = pos_of_expr $3 in
				    make_function_definition $1 ([ReturnExpr (pos, $3)], pos) $4
				}
    | operator_heading TokSemi function_body TokSemi
				{ make_function_definition $1 $3 $4 }
    | operator_heading TokEq expression TokSemi
				{ let pos = pos_of_expr $3 in
				    make_function_definition $1 ([ReturnExpr (pos, $3)], pos) $4
				}

procedure_declaration:
      procedure_heading TokSemi TokForward TokSemi
				{ make_proc_decl $1 None $4 }
    | procedure_heading TokSemi TokExternal TokEq TokString TokSemi
				{ make_proc_decl $1 (Some $5) $6 }

procedure_definition:
      procedure_heading TokSemi procedure_body TokSemi
				{ make_proc_definition $1 $3 $4 }
    | procedure_heading TokEq statement TokSemi
				{ make_proc_definition $1 ([$3], pos_of_expr $3) $4 }

interface_body:
      interface_body interface_body_element
    				{ $1 @ $2 }
    | interface_body_element	{ $1 }

interface_body_element:
      function_declaration	{ [fst $1] }
    | procedure_declaration	{ [fst $1] }
    | type_def 			{ fst $1 }
    | variable_decl		{ [$1] }

implementation_body:
      TokImplementation block_list
    				{ fst $2 }

program_header:
      TokProgram identifier TokSemi
				{ $2, [], union_pos $1 $3 }
    | TokProgram identifier TokLeftParen plain_identifier_list TokRightParen TokSemi
				{ $2, $4, union_pos $1 $6 }

identifier:
      TokId 			{ $1 }

plain_identifier_list:
      plain_identifier_list TokComma identifier
				{ $1 @ [$3] }
    | identifier		{ [$1] }

identifier_and_expr:
      identifier TokEq expression
    				{ $1, Some $3 }
    | identifier		{ $1, None }

identifier_and_expr_list:
      identifier_and_expr_list TokComma identifier_and_expr
				{ $1 @ [$3] }
    | identifier_and_expr	{ [$1] }

var_pattern_list:
      var_pattern_list TokComma var_pattern
				{ $1 @ [$3] }
    | var_pattern		{ [$1] }

var_pattern:
      identifier		{ VarPattern(snd $1, fst $1, fst $1, None), snd $1 }
    | TokLeftParen plain_identifier_list TokRightParen
				{ let fields = List.map (fun (sym, pos) ->
				    None, VarPattern(pos, sym, sym, None)) $2 in
				    StructPattern(union_pos $1 $3, fields), union_pos $1 $3
				}

block_list:
      block_list block		{ fst $1 @ fst $2, union_pos (snd $1) (snd $2) }
    | block			{ $1 }

block:
      type_def			{ $1 }
    | exception_declaration	{ [$1], pos_of_expr $1 }
    | variable_decl		{ [$1], pos_of_expr $1 }
    | constant_declaration	{ [$1], pos_of_expr $1 }
    | function_declaration 	{ [fst $1], snd $1 }
    | function_definition	{ [fst $1], snd $1 }
    | procedure_declaration	{ [fst $1], snd $1 }
    | procedure_definition	{ [fst $1], snd $1 }
    | label_declaration		{ [$1], pos_of_expr $1 }

/* HACK: label declarations are ignored */
label_declaration:
      TokLabel label_list TokSemi
				{ void_expr (union_pos $1 $3) }

label_list:
      label_list TokComma label { $1 @ [$3] }
    | label			{ [$1] }

label:
      identifier		{ snd $1 }
    | TokInt			{ snd $1 }

program_body:
      block_list statements TokDot
    				{ make_program (fst $1) (union_pos (snd $1) $3) $2 type_void }
    | block_list statements TokAnd TokReturn TokDot
				{ warn_if_not_pasqual (union_pos (snd $1) $5) "\"and return\"";
				  make_program (fst $1) (union_pos (snd $1) $5) $2 type_int32
				}
    | block_list TokDot		{ make_program_without_body (fst $1) (union_pos (snd $1) $2) }
    | statements TokDot		{ make_program [] (union_pos (snd $1) $2) $1 type_void }
    | statements TokAnd TokReturn TokDot
				{ warn_if_not_pasqual (union_pos (snd $1) $4) "\"and return\"";
				  make_program [] (union_pos (snd $1) $4) $1 type_int32
				}

body:
      block_list statements 	{ (fst $1) @ (wrap_expr_list (snd $2) (fst $2)), union_pos (snd $1) (snd $2) }
    | statements		{ wrap_expr_list (snd $1) (fst $1), snd $1 }

type_def:
      TokType type_definition_list
    				{ make_type_definition (union_pos $1 (snd $2)) $2 }

type_definition_list:
      type_definition_list_rev	{ List.rev (fst $1), snd $1 }

type_definition_list_rev:
      type_definition_list_rev type_definition
    				{ (fst $2) :: (fst $1), union_pos (snd $2) (snd $1) }
    | type_definition		{ [fst $1], snd $1 }

type_definition:
    identifier TokEq type_denoter TokSemi
				{
				  let sym, p1 = $1 in
				  let pos = union_pos p1 $4 in
				    (p1, sym, TypeLambda(snd $3, StatusNormal, [], fst $3), None), pos
				}

type_list:
      type_list TokSemi type_denoter
    				{ (fst $1) @ [fst $3], union_pos (snd $1) (snd $3) }
    | type_denoter 		{ [fst $1], snd $1 }

type_list_with_comma:
      type_list_with_comma TokComma type_denoter
    				{ (fst $1) @ [fst $3], union_pos (snd $1) (snd $3) }
    | type_denoter 		{ [fst $1], snd $1 }

type_denoter:
      ref_or_basic_type		{ $1 }

parameter_type_denoter:
      ref_or_basic_parameter_type
    				{ $1 }
ref_or_basic_type:
      TokRef basic_type 	{ let pos = union_pos $1 (snd $2) in
    				  warn_if_not_pasqual pos "\"ref\"";
				    type_pointer pos (fst $2), pos
				}
    | TokMutable basic_type	{ let pos = union_pos $1 (snd $2) in
    				  warn_if_not_pasqual pos "\"mutable\"";
				  type_mutable pos (fst $2), pos
				}
    | TokHat basic_type		{ let pos = union_pos $1 (snd $2) in
				    type_pointer pos (fst $2), pos
				}
    | basic_type		{ $1 }

ref_or_basic_parameter_type:
      TokRef basic_type 	{ let pos = union_pos $1 (snd $2) in
    				  warn_if_not_pasqual pos "\"ref\"";
				    type_pointer pos (fst $2), pos
				}
    | TokMutable basic_type	{ let pos = union_pos $1 (snd $2) in
    				  warn_if_not_pasqual pos "\"mutable\"";
				  type_mutable pos (fst $2), pos
				}
    | TokHat basic_type		{ let pos = union_pos $1 (snd $2) in
				    type_pointer pos (fst $2), pos
				}
    | basic_parameter_type	{ $1 }

type_identifier:
      TokTypeChar		{ type_char $1, $1 }
    | TokTypeByte		{ type_int $1 Rawint.Int8 true, $1}
    | TokTypeShort		{ type_int $1 Rawint.Int16 true, $1 }
    | TokTypeWord		{ type_int $1 Rawint.Int16 false, $1 }
    | TokTypeInteger		{ type_int $1 Rawint.Int32 true, $1 }
    | TokTypeDWord		{ type_int $1 Rawint.Int32 false, $1 }
    | TokTypeInt64		{ type_int $1 Rawint.Int64 true, $1 }
    | TokTypeUInt64		{ type_int $1 Rawint.Int64 false, $1 }

    | TokTypeReal		{ type_float $1 Rawfloat.Single, $1 }
    | TokTypeDouble		{ type_float $1 Rawfloat.Double, $1 }
    | TokTypeLongDouble		{ type_float $1 Rawfloat.LongDouble, $1 }

    | TokTypeBoolean		{ type_boolean $1, $1 }

    | TokTypeCString		{ type_cstring $1, $1 }

tuple_type:
      TokTuple TokOf TokLeftParen type_list_with_comma TokRightParen
    				{ TypeProduct (union_pos $1 $5, StatusNormal, fst $4), union_pos $1 $5 }

basic_type:
      tuple_type		{ $1 }
    | type_identifier		{ $1 }
    | identifier		{ type_apply (snd $1) [] (fst $1), snd $1 }
    | TokPoly identifier	{ warn_if_not_pasqual (union_pos $1 (snd $2)) "polymorphic types";
				  type_var (union_pos $1 (snd $2)) (fst $2), union_pos $1 (snd $2)
				}
    | enum_type			{ $1 }
    | function_type 		{ halt_if_not_pasqual (snd $1) "function-type"; $1 }
    | procedure_type 		{ $1 }
    | new_structured_type 	{ $1 }

basic_parameter_type:
      tuple_type		{ $1 }
    | type_identifier		{ $1 }
    | identifier		{ type_apply (snd $1) [] (fst $1), snd $1 }
    | TokPoly identifier	{ warn_if_not_pasqual (union_pos $1 (snd $2)) "polymorphic types";
				  type_var (union_pos $1 (snd $2)) (fst $2), union_pos $1 (snd $2)
				}
    | function_type 		{ halt_if_not_pasqual (snd $1) "function-type"; $1 }
    | procedure_type 		{ $1 }
    | new_parameter_structured_type
				{ $1 }
new_structured_type:
      array_type 		{ $1 }
    | record_type 		{ $1 }

new_parameter_structured_type:
      conformant_array_type	{ $1 }
    | record_type		{ $1 }

enum_type:
      TokLeftParen identifier_and_expr_list TokRightParen
    				{ make_enum_type (union_pos $1 $3) $2, union_pos $1 $3 }

array_type:
      TokArray TokLeftBrack index_list TokRightBrack TokOf type_denoter
				{ make_array_type (union_pos $1 (snd $6)) false $3 (fst $6), union_pos $1 (snd $6) }

conformant_array_type:
      TokArray TokLeftBrack conf_index_list TokRightBrack TokOf parameter_type_denoter
    				{ make_array_type (union_pos $1 (snd $6)) true $3 (fst $6), union_pos $1 (snd $6) }

exception_declaration:
      TokException identifier TokColon type_identifier TokSemi
				{ let _ = add_exception (snd $2, fst $2, Some (fst $4)) in
				    void_expr (union_pos $1 $5)
				}
    | TokException identifier TokSemi
				{ let _ = add_exception (snd $2, fst $2, None) in
				    void_expr (union_pos $1 $3)
				}

index_list:
      index_list TokComma index { $1 @ [$3] }
    | index 			{ [$1] }

index:
      index_prim TokTwoDots index_prim
    				{ $1, $3 }

index_prim:
      TokInt			{ IntExpr (snd $1, fst $1) }
    | identifier		{ VarExpr (snd $1, fst $1, fst $1) }

conf_index_list:
      conf_index_list TokComma conf_index
    				{ $1 @ [$3] }
    | conf_index		{ [$1] }

conf_index:
      identifier TokTwoDots identifier
				{ VarExpr (snd $1, fst $1, fst $1), VarExpr (snd $3, fst $3, fst $3) }

record_type:
      TokRecord record_section_list TokSemi TokEnd
				{ let fields = List.map (fun (pos, sym, ty) -> pos, sym, ty, None) $2 in
			          let pos = union_pos $1 $4 in
			             TypeStruct(pos, StatusNormal, Fields fields), pos
				}

record_section_list:
      record_section_list TokSemi record_section
    				{ $1 @ $3 }
    | record_section 		{ $1 }

record_section:
      plain_identifier_list TokColon type_denoter
				{ List.map (fun (sym, pos) -> pos, sym, fst $3) $1 }

constant_declaration:
      TokConst constant_declaration_list TokSemi
				{ let vars = List.map (fun (pos, patt, ty, ie) ->
				    pos, StoreAuto, patt, ty, ie) $2 in
					VarDefs (union_pos $1 $3, vars)
				}

constant_declaration_list:
      constant_declaration_list TokSemi constant_decl_prim
    				{ $1 @ $3 }
    | constant_decl_prim	{ $1 }

constant_decl_prim:
      plain_identifier_list TokEq TokInt
    				{ List.map (fun (sym, pos) ->
				    pos, VarPattern (pos, sym, sym, None), ctype_int pos,
					InitExpr (pos, IntExpr (snd $3, fst $3))) $1
				}
    | plain_identifier_list TokEq TokMinus TokInt
    				{ List.map (fun (sym, pos) ->
				    pos, VarPattern (pos, sym, sym, None), ctype_int pos,
					InitExpr (pos,
					    make_unop uMinusOp_sym (IntExpr (snd $4, fst $4)))) $1
				}
    | plain_identifier_list TokEq TokFloat
    				{ List.map (fun (sym, pos) ->
				    pos, VarPattern (pos, sym, sym, None), ctype_float pos,
					InitExpr (pos, FloatExpr (snd $3, fst $3))) $1
				}
    | plain_identifier_list TokEq TokMinus TokFloat
    				{ List.map (fun (sym, pos) ->
				    pos, VarPattern (pos, sym, sym, None), ctype_float pos,
					InitExpr (pos,
					    make_unop uMinusOp_sym (FloatExpr (snd $4, fst $4)))) $1
				}
    | plain_identifier_list TokEq TokString
				{ let sprec, sia, spos = $3 in
    				  List.map (fun (sym, pos) ->
				    pos, VarPattern (pos, sym, sym, None), ctype_cstring pos,
					InitExpr (pos, StringExpr (spos, sprec, sia))) $1
				}

variable_decl:
      TokVar variable_declaration_list TokSemi
				{ let vars = List.map (fun (pos, patt, ty, ie) ->
				    pos, StoreAuto, patt, ty, ie) $2 in
					VarDefs(union_pos $1 $3, vars)
				}
variable_declaration_list:
      variable_declaration_list TokSemi variable_declaration
    				{ $1 @ $3 }
    | variable_declaration_list TokSemi variable_init_declaration
    				{ $1 @ $3 }
    | variable_declaration	{ $1 }
    | variable_init_declaration { $1 }

variable_declaration:
      var_pattern_list TokColon type_denoter
				{ List.map (fun (patt, pos) -> pos, patt, fst $3, InitNone) $1 }

variable_init_declaration:
      var_pattern_list TokEq TokInt
				{ List.map (fun (patt, pos) -> pos, patt, type_int32 pos, InitExpr (snd $3, IntExpr (pos, fst $3))) $1 }

    | var_pattern_list TokEq TokFloat
				{ List.map (fun (patt, pos) -> pos, patt, type_float32 pos, InitExpr (snd $3, FloatExpr (pos, fst $3))) $1 }
    | var_pattern_list TokEq TokString
				{ let prec, ia, spos = $3 in
				    List.map (fun (patt, pos) -> pos, patt, type_cstring pos, InitExpr (spos, StringExpr (pos, prec, ia))) $1
				}
    | var_pattern_list TokColon type_denoter TokEq expression
				{ List.map (fun (patt, pos) -> pos, patt, fst $3, InitExpr (pos_of_expr $5, $5)) $1 }
    | var_pattern_list TokColon type_denoter TokEq array_init_expression
				{ List.map (fun (patt, pos) -> pos, patt, fst $3, InitArray(pos, $5)) $1 }

/*****************************************
 * Functions
 *****************************************/

operator_symbol:
      TokBrackets		{ subscript_sym, $1 }
    | TokEq			{ eqOp_sym, $1 }
    | TokAssignEq		{ assign_sym, $1 }
    | TokPlus			{ plusOp_sym, $1 }
    | TokMinus			{ minusOp_sym, $1 }
    | TokDiv			{ divOp_sym, $1 }
    | TokSlash			{ intDivideOp_sym, $1 }
    | TokStar			{ timesOp_sym, $1 }
    | TokMod			{ modOp_sym, $1 }
    | TokLe			{ leOp_sym, $1 }
    | TokLt			{ ltOp_sym, $1 }
    | TokGe			{ geOp_sym, $1 }
    | TokGt			{ gtOp_sym, $1 }
    | TokAnd			{ andOp_sym, $1 }
    | TokOr			{ orOp_sym, $1 }
    | TokXor			{ xorOp_sym, $1 }
    | TokShl			{ lShiftOp_sym, $1 }
    | TokShr			{ rShiftOp_sym, $1 }

function_type:
      TokFunction TokLeftParen TokRightParen TokArrow type_denoter
    				{ let pos = union_pos $1 (snd $5) in
				  halt_if_not_pasqual pos "function-type";
    				    type_pointer pos (type_fun pos [] (fst $5)), pos
				}
    | TokFunction TokLeftParen type_list TokRightParen TokArrow type_denoter
				{ let pos = union_pos $1 (snd $6) in
				  halt_if_not_pasqual pos "function-type";
				    type_pointer pos (type_fun pos (fst $3) (fst $6)), pos
				}

function_heading:
      TokFunction identifier TokLeftParen TokRightParen TokColon type_denoter
    				{ $2, [], $6 }
    | TokFunction identifier TokColon type_denoter
				{ warn_if_not_pascal (union_pos $1 (snd $4)) "Pascal-style function declaration";
				  $2, [], $4
				}
    | TokFunction identifier TokLeftParen formal_parameters TokRightParen TokColon type_denoter
				{ $2, $4, $7 }
/*    | TokFunction identifier TokLeftParen pascal_formal_parameters TokRightParen TokColon type_denoter
				{ halt_if_not_pascal (union_pos $1 (snd $7)) "Pascal-style function declaration";
				  $2, $4, $7
				}
    | TokFunction identifier TokLeftParen pascal_function_parameters TokRightParen TokColon type_denoter
				{ halt_if_not_pascal (union_pos $1 (snd $7)) "Pascal-style function declaration";
				  $2, $4, $7
				}
*/
operator_heading:
      TokOperator operator_symbol TokLeftParen formal_parameters TokRightParen TokColon type_denoter
    				{ $2, $4, $7 }
    | TokOperator operator_symbol TokLeftParen formal_parameters TokRightParen
				{ let pos = union_pos $1 $5 in
				    $2, $4, (type_void pos, pos)
				}

formal_parameters:
      formal_parameters TokSemi value_parameter_set
    				{ $1 @ $3 }
    | formal_parameters TokSemi var_parameter_set
				{ $1 @ $3 }
    | value_parameter_set	{ $1 }
    | var_parameter_set		{ $1 }

value_parameter_set:
      var_pattern_list TokColon parameter_type_denoter
    				{ List.map (fun (patt, pos) -> pos, patt, fst $3) $1 }

var_parameter_set:
      TokVar value_parameter_set
    				{ List.map (fun (pos, patt, ty) -> pos, patt, type_mutable pos ty) $2 }
    | TokMutable value_parameter_set
				{ List.map (fun (pos, patt, ty) -> pos, patt, type_mutable pos ty) $2 }

/*
pascal_formal_parameters:
      pascal_formal_parameters TokSemi pascal_value_parameter_set
    				{ $1 @ $3 }
    | pascal_formal_parameters TokSemi pascal_var_parameter_set
				{ $1 @ $3 }
    | pascal_value_parameter_set{ $1 }
    | pascal_var_parameter_set	{ $1 }

pascal_value_parameter_set:
    | pascal_function_parameters TokSemi plain_identifier_list TokColon parameter_type_denoter
    				{ let res = List.map (fun (sym, pos) ->
				    pos, VarPattern (pos, sym, sym, None), fst $5) $3 in
					$1 @ res
				}
    | plain_identifier_list TokColon parameter_type_denoter TokSemi pascal_function_parameters
    				{ let res = List.map (fun (sym, pos) ->
				    pos, VarPattern (pos, sym, sym, None), fst $3) $1 in
					$5 @ res
				}

pascal_var_parameter_set:
      TokVar pascal_value_parameter_set
    				{ List.map (fun (pos, patt, ty) -> pos, patt, type_mutable pos ty) $2 }
    | TokMutable pascal_value_parameter_set
				{ List.map (fun (pos, patt, ty) -> pos, patt, type_mutable pos ty) $2 }

pascal_function_parameters:
    | TokFunction plain_identifier_list TokColon parameter_type_denoter
				{ List.map (fun (sym, pos) ->
				    pos, VarPattern (pos, sym, sym, None), TypeFun (pos, StatusNormal, [], fst $4)) $2
				}
    | TokFunction plain_identifier_list TokLeftParen pascal_formal_parameters TokRightParen TokColon parameter_type_denoter
				{ let ty_list = List.map (fun (_, _, ty) -> ty) $4 in
				    List.map (fun (sym, pos) ->
					pos, VarPattern (pos, sym, sym, None), TypeFun (pos, StatusNormal, ty_list, fst $7)) $2
				}
*/
function_body:
      body			{ $1 }

procedure_type:
      TokProcedure TokLeftParen TokRightParen
    				{ let pos = union_pos $1 $3 in
				  halt_if_not_pasqual pos "procedure-type";
    				    type_pointer pos (type_fun pos [] (type_void pos)), pos
				}
    | TokProcedure TokLeftParen type_list TokRightParen
				{ let pos = union_pos $1 $4 in
				  halt_if_not_pasqual pos "procedure-type";
    				    type_pointer pos (type_fun pos (fst $3) (type_void pos)), pos
				}

procedure_heading:
      TokProcedure identifier TokLeftParen TokRightParen
    				{ $2, [] }
    | TokProcedure identifier	{ warn_if_not_pascal (union_pos $1 (snd $2)) "warning: Pascal-style function declaration, use -pascal-compat\n";
				  $2, []
				}
    | TokProcedure identifier TokLeftParen formal_parameters TokRightParen
				{ $2, $4 }

procedure_body:
      body			{ $1 }

statement_body:
      statement			{ wrap_expr $1 }
    | statements		{ SeqExpr (snd $1, wrap_expr_list (snd $1) (fst $1)) }

statements:
      TokBegin statement_list TokEnd
    				{ fst $2, union_pos $1 $3 }
    | TokBegin statement_list TokSemi TokEnd
    				{ fst $2, union_pos $1 $4 }
    | TokBegin TokEnd		{ [], union_pos $1 $2 }

statement_list:
      statement_list TokSemi statement
    				{ (fst $1) @ [$3], union_pos (snd $1) (pos_of_expr $3) }
    | statement			{ [$1], pos_of_expr $1 }

statement:
      unsigned_constant 	{ $1 }
    | case_statement 		{ $1 }
    | repeat_statement 		{ $1 }
    | if_statement 		{ $1 }
    | try_statement 		{ $1 }
    | raise_statement		{ $1 }
    | while_statement 		{ $1 }
    | for_statement 		{ $1 }
    | break_statement 		{ $1 }
    | continue_statement 	{ $1 }
    | goto_statement 		{ $1 }
    | return_statement		{ halt_if_not_pasqual (pos_of_expr $1) "\"return\""; $1 }
    | label_statement		{ $1 }
    | with_statement		{ $1 }
    | assignment_statement	{ $1 }
    | TokSemi			{ void_expr $1 }

assignment_statement:
      primary TokAssignEq expression
				{ make_assign_expr (union_pos (pos_of_expr $1) (pos_of_expr $3)) $1 $3 }
/*    | let_statement		{ $1 }

let_statement:
      TokLet struct_pattern TokAssignEq expression
				{ make_matched_vardecl (fst $2) $4 (union_pos $1 (pos_of_expr $4)) }
    | TokLet struct_pattern TokAssignEq TokLeftParen relop_expression_list_as_list TokRightParen
				{ make_matched_vardecl_aggregate (fst $2) $5 (union_pos $1 $6) }
*/
nonempty_statement:
      unsigned_constant 	{ $1 }
    | case_statement 		{ $1 }
    | repeat_statement 		{ $1 }
    | if_statement 		{ $1 }
    | try_statement 		{ $1 }
    | raise_statement		{ $1 }
    | while_statement 		{ $1 }
    | for_statement 		{ $1 }
    | break_statement 		{ $1 }
    | continue_statement 	{ $1 }
    | goto_statement 		{ $1 }
    | primary TokAssignEq expression
				{ make_assign_expr (union_pos (pos_of_expr $1) (pos_of_expr $3)) $1 $3 }
    | return_statement		{ halt_if_not_pasqual (pos_of_expr $1) "\"return\""; $1 }
    | label_statement		{ $1 }

label_statement:
      identifier TokColon	{ LabelExpr (union_pos (snd $1) $2, fst $1) }
    | TokInt TokColon		{ LabelExpr (union_pos (snd $1) $2, goto_symbol_of_int $1) }
    | identifier TokColon nonempty_statement
				{ SeqExpr (union_pos (snd $1) (pos_of_expr $3),
				    LabelExpr (union_pos (snd $1) $2, fst $1)
				    :: [$3])
				}
    | TokInt TokColon nonempty_statement
				{ SeqExpr (union_pos (snd $1) (pos_of_expr $3),
				    LabelExpr (union_pos (snd $1) $2, goto_symbol_of_int $1)
				    :: [$3])
				}

case_statement:
      TokCase expression TokOf case_element_list TokEnd
				{ make_case (union_pos $1 $5) $2 $4 None }
    | TokCase expression TokOf case_element_list TokElse statement_body TokEnd
				{ make_case (union_pos $1 $7) $2 $4 (Some $6) }

case_element_list:
      case_element_list_rev	{ List.rev $1 }

case_element_list_rev:
      case_element_list_rev case_element
    				{ $2 :: $1 }
    | case_element		{ [$1] }

case_element:
      constant_expression TokColon statement_body TokSemi
    				{ $1, $3 }

pattern_element_list:
      pattern_element pattern_element_list
    				{ (fst $1) :: (fst $2), union_pos (snd $1) (snd $2) }
    | pattern_element		{ [fst $1], snd $1 }

pattern_element:
    | pattern TokColon statement_body TokSemi
				{ (fst $1, $3), union_pos (snd $1) $4 }

pattern:
      TokChar 			{ CharPattern(snd $1, rawint_of_int (Char.code (fst $1))), snd $1 }
    | TokInt			{ IntPattern(snd $1, fst $1), snd $1 }
    | TokFloat			{ FloatPattern(snd $1, fst $1), snd $1 }
    | TokString 		{ let prec, s, pos = $1 in StringPattern(pos, prec, s), pos }
    | TokId			{ VarPattern(snd $1, fst $1, fst $1, None), snd $1 }
    | TokId TokLeftParen plain_identifier_list TokRightParen
				{ let ids = $1 :: $3 in make_enum_pattern ids, union_pos (snd $1) $4 }

try_statement:
      TokTry statement_body TokExcept pattern_element_list TokEnd
				{ warn_if_not_pasqual (union_pos $1 $5) "\"try\"";
    				  let e_list = List.map (fun (patt, exp) -> patt, [exp] @ [BreakExpr (union_pos $1 $5)]) (fst $4) in
				    TryExpr (union_pos $1 $5, $2, e_list, None)
				}
    | TokTry statement_body TokExcept pattern_element_list TokFinally statement_body
				{ warn_if_not_pasqual (union_pos $1 (pos_of_expr $6)) "\"try\"";
				  let e_list = List.map (fun (patt, exp) ->
				    patt, [exp] @ [BreakExpr (snd $4)]) (fst $4)
				  in
				    TryExpr (union_pos $1 (pos_of_expr $6), $2, e_list, Some $6)
				}

raise_statement:
      TokRaise expression	{ warn_if_not_pasqual (union_pos $1 (pos_of_expr $2)) "\"raise\"";
    				  RaiseExpr (union_pos $1 (pos_of_expr $2), $2)
				}

repeat_statement:
      TokRepeat statement_list TokUntil boolean_expression
    				{ let pos = union_pos $1 (pos_of_expr $4) in
				  let stmts = wrap_expr_list (snd $2) (fst $2) in
    				    DoExpr (pos, SeqExpr (snd $2, stmts), $4)
				}

if_statement:
      TokIf boolean_expression TokThen statement_body %prec prec_ifthen
				{ let pos = union_pos $1 (pos_of_expr $4) in
    				    IfExpr (pos, $2, $4, None)
				}
    | TokIf boolean_expression TokThen statement_body TokElse statement_body %prec prec_ifthenelse
				{ let pos = union_pos $1 (pos_of_expr $6) in
				    IfExpr (pos, $2, $4, Some $6)
				}

while_statement:
      TokWhile boolean_expression TokDo statement_body
				{ let pos = union_pos $1 (pos_of_expr $4) in
    				    WhileExpr (pos, $2, $4)
				}

for_statement:
      TokFor identifier TokAssignEq expression TokTo expression TokDo statement_body
				{ make_for (union_pos $1 (pos_of_expr $8))
				    $2 $4 leOp_sym $6 plusOp_sym $8
				}
    | TokFor identifier TokAssignEq expression TokDownTo expression TokDo statement_body
				{ make_for (union_pos $1 (pos_of_expr $8))
				    $2 $4 geOp_sym $6 minusOp_sym $8
				}

return_statement:
      TokReturn expression	{ ReturnExpr (union_pos $1 (pos_of_expr $2), $2) }
    | TokReturn			{ ReturnExpr ($1, UnitExpr ($1, 1, 0)) }

break_statement:
      TokBreak			{ BreakExpr ($1) }

continue_statement:
      TokContinue 		{ ContinueExpr ($1) }

goto_statement:
      TokGoto identifier	{ GotoExpr (union_pos $1 (snd $2), fst $2) }
    | TokGoto TokInt		{ GotoExpr (union_pos $1 (snd $2), goto_symbol_of_int $2) }

with_statement:
      TokWith expression TokDo statement_body
    				{ let pos = union_pos $1 (pos_of_expr $4) in
				    WithExpr (pos, $2, $4)
				}

array_init_expression:
      TokLeftParen relop_expression_list_as_list TokRightParen
				{ List.map (fun e -> None, InitExpr (pos_of_expr e, e)) $2 }
constant_expression:
      TokLeftTuple constant_relop_expression_list TokRightTuple
    				{ $2 }
    | constant_relop_expression	{ $1 }

constant_relop_expression_list:
      constant_relop_expression TokComma constant_relop_expression_list
    				{ make_binop $1 commaOp_sym $3 }
    | constant_relop_expression TokComma constant_relop_expression
				{ make_binop $1 commaOp_sym $3 }

constant_relop_expression:
      constant_simple_expression relop constant_simple_expression
				{ make_binop $1 (fst $2) $3 }
    | constant_simple_expression
				{ $1 }

constant_simple_expression:
      constant_add_term 	{ $1 }
    | constant_simple_expression logop constant_add_term
				{ make_binop $1 (fst $2) $3 }

constant_add_term:
      constant_term 		{ $1 }
    | constant_add_term addop constant_term
				{ make_binop $1 (fst $2) $3 }

constant_term:
      constant_factor 		{ $1 }
    | constant_term mulop constant_factor
				{ make_binop $1 (fst $2) $3 }

constant_factor:
      TokMinus constant_factor	{ make_unop uMinusOp_sym $2 }
    | constant_exponentiation	{ $1 }

/* Need exponentiation ** operator */
constant_exponentiation:
/*   constant_primary TokStarStar constant_exponentiation */
      constant_primary		{ $1 }

constant_primary:
      TokLeftParen constant_expression TokRightParen
    				{ $2 }
    | constant_unsigned_constant
				{ $1 }
    | TokNot constant_primary	{ make_unop uNotOp_sym $2 }

constant_unsigned_constant:
      TokInt			{ IntExpr (snd $1, fst $1) }
    | character_string		{ let char_list, pos = $1 in
				    StringExpr (pos, NormalPrecision,
					make_string_out_of_char_list char_list)
    				}
    | TokChar			{ CharExpr (snd $1, rawint_of_int (Char.code (fst $1))) }
    | TokFloat 			{ FloatExpr (snd $1, fst $1) }
    | TokString			{ let prec, s, pos = $1 in StringExpr (pos, prec, s) }
    | TokNil			{ UnitExpr ($1, 1, 0) }
    | boolean_value		{ $1 }

boolean_value:
      TokTrue 			{ UnitExpr ($1, 2, 1) }
    | TokFalse 			{ UnitExpr ($1, 2, 0) }

relop:
      TokLe 			{ leOp_sym, $1 }
    | TokLt 			{ ltOp_sym, $1 }
    | TokGe 			{ geOp_sym, $1 }
    | TokGt 			{ gtOp_sym, $1 }
    | TokEq 			{ eqOp_sym, $1 }
    | TokNotEq 			{ notEqOp_sym, $1 }

logop:
      TokAnd 			{ andOp_sym, $1 }
    | TokOr 			{ orOp_sym, $1 }
    | TokXor 			{ xorOp_sym, $1 }
    | TokShl 			{ lShiftOp_sym, $1 }
    | TokShr 			{ rShiftOp_sym, $1 }

addop:
      TokPlus 			{ plusOp_sym, $1 }
    | TokMinus 			{ minusOp_sym, $1 }

mulop:
      TokStar 			{ timesOp_sym, $1 }
    | TokSlash 			{ divOp_sym, $1 }
    | TokMod 			{ modOp_sym, $1 }
    | TokDiv 			{ intDivideOp_sym, $1 }

boolean_expression:
      expression 		{ $1 }

expression_list:
      expression_list_rev 	{ List.rev $1 }

expression_list_rev:
      expression_list_rev TokComma expression
    				{ $3 :: $1 }
    | expression 		{ [$1] }

expression:
      TokLeftTuple relop_expression_list TokRightTuple
    				{ $2 }
    | relop_expression		{ $1 }

relop_expression_list_as_list:
      relop_expression TokComma relop_expression_list_as_list
    				{ [$1] @ $3 }
    | relop_expression TokComma relop_expression
				{ [$1] @ [$3] }

relop_expression_list:
      relop_expression TokComma relop_expression_list
    				{ make_binop $1 commaOp_sym $3 }
    | relop_expression TokComma relop_expression
				{ make_binop $1 commaOp_sym $3 }

relop_expression:
      simple_expression relop simple_expression
    				{ make_binop $1 (fst $2) $3 }
    | simple_expression 	{ $1 }

simple_expression:
      add_term 			{ $1 }
    | simple_expression logop add_term
				{ make_binop $1 (fst $2) $3 }

add_term:
      term 			{ $1 }
    | add_term addop term 	{ make_binop $1 (fst $2) $3 }

term:
      factor 			{ $1 }
    | term mulop factor 	{ make_binop $1 (fst $2) $3 }

factor:
      TokMinus factor 		{ make_unop uMinusOp_sym $2 }
    | exponentiation 		{ $1 }

/* BUG: Need exponentation */
exponentiation:
/*   primary TokStarStar exponentiation */
      primary 			{ $1 }

primary:
      unsigned_constant 	{ $1 }

    | TokNot primary 		{ make_unop uNotOp_sym $2 }
    | TokDeref primary		{ warn_if_not_pasqual (union_pos $1 (pos_of_expr $2)) "\"deref\"";
				    make_unop uDerefOp_sym $2
				}
    | TokRef primary 		{ warn_if_not_pasqual (union_pos $1 (pos_of_expr $2)) "\"ref\"";
				    make_unop uAddrOfOp_sym $2
				}
    | TokAt primary 		{ make_unop uAddrOfOp_sym $2 }

unsigned_constant:
      TokInt 			{ IntExpr (snd $1, fst $1) }
    | TokLeftParen expression TokRightParen
				{ $2 }
    | character_string 		{ let char_list, pos = $1 in
				    StringExpr (pos, NormalPrecision, make_string_out_of_char_list char_list)
				}
    | TokChar 			{ CharExpr (snd $1, rawint_of_int (Char.code (fst $1))) }
    | TokFloat 			{ FloatExpr (snd $1, fst $1) }
    | TokString 		{ let prec, s, pos = $1 in StringExpr (pos, prec, s) }
    | boolean_value 		{ $1 }
    | function_call 		{ $1 }
    | lvalue			{ $1 }

lvalue:
      identifier 		{ VarExpr (snd $1, fst $1, fst $1) }
    | component_variable	{ $1 }
    | identified_variable	{ $1 }

component_variable:
      indexed_variable		{ $1 }
    | field_designator		{ $1 }

indexed_variable:
      lvalue TokLeftBrack expression_list TokRightBrack
				{ make_subscript_expr $1 $3 (union_pos $2 $4)  }

field_designator:
      lvalue TokDot identifier	{ let pos = union_pos (pos_of_expr $1) (snd $3) in
				    ProjectExpr (pos, OpExpr (pos, PreOp, uAddrOfOp_sym, uAddrOfOp_sym, [$1]), fst $3)
				}

identified_variable:
      lvalue TokHat		{ make_unop uDerefOp_sym $1 }

/*
 * Single character handled as TokChar above
 */
character_string:
      character_string_rev	{ List.rev (fst $1), (snd $1) }

character_string_rev:
   character_string_rev TokChar { let chars, pos1 = $1 in
				  let ch, pos2 = $2 in
    				     ch :: chars, union_pos pos1 pos2
				}

function_call:
      unsigned_constant TokLeftParen expression_list TokRightParen
    				{ make_apply_expr (union_pos (pos_of_expr $1) $4) $1 $3 }
    | type_identifier TokLeftParen expression TokRightParen
				{ CastExpr (union_pos (snd $1) $4, fst $1, $3) }
    | TokSizeOf TokLeftParen expression TokRightParen
				{ SizeofExpr (union_pos $1 $4, $3) }
    | TokSizeOf TokLeftParen type_identifier TokRightParen
				{ SizeofType(union_pos $1 $4, fst $3) }
    | unsigned_constant TokLeftParen TokRightParen
				{ warn_if_not_pasqual (union_pos (pos_of_expr $1) $3) "calling function with no parameters";
				  make_apply_expr (union_pos (pos_of_expr $1) $3) $1 []
				}

