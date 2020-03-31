/*
 * Parser for a simple Pascal grammar.
 *
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
 */

%{
open Symbol
open Pascal_ast_type
open Pascal_ast_util

let make_unop op expr =
   UnOpExpr(op, expr, pos_of_expr expr)

let make_binop expr1 op expr2 =
   let pos = union_pos (pos_of_expr expr1) (pos_of_expr expr2) in
      BinOpExpr(op, expr1, expr2, pos)

let make_string_out_of_char_list charpos_list =
    "not yet implemented"

let rec make_case_statement expr1 case_expr_list else_expr pos =
   match case_expr_list with
      (head_case, head_what) :: l -> IfExpr(BinOpExpr(EqOp, expr1, head_case, pos), head_what, Some (make_case_statement expr1 l else_expr pos), pos)
    | _ -> SeqExpr([else_expr], pos)

%}
%token TokEof

%token <Pascal_ast_type.pos> TokNil
%token <Pascal_ast_type.pos> TokTrue
%token <Pascal_ast_type.pos> TokFalse

%token <Pascal_ast_type.pos> TokPlus
%token <Pascal_ast_type.pos> TokMinus
%token <Pascal_ast_type.pos> TokStar
%token <Pascal_ast_type.pos> TokSlash
%token <Pascal_ast_type.pos> TokLShift
%token <Pascal_ast_type.pos> TokRShift
%token <Pascal_ast_type.pos> TokEq
%token <Pascal_ast_type.pos> TokAssignEq
%token <Pascal_ast_type.pos> TokNotEq
%token <Pascal_ast_type.pos> TokLt
%token <Pascal_ast_type.pos> TokLe
%token <Pascal_ast_type.pos> TokGt
%token <Pascal_ast_type.pos> TokGe
%token <Pascal_ast_type.pos> TokAmp
%token <Pascal_ast_type.pos> TokHat

%token <Pascal_ast_type.pos> TokColon
%token <Pascal_ast_type.pos> TokSemi
%token <Pascal_ast_type.pos> TokComma
%token <Pascal_ast_type.pos> TokDot
%token <Pascal_ast_type.pos> TokTwoDots
%token <Pascal_ast_type.pos> TokLeftParen
%token <Pascal_ast_type.pos> TokRightParen
%token <Pascal_ast_type.pos> TokLeftBrack
%token <Pascal_ast_type.pos> TokRightBrack
%token <Pascal_ast_type.pos> TokLeftBrace
%token <Pascal_ast_type.pos> TokRightBrace

/*
 * Keywords
 */
%token <Pascal_ast_type.pos> TokMod
%token <Pascal_ast_type.pos> TokDiv
%token <Pascal_ast_type.pos> TokAnd
%token <Pascal_ast_type.pos> TokOr
%token <Pascal_ast_type.pos> TokXor
%token <Pascal_ast_type.pos> TokNot
%token <Pascal_ast_type.pos> TokShl
%token <Pascal_ast_type.pos> TokShr

%token <Pascal_ast_type.pos> TokBegin
%token <Pascal_ast_type.pos> TokEnd
%token <Pascal_ast_type.pos> TokBreak
%token <Pascal_ast_type.pos> TokCase

%token <Pascal_ast_type.pos> TokWhile
%token <Pascal_ast_type.pos> TokFor
%token <Pascal_ast_type.pos> TokRepeat
%token <Pascal_ast_type.pos> TokUntil
%token <Pascal_ast_type.pos> TokDo

%token <Pascal_ast_type.pos> TokType
%token <Pascal_ast_type.pos> TokVar
%token <Pascal_ast_type.pos> TokConst
%token <Pascal_ast_type.pos> TokProcedure
%token <Pascal_ast_type.pos> TokFunction
%token <Pascal_ast_type.pos> TokProgram
%token <Pascal_ast_type.pos> TokArray
%token <Pascal_ast_type.pos> TokRecord
%token <Pascal_ast_type.pos> TokUnit
%token <Pascal_ast_type.pos> TokImplementation
%token <Pascal_ast_type.pos> TokInterface
%token <Pascal_ast_type.pos> TokUses

%token <Pascal_ast_type.pos> TokOf

%token <Pascal_ast_type.pos> TokIf
%token <Pascal_ast_type.pos> TokThen
%token <Pascal_ast_type.pos> TokElse

%token <Pascal_ast_type.pos> TokForward
%token <Pascal_ast_type.pos> TokExternal
%token <Pascal_ast_type.pos> TokTo
%token <Pascal_ast_type.pos> TokDownTo

%token <Pascal_ast_type.pos> TokInc
%token <Pascal_ast_type.pos> TokDec

%token <Pascal_ast_type.pos> TokTypeInteger
%token <Pascal_ast_type.pos> TokTypeString
%token <Pascal_ast_type.pos> TokTypeSingle
%token <Pascal_ast_type.pos> TokTypeChar

/*
 * Terminal tokens
 */
%token <int * Pascal_ast_type.pos> TokInt
%token <float * Pascal_ast_type.pos> TokFloat
%token <char * Pascal_ast_type.pos> TokChar
%token <string * Pascal_ast_type.pos> TokString
%token <Symbol.symbol * Pascal_ast_type.pos> TokId

/*
 * Precedences
 */
%left TokComma
%right TokEq
%left TokMod
%left TokNotEq
%left TokLe TokLt TokGe TokGt
%left TokLShift TokRShift
%left TokPlus TokMinus
%left TokStart TokSlash
%left TokAmp
%left TokHat

/*
 * Program
 */
%start console
%type <Pascal_ast_type.prog option> console

%start program
%type <Pascal_ast_type.prog> program

%start unit_body
%type <Pascal_ast_type.prog> unit_body

%type <(symbol * pos) * (Symbol.symbol list * Pascal_ast_type.pos)> unit_header
%type <Pascal_ast_type.expr list> interface_body
%type <Pascal_ast_type.expr list> interface_body_rev
%type <Pascal_ast_type.expr> interface_body_element

%type <Symbol.symbol * Pascal_ast_type.pos * Symbol.symbol list * Pascal_ast_type.pos> program_header
%type <Symbol.symbol * Pascal_ast_type.pos> identifier
%type <Symbol.symbol list * Pascal_ast_type.pos> identifier_list
%type <Symbol.symbol list * Pascal_ast_type.pos> identifier_list_rev
%type <Pascal_ast_type.expr list * Pascal_ast_type.pos> block_list
%type <Pascal_ast_type.expr list * Pascal_ast_type.pos> block_list_rev
%type <Pascal_ast_type.expr * Pascal_ast_type.pos> block
%type <Pascal_ast_type.expr list> body
%type <Pascal_ast_type.expr> type_def
%type <Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos> type_definition
%type <(Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos) list> type_definition_list
%type <(Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos) list> type_definition_list_rev
%type <Pascal_ast_type.ty> type_denoter
%type <Pascal_ast_type.ty> new_type
%type <Pascal_ast_type.ty> new_structured_type
%type <Pascal_ast_type.ty> array_type
%type <(Pascal_ast_type.expr * Pascal_ast_type.expr) list> index_list
%type <(Pascal_ast_type.expr * Pascal_ast_type.expr) list> index_list_rev
%type <Pascal_ast_type.expr * Pascal_ast_type.expr> index
%type <Pascal_ast_type.ty> record_type
%type <(Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos) list> record_section_list
%type <(Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos) list> record_section
%type <Pascal_ast_type.ty> new_pointer_type
%type <Pascal_ast_type.expr> variable_decl
%type <(Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos) list> variable_declaration_list
%type <(Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos) list> variable_declaration

%type <Pascal_ast_type.expr> function_decl

%type <Symbol.symbol * (Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos) list * Pascal_ast_type.ty> function_heading
%type <(Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos) list * Pascal_ast_type.ty * Pascal_ast_type.pos> function_type_heading
%type <(Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos) list> formal_parameters
%type <(Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos) list> value_parameter_set
%type <(Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos) list> var_parameter_set
%type <Pascal_ast_type.pos> directive
%type <Pascal_ast_type.expr list> function_body
%type <Pascal_ast_type.expr> procedure_decl
%type <Symbol.symbol * (Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos) list> procedure_heading
%type <(Symbol.symbol * Pascal_ast_type.ty * Pascal_ast_type.pos) list * Pascal_ast_type.pos> procedure_type_heading
%type <Pascal_ast_type.expr list> procedure_body
%type <Pascal_ast_type.expr> statements
%type <Pascal_ast_type.expr list * Pascal_ast_type.pos> statement_list
%type <Pascal_ast_type.expr list * Pascal_ast_type.pos> statement_list_rev
%type <Pascal_ast_type.expr> statement
%type <Pascal_ast_type.expr> case_statement
%type <(Pascal_ast_type.expr * Pascal_ast_type.expr) list> case_element_list
%type <(Pascal_ast_type.expr * Pascal_ast_type.expr) list> case_element_list_rev
%type <Pascal_ast_type.expr * Pascal_ast_type.expr> case_element
%type <Pascal_ast_type.expr> repeat_statement
%type <Pascal_ast_type.expr> if_statement
%type <Pascal_ast_type.expr> while_statement
%type <Pascal_ast_type.expr> for_statement
%type <Pascal_ast_type.expr> constant_expression
%type <Pascal_ast_type.expr> constant_simple_expression
%type <Pascal_ast_type.expr> constant_add_term
%type <Pascal_ast_type.expr> constant_term
%type <Pascal_ast_type.expr> constant_factor
%type <Pascal_ast_type.expr> constant_exponentiation
%type <Pascal_ast_type.expr> constant_primary
%type <Pascal_ast_type.expr> constant_unsigned_constant
%type <Pascal_ast_type.expr> boolean_value
%type <Pascal_ast_type.binop> relop
%type <Pascal_ast_type.binop> logop
%type <Pascal_ast_type.binop> addop
%type <Pascal_ast_type.binop> mulop
%type <Pascal_ast_type.sign> sign
%type <Pascal_ast_type.expr> boolean_expression
%type <Pascal_ast_type.expr list> expression_list
%type <Pascal_ast_type.expr list> expression_list_rev
%type <Pascal_ast_type.expr> expression
%type <Pascal_ast_type.expr> simple_expression
%type <Pascal_ast_type.expr> add_term
%type <Pascal_ast_type.expr> term
%type <Pascal_ast_type.expr> factor
%type <Pascal_ast_type.expr> exponentiation
%type <Pascal_ast_type.expr> primary
%type <Pascal_ast_type.expr> unsigned_constant
%type <char list * Pascal_ast_type.pos> character_string
%type <char list * Pascal_ast_type.pos> character_string_rev
%type <Pascal_ast_type.expr> function_call
%%

console:
   statement { Some [$1] }
 | program { Some $1 }
 | unit_body { Some $1 }
 | TokEof { None }

program:
   program_header body TokDot { $2 }

unit_body:
   unit_header interface_body implementation_body statements TokDot { ($2 @ $3) @ [$4] }
 | unit_header interface_body implementation_body TokEnd TokDot { $2 @ $3 }

/* returns (sym * pos) * (Symbol.symbol list * Pascal_ast_type.pos) */
unit_header:
   TokUnit identifier TokSemi { $2, ([],union_pos $1 $3) }
 | TokUnit identifier TokSemi TokUses identifier_list TokSemi { $2, $5 }

interface_body:
   TokInterface interface_body_rev { List.rev $2 }

interface_body_rev:
   interface_body_rev interface_body_element { $2 :: $1 }
 | interface_body_element { [$1] }

/* returns expr */
interface_body_element:
   function_heading TokSemi {
      let sym, sym_ty_pos_list, ty = $1 in
         FunDef(sym, sym_ty_pos_list, ty, SeqExpr([], $2), $2)
   }
 | function_heading TokSemi directive TokSemi {
      let sym, sym_ty_pos_list, ty = $1 in
      let pos = union_pos $2 $4 in
         FunDef(sym, sym_ty_pos_list, ty, SeqExpr([], pos), pos)
   }
 | procedure_heading TokSemi {
      let sym, sym_ty_pos_list = $1 in
         FunDef(sym, sym_ty_pos_list, TypeUnit($2), SeqExpr([], $2), $2)
   }
 | procedure_heading TokSemi directive TokSemi {
      let sym, sym_ty_pos_list = $1 in
      let pos = union_pos $2 $4 in
         FunDef(sym, sym_ty_pos_list, TypeUnit($2), SeqExpr([], pos), pos)
   }
 | type_def { $1  }
 | variable_decl { $1 }

implementation_body:
   TokImplementation block_list {
      let expr_list, pos = $2 in
         expr_list
   }

program_header:
   TokProgram identifier TokSemi {
      let sym, pos = $2 in
         sym, pos, [], $3
   }
 | TokProgram identifier TokLeftParen identifier_list TokRightParen TokSemi {
      let sym, pos = $2 in
      let syms, pos = $4 in
         sym, pos, syms, pos
   }

/* returns sym * pos */
identifier:
   TokId { $1 }

identifier_list:
   identifier_list_rev {
      let syms, pos = $1 in
         List.rev syms, pos
   }

/* returns symbol list * pos */
identifier_list_rev:
   identifier_list_rev TokComma identifier {
      let syms, pos1 = $1 in
      let sym, pos2 = $3 in
         sym :: syms, union_pos pos1 pos2
   }
 | identifier {
      let sym, pos = $1 in
        [sym], pos
   }

block_list:
   block_list_rev {
      let syms, pos = $1 in
         List.rev syms, pos
   }

/* returns expr list, pos */
block_list_rev:
   block_list_rev block {
      let exps1, pos1 = $1 in
      let exp2, pos2 = $2 in
         exp2 :: exps1, union_pos pos1 pos2
   }
 | block {
      let exp, pos = $1 in
         [exp], pos
   }

/* returns expr * pos */
block:
   type_def { $1, pos_of_expr $1 }
 | variable_decl { $1, pos_of_expr $1 }
 | function_decl { $1, pos_of_expr $1 }
 | procedure_decl { $1, pos_of_expr $1 }


/* returns expr list instead of the old expr (SeqExpr) */
body:
   block_list statements {
      let exps, pos = $1 in
         exps @ [$2]
   }
 | statements { [$1] }

type_def:
   TokType type_definition_list { TypeDefs($2, $1) }

type_definition_list:
   type_definition_list_rev { List.rev $1 }

/* (symbol * ty * pos) list */
type_definition_list_rev:
   type_definition_list_rev type_definition { $2 :: $1 }
 | type_definition { [$1] }

/* symbol * ty * pos */
type_definition:
   identifier TokEq type_denoter TokSemi {
      let sym, pos = $1 in
         sym, $3, union_pos pos $4
   }

/* returns ty */
type_denoter:
   TokTypeInteger { TypeInt($1) }
 | TokTypeString  { TypeArray(TypeChar($1), [IntExpr(0, $1), IntExpr(255, $1)], $1) }
 | TokTypeSingle  { TypeFloat($1) }
 | TokTypeChar    { TypeChar($1) }
 | identifier {
      let sym, pos = $1 in
         TypeVar(sym, pos)
   }
 | new_type { $1 }
 | function_type_heading {
      (* bogus position .... *)
      let sym_ty_pos_list, ty, pos = $1 in
      let ty_list = List.map (fun (sym, ty, pos) -> ty) sym_ty_pos_list in
         TypeFun(ty_list, ty, pos)
   }
 | procedure_type_heading {
      let sym_ty_pos_list, pos = $1 in
      let ty_list = List.map (fun (sym, ty, pos) -> ty) sym_ty_pos_list in
         TypeFun(ty_list, TypeUnit(pos), pos)
   }

new_type:
   new_structured_type { $1 }
 | new_pointer_type { $1 }

new_structured_type:
   array_type { $1 }
 | record_type { $1 }

array_type:
   TokArray TokLeftBrack index_list TokRightBrack TokOf type_denoter {
      TypeArray($6, $3, union_pos $1 $5)
   }

index_list:
   index_list_rev { List.rev $1 }

/* returns (expr * expr) list */
index_list_rev:
   index_list_rev TokComma index { $3 :: $1 }
 | index { [$1] }

/* returns expr * expr */
index:
   TokInt TokTwoDots TokInt {
      let int1, pos1 = $1 in
      let int2, pos2 = $3 in
         IntExpr(int1, pos1), IntExpr(int2, pos2)
   }

record_type:
   TokRecord record_section_list TokSemi TokEnd {
      TypeStruct($2, union_pos $1 $4)
   }

/* returns (symbol * ty * pos) list */
record_section_list:
   record_section_list TokSemi record_section { $1 @ $3 }
 | record_section { $1 }

/* returns (symbol * ty * pos) list */
record_section:
   identifier_list TokColon type_denoter {
      let syms, pos = $1 in
      let res = List.map (fun sym -> sym, $3, pos) syms in
         res
   }

new_pointer_type:
   TokHat type_denoter {
     TypePointer($2, union_pos $1 (pos_of_type $2))
   }

/* returns expr (VarDefs) */
variable_decl:
   TokVar variable_declaration_list TokSemi {
      let vars = List.map (fun (sym, ty, pos) -> sym, ty, None, pos) $2 in
         VarDefs(vars, union_pos $1 $3)
   }

/* returns (symbol * ty * pos) list */
variable_declaration_list:
   variable_declaration_list TokSemi variable_declaration { $1 @ $3 }
 | variable_declaration { $1 }

/* returns (symbol * ty * pos) list */
variable_declaration:
   identifier_list TokColon type_denoter {
      let ty = $3 in
      let syms, pos = $1 in
      let res = List.map (fun sym -> (sym, ty, pos)) syms in
         res
   }

/* returns symbol * (symbol * ty * pos) list * ty */
function_heading:
   TokFunction identifier TokColon type_denoter {
      let sym, pos = $2 in
      let typ = $4 in
         sym, [], typ
   }
 | TokFunction identifier TokLeftParen formal_parameters TokRightParen TokColon type_denoter {
      let sym, pos = $2 in
      let symtypos_list = $4 in
         sym, symtypos_list, $7
   }

/* returns (symbol * ty * pos) list * ty * pos */
function_type_heading:
   TokFunction TokColon type_denoter { [], $3, union_pos $1 $2 }
 | TokFunction TokLeftParen formal_parameters TokRightParen TokColon type_denoter { $3, $6, union_pos $1 $5 }

formal_parameters:
 | formal_parameters TokSemi value_parameter_set { $1 @ $3 }
 | formal_parameters TokSemi var_parameter_set { $1 @ $3 }
 | value_parameter_set { $1 }
 | var_parameter_set { $1 }

/* returns (symbol * ty * pos) list
value_parameter_set_list:
   value_parameter_set_list TokSemi value_parameter_set { $1 @ $3 }
 | value_parameter_set { $1 }
*/
/* returns (symbol * ty * pos) list */
value_parameter_set:
   identifier_list TokColon type_denoter {
      let syms, pos = $1 in
      let ty = $3 in
      let syms = List.map (fun sym -> sym, ty, pos) syms in
         syms
   }

/* (symbol * ty * pos) list, where ty is TypeReference(ty,..)
var_parameter_set_list:
   var_parameter_set_list TokSemi var_parameter_set { $1 @ $2 }
 | var_parameter_set { $1 }
*/
/* (symbol * ty * pos) list, where ty is TypeReference(ty,..) */
var_parameter_set:
   TokVar value_parameter_set {
      let param_set = List.map (fun (sym, ty, pos) -> sym, TypeReference(ty, pos), pos) $2 in
         param_set
   }

directive:
   TokForward { $1 }
 | TokExternal { $1 }

/* returns expr */
function_decl:
   function_heading TokSemi directive TokSemi {
      let sym, params, typ = $1 in
         FunDef(sym, params, typ, SeqExpr([], $4), $4)
   }
/* bogus position for now */
 | function_heading TokSemi function_body TokSemi {
      let pos = $2 in
      let sym, params, ty = $1 in
         FunDef(sym, params, ty, SeqExpr($3, pos), $2)
   }


/* returns expr list, instead of expr */
function_body:
   body { $1 }

/* returns expr (FunDef) */
procedure_decl:
   procedure_heading TokSemi directive TokSemi {
     let name, params = $1 in
     let pos = $4 in
     let typ = TypeUnit(pos) in
        FunDef(name, params, typ, SeqExpr([], pos), pos)
   }
 | procedure_heading TokSemi procedure_body TokSemi {
     let name, params = $1 in
     let pos = $2 in
     let typ = TypeUnit(pos) in
        FunDef(name, params, typ, SeqExpr($3, pos), pos)
   }

/* returns sym * (sym * ty * pos) list */
procedure_heading:
   TokProcedure identifier {
      let sym, pos = $2 in
         sym, []
   }
 | TokProcedure identifier TokLeftParen formal_parameters TokRightParen {
      let sym, pos = $2 in
         sym, $4
   }

/* returns (sym * ty * pos) list */
procedure_type_heading:
   TokProcedure identifier {
      let sym, pos = $2 in
         [], union_pos $1 pos
   }
 | TokProcedure TokLeftParen formal_parameters TokRightParen { $3, union_pos $1 $4 }

/* returns expr list, instead of expr */
procedure_body:
   body { $1 }

/* returns expr (SeqExpr) */
statements:
   TokBegin statement_list TokEnd {
      let exprs, pos = $2 in
         SeqExpr(exprs, pos)
   }

statement_list:
   statement_list_rev {
      let explist, pos = $1 in
         List.rev explist, pos
   }

/* returns expr list * pos */
statement_list_rev:
   statement_list_rev statement {
      let sts1, pos1 = $1 in
      let pos = union_pos pos1 (pos_of_expr $2) in
         $2 :: sts1, pos
   }
 | statement {
      let pos = pos_of_expr $1 in
         [$1], pos
   }

/* returns expr */
statement:
/*   identifier TokSemi {
      let sym, pos = $1 in
         VarExpr(sym, union_pos pos $2)
   }
 | identifier TokLeftParen expression_list TokRightParen TokSemi {
      let sym, pos = $1 in
      let pos = union_pos pos $4 in
         ApplyExpr (VarExpr(sym, pos), $3, pos)
   }*/
 | case_statement { $1 }
 | repeat_statement { $1 }
 | if_statement { $1 }
 | while_statement { $1 }
 | for_statement { $1 }
 | unsigned_constant TokAssignEq expression TokSemi {
      AssignExpr(None, $1, $3, union_pos (pos_of_expr $1) $4)
   }
 | unsigned_constant TokHat TokAssignEq expression TokSemi {
      AssignExpr(None, make_unop UHatOp $1, $4, union_pos (pos_of_expr $1) $5)
   }
 | expression TokSemi { $1 }

case_statement:
   TokCase expression TokOf case_element_list TokEnd TokSemi {
      let pos = union_pos $1 $6 in
      let else_expr = SeqExpr([], pos) in
      let exp = make_case_statement $2 $4 else_expr pos in
      match exp with
         SeqExpr([],_) -> raise (ParseError (pos, "No case specified in 'case' statement"))
       | _ -> exp
   }
 | TokCase expression TokOf case_element_list TokElse statement TokEnd TokSemi {
      let pos = union_pos $1 $8 in
      let else_expr = $6 in
      let exp = make_case_statement $2 $4 else_expr pos in
      match exp with
         SeqExpr([],_) -> raise (ParseError (pos, "No case specified in 'case' statement"))
       | _ -> exp
   }

case_element_list:
   case_element_list_rev { List.rev $1 }

case_element_list_rev:
   case_element_list_rev case_element { $2 :: $1 }
 | case_element { [$1] }

case_element:
   constant_expression TokColon statement { $1, $3 }
 | constant_expression TokColon statements TokSemi { $1, $3 }

repeat_statement:
   TokRepeat statement_list TokUntil boolean_expression TokSemi {
      let stmts, pos = $2 in
      let pos = union_pos $1 (pos_of_expr $4) in
         SeqExpr(WhileExpr($4, SeqExpr(stmts, pos), pos) :: stmts, pos)
   }

if_statement:
   TokIf boolean_expression TokThen statements TokSemi {
      let pos = union_pos $1 $5 in
         IfExpr($2, $4, None, pos)
   }
 | TokIf boolean_expression TokThen statements TokElse statements TokSemi {
      let pos = union_pos $1 $7 in
         IfExpr($2, $4, Some $6, pos)
   }

while_statement:
   TokWhile boolean_expression TokDo statement TokSemi {
      let pos = union_pos $1 $5 in
         WhileExpr($2, $4, pos)
   }
 | TokWhile boolean_expression TokDo statements TokSemi {
      let pos = union_pos $1 $5 in
         WhileExpr($2, $4, pos)
   }

for_statement:
   TokFor identifier TokAssignEq expression TokTo expression TokDo statement TokSemi {
      let pos = union_pos $1 $9 in
      let default_expr = IntExpr(1, pos) in
      let sym, p = $2 in
      let init = AssignExpr(None, VarExpr(sym,p), $4, pos) in
      let test = BinOpExpr(LeOp, VarExpr(sym, p), $6, pos) in
      let step = AssignExpr(Some PlusOp, VarExpr(sym, p), IntExpr(1, pos), pos) in
         ForExpr(init, test, step, $8, pos)
   }
 | TokFor identifier TokAssignEq expression TokTo expression TokDo statements TokSemi {
      let pos = union_pos $1 $9 in
      let default_expr = IntExpr(1, pos) in
      let sym, p = $2 in
      let init = AssignExpr(None, VarExpr(sym, p), $4, pos) in
      let test = BinOpExpr(LeOp, VarExpr(sym, p), $6, pos) in
      let step = AssignExpr(Some PlusOp, VarExpr(sym, p), IntExpr(1, pos), pos) in
         ForExpr(init, test, step, $8, pos)
   }
 | TokFor identifier TokAssignEq expression TokDownTo expression TokDo statement TokSemi {
      let pos = union_pos $1 $9 in
      let default_expr = IntExpr(1, pos) in
      let sym, p = $2 in
      let init = AssignExpr(None, VarExpr(sym, p), $4, pos) in
      let test = BinOpExpr(GeOp, VarExpr(sym, p), $6, pos) in
      let step = AssignExpr(Some MinusOp, VarExpr(sym, p), IntExpr(1, pos), pos) in
         ForExpr(init, test, step, $8, pos)
   }
 | TokFor identifier TokAssignEq expression TokDownTo expression TokDo statements TokSemi {
      let pos = union_pos $1 $9 in
      let default_expr = IntExpr(1, pos) in
      let sym, p = $2 in
      let init = AssignExpr(None, VarExpr(sym, p), $4, pos) in
      let test = BinOpExpr(GeOp, VarExpr(sym, p), $6, pos) in
      let step = AssignExpr(Some MinusOp, VarExpr(sym, p), IntExpr(1, pos), pos) in
         ForExpr(init, test, step, $8, pos)
   }

constant_expression:
   constant_simple_expression relop constant_simple_expression { make_binop $1 $2 $3 }
 | constant_simple_expression { $1 }

constant_simple_expression:
   constant_add_term { $1 }
 | constant_simple_expression logop constant_add_term { make_binop $1 $2 $3 }

constant_add_term:
   constant_term { $1 }
 | constant_add_term addop constant_term { make_binop $1 $2 $3 }

constant_term:
   constant_factor { $1 }
 | constant_term mulop constant_factor { make_binop $1 $2 $3 }

constant_factor:
   sign constant_factor {
      match $1 with
         SiMinus -> make_unop UMinusOp $2
       | SiPlus -> $2
   }
 | constant_exponentiation { $1 }

/* Need exponentiation ** operator */
constant_exponentiation:
/*   constant_primary TokStarStar constant_exponentiation */
   constant_primary { $1 }

constant_primary:
   TokLeftParen constant_expression TokRightParen { $2 }
 | constant_unsigned_constant { $1 }
 | TokNot constant_primary { make_unop UNotOp $2 }

constant_unsigned_constant:
   TokInt {
      let i, pos = $1 in
         IntExpr(i, pos)
   }
 | character_string {
      let char_list, pos = $1 in
      StringExpr(make_string_out_of_char_list char_list, pos)
   }
 | TokChar {
      let c, pos = $1 in
         CharExpr(c, pos)
   }
 | TokFloat {
      let f, pos = $1 in
         FloatExpr(f, pos)
   }
 | TokString {
      let s, pos = $1 in
         StringExpr(s, pos)
   }
 | TokNil { IntExpr(0, $1) }
 | boolean_value { $1 }

boolean_value:
   TokTrue { IntExpr(1, $1) }
 | TokFalse { IntExpr(0, $1) }

relop:
   TokLe { LeOp }
 | TokLt { LtOp }
 | TokGe { GeOp }
 | TokGt { GtOp }
 | TokEq { EqOp }
 | TokNotEq { NotEqOp }

logop:
   TokAnd { AndOp }
 | TokOr { OrOp }
 | TokXor { XorOp }
 | TokShl { LShiftOp }
 | TokShr { RShiftOp }

addop:
   TokPlus { PlusOp }
 | TokMinus { MinusOp }

mulop:
   TokStar { TimesOp }
 | TokSlash { DivideOp }
 | TokMod { ModOp }
 | TokDiv { IntDivideOp }

sign:
   TokMinus { SiMinus }
 | TokPlus { SiPlus }

boolean_expression:
   expression { $1 }

expression_list:
   expression_list_rev { List.rev $1 }

/* returns expr list */
expression_list_rev:
   expression_list_rev TokComma expression { $3 :: $1 }
 | expression { [$1] }

/* returns expr */
expression:
   simple_expression relop simple_expression { make_binop $1 $2 $3 }
 | simple_expression { $1 }

simple_expression:
   add_term { $1 }
 | simple_expression logop add_term { make_binop $1 $2 $3 }

add_term:
   term { $1 }
 | add_term addop term { make_binop $1 $2 $3 }

term:
   factor { $1 }
 | term mulop factor { make_binop $1 $2 $3 }

factor:
   sign factor {
      match $1 with
         SiMinus -> make_unop UMinusOp $2
       | SiPlus -> $2
   }
 | exponentiation { $1 }

/* Need exponentation */
exponentiation:
/*   primary TokStarStar exponentiation */
   primary { $1 }

primary:
   TokLeftParen expression TokRightParen { $2 }
 | unsigned_constant { $1 }
 | TokNot primary { make_unop UMinusOp $2 }

/*
 * function calls and variable accesses are both!
 * represented as VarExpr, then later resolved
 * correctly.
 */
unsigned_constant:
   TokInt {
      let i, pos = $1 in
         IntExpr(i, pos)
   }
 | character_string {
      let char_list, pos = $1 in
      StringExpr(make_string_out_of_char_list char_list, pos)
   }
 | TokChar {
      let c, pos = $1 in
         CharExpr(c, pos)
   }
 | TokFloat {
      let f, pos = $1 in
         FloatExpr(f, pos)
   }
 | TokString {
      let s, pos = $1 in
         StringExpr(s, pos)
   }
 | TokNil { IntExpr(0, $1) }
 | boolean_value { $1 }
 | TokAmp identifier {
      let sym, pos = $2 in
         AddrOfExpr(VarExpr(sym, pos), pos)
   }
 | unsigned_constant TokHat { make_unop UHatOp $1 }
 | identifier {
      let v, pos = $1 in
         VarExpr(v, pos)
   }
 | identifier TokDot identifier {
      let sym1, pos1 = $1 in
      let sym2, pos2 = $3 in
      let pos = union_pos pos1 pos2 in
         ProjectExpr(VarExpr(sym1, pos1), sym2, pos)
   }
 | TokInc TokLeftParen identifier TokRightParen {
      let sym, pos = $3 in
         UArithExpr(PreIncrOp, VarExpr(sym, pos), pos)
   }
 | TokDec TokLeftParen identifier TokRightParen {
      let sym, pos = $3 in
         UArithExpr(PreDecrOp, VarExpr(sym, pos), pos)
   }
 | TokInc TokLeftParen identifier TokComma expression TokRightParen {
      let sym, pos = $3 in
         AssignExpr(Some PlusOp, VarExpr(sym, pos), $5, pos)
   }
 | TokDec TokLeftParen identifier TokComma expression TokRightParen {
      let sym, pos = $3 in
         AssignExpr(Some MinusOp, VarExpr(sym, pos), $5, pos)
   }
 | function_call { $1 }

/*
 * Single character handled as TokChar above
 */
/* char list * pos */
character_string:
   character_string_rev {
      let chars, pos = $1 in
         List.rev chars, pos
   }

character_string_rev:
   character_string_rev TokChar {
      let chars, pos1 = $1 in
	      let ch, pos2 = $2 in
         ch :: chars, union_pos pos1 pos2
   }

/* expr */
function_call:
   identifier TokLeftParen expression_list TokRightParen {
      let sym, pos1 = $1 in
      let pos = union_pos pos1 $4 in
         ApplyExpr(VarExpr(sym, pos1), $3, pos)
   }
