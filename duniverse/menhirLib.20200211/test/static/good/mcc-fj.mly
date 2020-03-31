%{
open Symbol
open Fj_ast
open Fj_ast_util
open Fj_ast_state
open Fj_ast_exn

(*
 * The name of the root class.
 *)
let object_sym = Symbol.add "FjObject"

(*
 * The star symbol is used for parsing import statements
 *)
let star_sym = Symbol.add "*"

(*
 * Var name in a declaration.
 *)
type var_name =
   VarNameId of symbol * pos
 | VarNameArray of var_name * pos
 | VarNameFun of var_name * (symbol * ty) list * pos

let pos_of_var_name = function
   VarNameId (_, pos) -> pos
 | VarNameArray (_, pos) -> pos
 | VarNameFun (_, _, pos) -> pos

(*
 * This is a temporary hack because new arrays do
 * not allow arbitrary types.
 *)
let var_of_var_name = function
   VarNameId (id, pos) ->
      id, pos
 | VarNameArray (_, pos)
 | VarNameFun (_, _, pos) ->
      raise (AstException (pos, StringError "complex array allocations not implemented"))

(*
 * Build a variable name from an id and a nest tree.
 *)
let make_var_name id nest pos =
   let rec collect name = function
      0 -> name
    | i -> collect (VarNameArray (name, pos)) (pred i)
   in
      collect (VarNameId (id, pos)) nest

(*
 * Do the same for types.
 *)
let make_type id nest pos =
   let rec collect ty = function
      0 -> ty
    | i -> collect (TypeArray (ty, pos)) (pred i)
   in
      collect (TypeId (id, pos)) nest

(*
 * Build a variable declaration from the syntax.
 *)
let rec make_var_decl ty = function
   VarNameId (n, pos) ->
      n, ty, pos
 | VarNameArray (v, pos) ->
      make_var_decl (TypeArray (ty, pos)) v
 | VarNameFun (v, args, pos) ->
      make_var_decl (TypeFun (List.map snd args, ty, pos)) v

and make_var_decls ty decls =
   List.map (make_var_decl ty) decls

(*
 * Make a param decl from a var decl.
 *)
let make_param_decls args =
   List.map (fun (v, ty, _) -> v, ty) args

(*
 * Build a variable declaration from the syntax.
 *)
let make_var_init_decls ty defs =
   (* Build the declaration with an initializer *)
   let rec make_def ty e = function
      VarNameId (n, pos) ->
         n, ty, e, pos
    | VarNameArray (v, pos) ->
         make_def (TypeArray (ty, pos)) e v
    | VarNameFun (v, args, pos) ->
         make_def (TypeFun (List.map snd args, ty, pos)) e v
   in

   (* Initial type *)
   let make_init_def (v, e) =
      make_def ty e v
   in
      List.map make_init_def defs

(*
 * A function definition.
 *)
let get_fun_var (v, ty) =
   v, ty, pos_of_type ty

let make_fun_def ty_mods ty decl body pos =
   let pos = union_pos (pos_of_type ty) pos in
   match decl with
      VarNameFun (res, vars, _) ->
         let vars = List.map get_fun_var vars in
         let f, ty, _ = make_var_decl ty res in
            FunDefs ([ty_mods, f, vars, ty, SeqExpr (body, pos), pos], pos)
    | VarNameId _
    | VarNameArray _ ->
         raise (AstException (pos, StringError "not a function"))

(*
 * Constructor definition.
 *)
let make_const_def ty_mods decl body pos =
   match decl with
      VarNameFun (res, vars, _) ->
         let vars = List.map get_fun_var vars in
         let f, _, _ = make_var_decl (TypeVoid pos) res in
            ConstDef (ty_mods, f, vars, SeqExpr (body, pos), pos)
    | VarNameId _
    | VarNameArray _ ->
         raise (AstException (pos, StringError "not a function"))

(*
 * Unary expression.
 *)
let make_unop op expr =
   UnOpExpr (op, expr, pos_of_expr expr)

(*
 * Binary expressions.
 *)
let make_binop op expr1 expr2 =
   let pos = union_pos (pos_of_expr expr1) (pos_of_expr expr2) in
      BinOpExpr (op, expr1, expr2, pos)

let make_boolop op expr1 expr2 =
   let pos = union_pos (pos_of_expr expr1) (pos_of_expr expr2) in
      BoolOpExpr (op, expr1, expr2, pos)

(*
 * Pre and pos increment.
 *)
let make_uarith_op pos op expr =
   let pos = union_pos pos (pos_of_expr expr) in
      UArithExpr (op, expr, pos)

(*
 * Optional expression.
 *)
let make_opt_expr opt_expr def_expr =
   match opt_expr with
      Some expr -> expr
    | None -> def_expr
%}

/*
 * End-of-file is a token.
 */
%token TokEof

/*
 * Binary operators return position.
 */
%token <Fj_ast.pos> TokPlus
%token <Fj_ast.pos> TokMinus
%token <Fj_ast.pos> TokPlusPlus
%token <Fj_ast.pos> TokMinusMinus
%token <Fj_ast.pos> TokStar
%token <Fj_ast.pos> TokSlash
%token <Fj_ast.pos> TokPercent
%token <Fj_ast.pos> TokLAnd
%token <Fj_ast.pos> TokLOr
%token <Fj_ast.pos> TokLsl
%token <Fj_ast.pos> TokLsr
%token <Fj_ast.pos> TokAsr
%token <Fj_ast.pos> TokEq
%token <Fj_ast.pos> TokEqEq
%token <Fj_ast.pos> TokNotEq
%token <Fj_ast.pos> TokLt
%token <Fj_ast.pos> TokLe
%token <Fj_ast.pos> TokGe
%token <Fj_ast.pos> TokGt
%token <Fj_ast.pos> TokAmp
%token <Fj_ast.pos> TokPipe
%token <Fj_ast.pos> TokHat
%token <Fj_ast.binop * Fj_ast.pos> TokAssign

/*
 * Keywords.
 */
%token <Fj_ast.pos> TokAbstract
/* %token <Fj_ast.pos> TokBoolean */
%token <Fj_ast.pos> TokBreak
/* %token <Fj_ast.pos> TokByte */
%token <Fj_ast.pos> TokCase
%token <Fj_ast.pos> TokCatch
/* %token <Fj_ast.pos> TokChar */
%token <Fj_ast.pos> TokClass
%token <Fj_ast.pos> TokConst
%token <Fj_ast.pos> TokContinue
%token <Fj_ast.pos> TokDefault
%token <Fj_ast.pos> TokDo
/* %token <Fj_ast.pos> TokDouble */
%token <Fj_ast.pos> TokElse
%token <Fj_ast.pos> TokExtends
%token <Fj_ast.pos> TokFinal
%token <Fj_ast.pos> TokFinally
/* %token <Fj_ast.pos> TokFloat */
%token <Fj_ast.pos> TokFor
%token <Fj_ast.pos> TokGoto
%token <Fj_ast.pos> TokIf
%token <Fj_ast.pos> TokImplements
%token <Fj_ast.pos> TokImport
%token <Fj_ast.pos> TokInstanceof
/* %token <Fj_ast.pos> TokInt */
%token <Fj_ast.pos> TokInterface
/* %token <Fj_ast.pos> TokLong */
%token <Fj_ast.pos> TokNative
%token <Fj_ast.pos> TokNew
%token <Fj_ast.pos> TokPackage
%token <Fj_ast.pos> TokPrivate
%token <Fj_ast.pos> TokProtected
%token <Fj_ast.pos> TokPublic
%token <Fj_ast.pos> TokReturn
/* %token <Fj_ast.pos> TokShort */
%token <Fj_ast.pos> TokStatic
%token <Fj_ast.pos> TokStrictfp
/* %token <Fj_ast.pos> TokSuper */
%token <Fj_ast.pos> TokSwitch
%token <Fj_ast.pos> TokSynchronized
/* %token <Fj_ast.pos> TokThis */
%token <Fj_ast.pos> TokThrow
%token <Fj_ast.pos> TokThrows
%token <Fj_ast.pos> TokTransient
%token <Fj_ast.pos> TokTry
/* %token <Fj_ast.pos> TokVoid */
%token <Fj_ast.pos> TokVolatile
%token <Fj_ast.pos> TokWhile

%token <Fj_ast.pos> TokNil
%token <Fj_ast.pos> TokTrue
%token <Fj_ast.pos> TokFalse

/* Punctuation */
%token <Fj_ast.pos> TokBang
%token <Fj_ast.pos> TokQuest
%token <Fj_ast.pos> TokColon
%token <Fj_ast.pos> TokSemi
%token <Fj_ast.pos> TokComma
%token <Fj_ast.pos> TokDot
%token <Fj_ast.pos> TokRightArrow
%token <Fj_ast.pos> TokLeftParen
%token <Fj_ast.pos> TokRightParen
%token <Fj_ast.pos> TokLeftBrack
%token <Fj_ast.pos> TokRightBrack
%token <Fj_ast.pos> TokDoubleBrack
%token <Fj_ast.pos> TokLeftBrace
%token <Fj_ast.pos> TokRightBrace

/*
 * Terminal tokens.
 */
%token <Symbol.symbol * Fj_ast.pos> TokId
%token <string * Fj_ast.pos> TokString
%token <char * Fj_ast.pos> TokChar
%token <int * Fj_ast.pos> TokInt
%token <float * Fj_ast.pos> TokFloat

/*
 * Precedences.
 */
%nonassoc TokInstanceof
%left TokComma
%right TokEq
%left TokLOr
%left TokLAnd
%left TokPipe
%left TokHat
%left TokAmp
%left TokEqEq TokNotEq
%left TokLe TokLt TokGe TokGt
%left TokLsl TokLsr TokAsr
%left TokPlus TokMinus
%left TokStar TokSlash TokPercent
%nonassoc TokThrow
%right prec_unary prec_cast TokPlusPlus TokMinusMinus
%left prec_apply prec_subscript TokDot TokLeftParen TokLeftBrack

/* Eliminate the if/then shift/reduce conflict */
%right      TokWhile TokFor
%nonassoc   prec_ifthen
%nonassoc   TokElse prec_ifthenelse
%nonassoc   TokTry
%left       TokCatch TokFinally

/*
 * A complete program.
 */
%start prog
%type <Fj_ast.prog> prog

%%

/************************************************************************
 * TOPLEVEL PRODUCTIONS
 ************************************************************************/

/*
 * A program is a sequence of
 * definitions.
 */
prog:     opt_package_stmt opt_import_list class_or_interface_defs TokEof
          { {  prog_package = $1;
               prog_imports = $2;
               prog_defs    = $3 }
          }
        ;

/************************************************************************
 * PACKAGES AND IMPORTS
 ************************************************************************/

opt_package_stmt:
      /* Empty */
      { None }
    | TokPackage rev_dotted_name TokSemi
      { Some (List.rev $2, union_pos $1 $3) }
      ;

opt_import_list:
      /* Empty */
      { [] }
    | rev_import_list
      { List.rev $1 }
      ;

rev_import_list:
      import_stmt
      { [$1] }
    | rev_import_list import_stmt
      { $2 :: $1 }
      ;

import_stmt:
      TokImport rev_dotted_name TokSemi
      { List.rev $2, union_pos $1 $3 }
    | TokImport rev_dotted_name TokDot TokStar TokSemi
      { List.rev (star_sym :: $2), union_pos $1 $5 }

rev_dotted_name:
      TokId
      { [fst $1] }
    | rev_dotted_name TokDot TokId
      { (fst $3) :: $1 }
      ;


/************************************************************************
 * CLASS AND INTERFACES
 ************************************************************************/

/*
 * Definitions.
 */
class_or_interface_defs:
          rev_class_or_interface_defs
          { List.rev $1 }
        ;

rev_class_or_interface_defs:
          /* empty */
          { [] }
        | rev_class_or_interface_defs class_or_interface_def
          { $2 :: $1 }
        ;

class_or_interface_def:
          rev_type_mod_list class_or_interface_def_content
          { (* Add type modifier information *)
            match $2 with
               TypeDef (_, v, ty, pos) ->
                  TypeDef (List.rev $1, v, ty, pos)
             | ClassDef (_, a, b, c, d, e) ->
                  ClassDef (List.rev $1, a, b, c, d, e)
             | _ ->
                  failwith "Internal error while parsing class/interface def."
          }
        ;

class_or_interface_def_content:
          class_def
          { $1 }
        | interface_def
          { $1 }
        ;

/*
 * An interface definition has an optional type modifier list, an indentifier,
 * an optional list of super-interfaces, and a list of variable declarations.
 */
interface_def:
          TokInterface TokId opt_extends_list
                  TokLeftBrace rev_interface_body TokRightBrace
          { let pos = union_pos $1 $6 in
            let v, _ = $2 in
            let ty = TypeInterface (v, $3, List.rev $5, pos) in
               TypeDef ([], v, ty, pos)
          }
        ;

/*
 * A class def has an optional type modifier list, then the class keyword,
 * then an optional extends modifier,
 * then an optional implements modifier,
 * the the class body.
 */
class_def:
          TokClass TokId opt_extends opt_implements
                  TokLeftBrace class_body TokRightBrace
          { let pos = union_pos $1 $7 in
            let v, _ = $2 in
               ClassDef ([], v, $3, $4, $6, pos)
          }
        ;

opt_extends_list:
          /* empty */
          { [] }
        | TokExtends var_list
          { List.map fst $2 }
        ;

opt_extends:
          /* empty */
          { object_sym }
        | TokExtends TokId
          { fst $2 }
        ;

opt_implements:
          /* empty */
          { [] }
        | TokImplements var_list
          { List.map fst $2 }
        ;

var_list: rev_var_list
          { List.rev $1 }
        ;

rev_var_list:
          TokId
          { [$1] }
        | rev_var_list TokComma TokId
          { $3 :: $1 }
        ;

/*
 * An interface body is just a list
 * of variable declarations.
 */
rev_interface_body:
          /* empty */
          { [] }
        | rev_interface_body rev_var_decl_list TokSemi
          { $2 @ $1 }
        ;

/*
 * A class body has a set of fields.
 */
class_body:
          rev_member_defs
          { List.rev $1 }
        ;

rev_member_defs:
          /* empty */
          { [] }
        | rev_member_defs rev_type_mod_list member_def
          { let ty_mods = List.rev $2 in
            let mem_def = match $3 with
               VarDefs (_, vdl, pos) ->
                  VarDefs (ty_mods, vdl, pos)
             | FunDefs ([_, f, vars, ty, se, pos], pos2) ->
                  FunDefs ([ty_mods, f, vars, ty, se, pos], pos2)
             | ConstDef (_, f, vars, se, pos) ->
                  ConstDef (ty_mods, f, vars, se, pos )
             | _ ->
                  failwith "Internal Error: Unexpected def while parsing"
            in
               mem_def :: $1
          }
        ;

member_def:
          var_defs
          { $1 }
        | fun_def
          { $1 }
        | const_def
          { $1 }
        ;

/************************************************************************
 * DECLARATIONS
 ************************************************************************/

/*
 * Standard type modifiers.
 * We just ignore all of these keywords.
 */
rev_type_mod_list:
          /* empty */
          { [] }
        | rev_type_mod_list type_mod
          { $2 :: $1 }
        ;

type_mod: TokPublic
          { ModPublic $1 }
        | TokProtected
          { ModProtected $1 }
        | TokPrivate
          { ModPrivate $1 }
        | TokStatic
          { ModStatic $1 }
        | TokFinal
          { ModFinal $1 }
        | TokNative
          { ModNative $1 }
        | TokAbstract
          { ModAbstract $1 }
        | TokSynchronized
          { ModSynchronized $1 }
        | TokTransient
          { ModTransient $1 }
        | TokVolatile
          { ModVolatile $1 }
        | TokStrictfp
          { ModStrictfp $1 }
        | TokConst
          { raise (AstException ($1,
               (StringError "'const' is reserved, but not a Java keyword"))) }
        ;

/*
 * A type specifier is just an id followed by some optional brackets.
 */
type_spec:
          id_brackets
          { let id, nest, pos = $1 in
               make_type id nest pos
          }
         ;

/*
 * An identifier followed by some brackets.
 * We have this strange form to prevent shift/reduce
 * conflicts with array subscripting, which requires
 * an expression between the brackets.
 */
id_brackets:
          TokId
          { let id, pos = $1 in
               id, 0, pos
          }
        | TokId brackets
          { let id, pos1 = $1 in
            let nest, pos2 = $2 in
            let pos = union_pos pos1 pos2 in
               id, nest, pos
          }
        ;

brackets:
          TokDoubleBrack
          { 1, $1 }
        | brackets TokDoubleBrack
          { let count, pos1 = $1 in
              succ count, union_pos pos1 $2
          }
        ;

/*
 * Variable declarations.
 * No initializer is allowed.
 */
opt_var_decl_list:
          /* empty */
          { [] }
        | var_decl_list
          { $1 }
        ;

var_decl_list:
          rev_var_decl_list
          { List.rev $1 }
        ;

rev_var_decl_list:
          var_decl
          { [$1] }
        | rev_var_decl_list TokComma var_decl
          { $3 :: $1 }
        ;

var_decl:
          type_spec direct_decl
          { make_var_decl $1 $2 }
        ;

/*
 * Variable definitions.
 */
var_defs:
          type_spec init_def_list TokSemi
          { let pos = union_pos (pos_of_type $1) $3 in
               VarDefs ([], make_var_init_decls $1 $2, pos)
          }
        ;

/*
 * Declarator with an optional initializer.
 */
init_def_list:
          rev_init_def_list
          { List.rev $1 }
        ;

rev_init_def_list:
          init_def
          { [$1] }
        | rev_init_def_list TokComma init_def
          { $3 :: $1 }
        ;

init_def:
          direct_decl
          { $1, None }
        | direct_decl TokEq expr
          { $1, Some $3 }
        ;

/*
 * Declarators without initializers.
 * Again, we have this strange form to prevent conflicts
 * with array subscripting.
 */
direct_decl:
          id_brackets
          { let id, nest, pos = $1 in
               make_var_name id nest pos
          }
        | fun_decl
          { $1 }
        ;

fun_decl:
          id_brackets TokLeftParen opt_var_decl_list TokRightParen
          { let id, nest, pos1 = $1 in
            let name = make_var_name id nest pos1 in
            let pos = union_pos pos1 $4 in
               VarNameFun (name, make_param_decls $3, pos)
          }
        | fun_decl TokDoubleBrack %prec prec_subscript
          { let pos = union_pos (pos_of_var_name $1) $2 in
               VarNameArray ($1, pos)
          }
         /* Optional "throws" list should go in next production */
        | fun_decl TokLeftParen opt_var_decl_list TokRightParen %prec prec_apply
          { let pos = union_pos (pos_of_var_name $1) $4 in
               VarNameFun ($1, make_param_decls $3, pos)
          }
        ;

/*
 * Declare a function.
 */
fun_def:  type_spec direct_decl TokLeftBrace stmt_list
               TokRightBrace
          { make_fun_def [] $1 $2 $4 $5 }
        ;

/*
 * A constructor is like a function,
 * but it has no type.
 */
const_def:
          direct_decl TokLeftBrace stmt_list TokRightBrace
          { make_const_def [] $1 $3 $4 }
        ;

/************************************************************************
 * EXPRESSIONS
 ************************************************************************/

/*
 * Expressions.
 */
expr:     ncast_expr
          { $1 }
        | cast_expr
          { $1 }
        ;

ncast_expr:
          TokInt
          { let i, pos = $1 in
               IntExpr (i, pos)
          }
        | TokFloat
          { let x, pos = $1 in
               FloatExpr (x, pos)
          }
        | TokChar
          { let c, pos = $1 in
               CharExpr (c, pos)
          }
        | TokString
          { let s, pos = $1 in
               StringExpr (s, pos)
          }
        | TokNil
          { NilExpr $1 }
        | TokTrue
          { BoolExpr (true, $1) }
        | TokFalse
          { BoolExpr (false, $1) }
        | TokMinus expr %prec prec_unary
          { make_unop UMinusOp $2 }
        | TokBang expr %prec prec_unary
          { make_unop UNotOp $2 }
        | TokPlusPlus expr %prec prec_unary
          { make_uarith_op $1 PreIncrOp $2 }
        | TokMinusMinus expr %prec prec_unary
          { make_uarith_op $1 PreDecrOp $2 }
        | expr TokPlusPlus %prec prec_unary
          { make_uarith_op $2 PostIncrOp $1 }
        | expr TokMinusMinus %prec prec_unary
          { make_uarith_op $2 PostDecrOp $1 }
        | expr TokPlus expr
          { make_binop PlusOp $1 $3 }
        | expr TokMinus expr
          { make_binop MinusOp $1 $3 }
        | expr TokStar expr
          { make_binop TimesOp $1 $3 }
        | expr TokSlash expr
          { make_binop DivideOp $1 $3 }
        | expr TokPercent expr
          { make_binop ModOp $1 $3 }
        | expr TokLsl expr
          { make_binop LslOp $1 $3 }
        | expr TokLsr expr
          { make_binop LsrOp $1 $3 }
        | expr TokAsr expr
          { make_binop AsrOp $1 $3 }
        | expr TokLAnd expr
          { make_boolop LAndOp $1 $3 }
        | expr TokLOr expr
          { make_boolop LOrOp $1 $3 }
        | expr TokEqEq expr
          { make_binop EqOp $1 $3 }
        | expr TokNotEq expr
          { make_binop NeqOp $1 $3 }
        | expr TokLe expr
          { make_binop LeOp $1 $3 }
        | expr TokLt expr
          { make_binop LtOp $1 $3 }
        | expr TokGe expr
          { make_binop GeOp $1 $3 }
        | expr TokGt expr
          { make_binop GtOp $1 $3 }
        | expr TokEq expr
          { let pos = union_pos (pos_of_expr $1) (pos_of_expr $3) in
               AssignExpr (None, $1, $3, pos)
          }
        | expr binop TokEq expr %prec TokEq
          { let pos = union_pos (pos_of_expr $1) (pos_of_expr $4) in
               AssignExpr (Some $2, $1, $4, pos)
          }
        | expr TokInstanceof TokId
          { let id, pos = $3 in
            let pos = union_pos (pos_of_expr $1) pos in
               InstanceofExpr ($1, id, pos)
          }
        | TokNew new_expr
          { $2 }
        | TokThrow expr
          { let pos = union_pos $1 (pos_of_expr $2) in
               ThrowExpr ($2, pos)
          }
        ;

cast_expr:
          TokId
          { let id, pos = $1 in
               VarExpr (id, pos)
          }
        | cast_expr TokDot TokId
          { let pos = union_pos (pos_of_expr $1) (snd $3) in
               ProjectExpr ($1, fst $3, pos)
          }
        | cast_expr TokLeftBrack expr TokRightBrack %prec prec_subscript
          { let pos = union_pos (pos_of_expr $1) $4 in
               SubscriptExpr ($1, $3, pos)
          }
        | cast_expr TokLeftParen arg_list TokRightParen %prec prec_apply
          { let pos = union_pos (pos_of_expr $1) $4 in
               ApplyExpr ($1, $3, pos)
          }
        | TokLeftParen TokId TokRightParen cast_expr %prec prec_cast
          { let id, pos2 = $2 in
            let ty = make_type id 0 pos2 in
            let pos = union_pos $1 (pos_of_expr $4) in
               CastExpr (ty, $4, pos)
          }
        | TokLeftParen TokId brackets TokRightParen cast_expr %prec prec_cast
          { let id, pos2 = $2 in
            let nest, pos3 = $3 in
            let ty = make_type id nest (union_pos pos2 pos3) in
            let pos = union_pos $1 (pos_of_expr $5) in
               CastExpr (ty, $5, pos)
          }
        | TokLeftParen ncast_expr TokRightParen %prec prec_apply
          { $2 }
        | TokLeftParen TokId TokRightParen %prec prec_apply
          { let id, pos = $2 in
               VarExpr (id, pos)
          }
        ;

/*
 * New is followed by either an array
 * or constructor declaration.  Note that
 * this is more general than what was given
 * in class.
 */
new_expr:
          new_array rev_brackets
          { let id, pos1 = var_of_var_name $1 in
            let dimens, pos2 = $2 in
            let pos = union_pos pos1 pos2 in
               NewArrayExpr (id, List.rev dimens, pos)
          }
        | TokId TokLeftParen rev_arg_list TokRightParen
          { let id, pos1 = $1 in
            let pos = union_pos pos1 $4 in
               NewConstExpr (id, List.rev $3, pos)
          }
        | new_const
          { let id, pos = $1 in
               NewConstExpr (id, [], pos)
          }
        ;

new_array: new_type
           { $1 }
         | TokId
           { let id, pos = $1 in
                VarNameId (id, pos)
           }
         ;

new_type:  new_type TokDoubleBrack
           { let pos = union_pos (pos_of_var_name $1) $2 in
               VarNameArray ($1, pos)
           }
         | new_type TokLeftParen opt_var_decl_list TokRightParen
           { let pos = union_pos (pos_of_var_name $1) $4 in
               VarNameFun ($1, make_param_decls $3, pos)
           }
         | TokId TokDoubleBrack
           { let id, pos1 = $1 in
             let pos = union_pos pos1 $2 in
                VarNameArray (VarNameId (id, pos1), pos)
           }
         | TokId TokLeftParen var_decl_list TokRightParen
           { let id, pos1 = $1 in
             let pos = union_pos pos1 $4 in
                VarNameFun (VarNameId (id, pos1), make_param_decls $3, pos)
           }
         | new_const
           { let id, pos = $1 in
               VarNameFun (VarNameId (id, pos), [], pos)
           }
         ;

new_const: TokId TokLeftParen TokRightParen
           { $1 }
         ;

rev_brackets:
          TokLeftBrack expr TokRightBrack %prec prec_subscript
          { let pos = union_pos $1 $3 in
               [$2], pos
          }
        | rev_brackets TokLeftBrack expr TokRightBrack %prec prec_subscript
          { let args, pos1 = $1 in
            let pos = union_pos pos1 $4 in
               $3 :: args, pos
          }
        ;

/*
 * An optional label
 */
opt_label: /* empty */
         { None }
       | TokId
         { Some (fst $1) }
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
          { SeqExpr ([], $1) }
        | expr TokSemi
          { $1 }
        | TokIf TokLeftParen expr TokRightParen stmt %prec prec_ifthen
          { let pos = union_pos $1 (pos_of_expr $5) in
               IfExpr ($3, $5, None, pos)
          }
        | TokIf TokLeftParen expr TokRightParen stmt TokElse stmt %prec prec_ifthenelse
          { let pos = union_pos $1 (pos_of_expr $7) in
               IfExpr ($3, $5, Some $7, pos)
          }
        | TokFor TokLeftParen opt_expr TokSemi opt_expr TokSemi opt_expr TokRightParen stmt %prec TokFor
          { let pos = union_pos $1 (pos_of_expr $9) in
            let def_expr = IntExpr (1, pos) in
            let init = make_opt_expr $3 def_expr in
            let test = make_opt_expr $5 def_expr in
            let step = make_opt_expr $7 def_expr in
               ForExpr (init, test, step, $9, pos)
          }
        | TokWhile TokLeftParen expr TokRightParen stmt %prec TokWhile
          { let pos = union_pos $1 (pos_of_expr $5) in
               WhileExpr ($3, $5, pos)
          }
        | TokDo stmt TokWhile TokLeftParen expr TokRightParen
          { let pos = union_pos $1 $6 in
               DoExpr ($2, $5, pos)
          }
        | TokReturn expr TokSemi
          { let pos = union_pos $1 (pos_of_expr $2) in
               ReturnExpr ($2, pos)
          }
        | TokBreak opt_label TokSemi
          { let pos = union_pos $1 $3 in
               BreakExpr ($2, pos)
          }
        | TokContinue opt_label TokSemi
          { let pos = union_pos $1 $3 in
               ContinueExpr ($2, pos)
          }
        | TokLeftBrace stmt_list TokRightBrace
          { let pos = union_pos $1 $3 in
               SeqExpr ($2, pos)
          }
        | TokTry TokLeftBrace stmt_list TokRightBrace rev_catches opt_finally %prec TokTry
          { let pos = union_pos $1 $4 in
               TryExpr (SeqExpr ($3, pos), $5, $6, pos)
          }
        | var_defs
          { let pos = pos_of_def $1 in
               DefExpr ($1, pos)
          }
        | fun_def
          { let pos = pos_of_def $1 in
               DefExpr ($1, pos)
          }
        | TokId TokColon stmt
          { let sym, pos = $1 in
            let pos = union_pos (snd $1) (pos_of_expr $3) in
               LabeledExpr (sym, $3, pos)
          }
        ;

rev_catches:
          /* empty */
          { [] }
        | rev_catches TokCatch TokLeftParen TokId TokId TokRightParen TokLeftBrace stmt_list TokRightBrace %prec TokCatch
          { let pos = union_pos $7 $9 in
               (fst $4, fst $5, SeqExpr ($8, pos)) :: $1
          }
        ;

opt_finally:
          /* empty */
          { None }
        | TokFinally TokLeftBrace stmt_list TokRightBrace %prec TokFinally
          { let pos = union_pos $1 $4 in
               Some (SeqExpr ($3, pos))
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
 * Arguments are comma-separated list of expressions.
 */
arg_list: /* empty */
          { [] }
        | rev_arg_list
          { List.rev $1 }
        ;

rev_arg_list:
          expr
          { [$1] }
        | rev_arg_list TokComma expr
          { $3 :: $1 }
        ;

/*
 * binary operator.
 */
binop:    TokPlus
          { PlusOp }
        | TokMinus
          { MinusOp }
        | TokStar
          { TimesOp }
        | TokSlash
          { DivideOp }
        | TokPercent
          { ModOp }
        | TokAmp
          { BAndOp }
        | TokPipe
          { BOrOp }
        | TokHat
          { BXorOp }
        | TokLsl
          { LslOp }
        | TokLsr
          { LsrOp }
        | TokAsr
          { AsrOp }
        ;
