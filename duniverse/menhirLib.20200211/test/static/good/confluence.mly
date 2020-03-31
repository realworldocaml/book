/*
    Confluence System Design Language Compiler
    Copyright (C) 2003-2005 Tom Hawkins (tomahawkins@yahoo.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
*/


%{
(* Pre Code *)
let parse_error s =
  CfParserUtil.error ("Unexpected token: '" ^ (CfParserUtil.get_current_token ()) ^ "'");;
%}

%token <Loc.loc * string> IDENTIFIER

%token <Loc.loc * Intbig.intbig> INTEGER
%token <Loc.loc * float>  FLOAT
%token <Loc.loc * string> STRING
%token <Loc.loc * bool>   BOOLEAN
%token <Loc.loc * string> VECTOR
%token <Loc.loc * string> FREE

%token <Loc.loc * string> END
%token <Loc.loc * string> COMP
%token <Loc.loc * string> COMPONENT
%token <Loc.loc * string> PRIM
%token <Loc.loc * string> IF
%token <Loc.loc * string> EF
%token <Loc.loc * string> ELSE
%token <Loc.loc * string> WITH
%token <Loc.loc * string> IS
%token <Loc.loc * string> LOCAL
%token <Loc.loc * string> IMPORT
%token <Loc.loc * string> ENVIRONMENT
%token <Loc.loc * string> ROOTENVIRONMENT
%token <Loc.loc * string> BRACE_LEFT
%token <Loc.loc * string> BRACE_RIGHT
%token <Loc.loc * string> PAREN_LEFT
%token <Loc.loc * string> PAREN_RIGHT
%token <Loc.loc * string> BRACKET_LEFT
%token <Loc.loc * string> BRACKET_RIGHT
%token <Loc.loc * string> DOLLAR
%token <Loc.loc * string> COLON
%token <Loc.loc * string> EOF
%token <Loc.loc * string> LexerError

/* Operators */
%token <Loc.loc * string> OP_UNIFY
%token <Loc.loc * string> OP_CONNECT_LEFT
%token <Loc.loc * string> OP_CONNECT_RIGHT
%token <Loc.loc * string> OP_THEN OP_VEC_THEN OP_VEC_ELSE
%token <Loc.loc * string> OP_PROP_U
%token <Loc.loc * string> OP_PROP_IMPLY
%token <Loc.loc * string> OP_PROP_EQUIV
%token <Loc.loc * string> OP_OR  OP_PROP_OR
%token <Loc.loc * string>        OP_PROP_XOR
%token <Loc.loc * string> OP_AND OP_PROP_AND
%token <Loc.loc * string> OP_BW_OR OP_VEC_OR
%token <Loc.loc * string> OP_BW_XOR OP_VEC_XOR
%token <Loc.loc * string> OP_BW_AND OP_VEC_AND
%token <Loc.loc * string> OP_EQU OP_NEQ OP_VEC_EQU OP_VEC_NEQ
%token <Loc.loc * string> OP_CONCAT OP_RETURN OP_VEC_CONCAT
%token <Loc.loc * string> OP_CONS
%token <Loc.loc * string> OP_REPEAT OP_VEC_REPEAT
%token <Loc.loc * string> OP_LT OP_GT OP_LE OP_GE OP_VEC_LT OP_VEC_GT OP_VEC_LE OP_VEC_GE OP_VEC_LT_S OP_VEC_GT_S OP_VEC_LE_S OP_VEC_GE_S
%token <Loc.loc * string> OP_LSHIFT OP_RSHIFT OP_VEC_LSHIFT OP_VEC_RSHIFT OP_VEC_RSHIFT_S
%token <Loc.loc * string> OP_ADD OP_SUB OP_VEC_ADD OP_VEC_SUB
%token <Loc.loc * string> OP_MUL OP_DIV OP_MOD OP_VEC_MUL OP_VEC_DIV OP_VEC_MOD OP_VEC_MUL_S
%token <Loc.loc * string> OP_POW OP_VEC_POW
%token <Loc.loc * string> OP_NOT OP_BW_NOT OP_HEAD OP_TAIL OP_LENGTH OP_WIDTH OP_VEC_NOT OP_VEC_MSB OP_VEC_MSBS OP_VEC_LSB OP_VEC_LSBS OP_PROP_NOT OP_PROP_X
%token <Loc.loc * string> OP_DOT OP_VEC_SELECT

%right    OP_UNIFY
%right    OP_CONNECT_LEFT
%left     OP_CONNECT_RIGHT
%right    OP_THEN ELSE OP_VEC_THEN OP_VEC_ELSE
%left     OP_PROP_U
%left     OP_PROP_IMPLY
%left     OP_PROP_EQUIV
%left     OP_OR OP_PROP_OR
%left           OP_PROP_XOR
%left     OP_AND OP_PROP_AND
%left     OP_BW_OR OP_VEC_OR
%left     OP_BW_XOR OP_VEC_XOR
%left     OP_BW_AND OP_VEC_AND
%left     OP_EQU OP_NEQ OP_VEC_EQU OP_VEC_NEQ
%right    OP_CONCAT OP_RETURN OP_VEC_CONCAT
%right    OP_CONS
%left     OP_REPEAT OP_VEC_REPEAT
%left     OP_LT OP_GT OP_LE OP_GE OP_VEC_LT OP_VEC_GT OP_VEC_LE OP_VEC_GE OP_VEC_LT_S OP_VEC_GT_S OP_VEC_LE_S OP_VEC_GE_S
%left     OP_LSHIFT OP_RSHIFT OP_VEC_LSHIFT OP_VEC_RSHIFT OP_VEC_RSHIFT_S
%left     OP_ADD OP_SUB OP_VEC_ADD OP_VEC_SUB
%left     OP_MUL OP_DIV OP_MOD OP_VEC_MUL OP_VEC_DIV OP_VEC_MOD OP_VEC_MUL_S
%right    OP_POW OP_VEC_POW
%nonassoc OP_NOT OP_BW_NOT OP_HEAD OP_TAIL OP_LENGTH OP_WIDTH OP_VEC_NOT OP_VEC_MSB OP_VEC_MSBS OP_VEC_LSB OP_VEC_LSBS OP_PROP_NOT OP_PROP_X
%left     OP_DOT OP_VEC_SELECT


%start file
%type <CfAst.expr> file

%%

file
  :                        file_application EOF
    {
      CfParserUtil.set_env "";
      (snd $1)
    }
  | ENVIRONMENT     STRING file_application EOF
    {
      CfParserUtil.set_env (CfParserUtil.format_string (snd $2));
      (snd $3)
    }
  | ROOTENVIRONMENT        file_application EOF
    {
      (snd $2)
    }
  ;

file_application
  : component_ports IS statements    { ($1, CfAst.Apply (CfParserUtil.current_file_loc (), "", CfAst.Comp (CfParserUtil.current_file_loc (), "", $1, $3), CfParserUtil.free_arg_list (CfParserUtil.current_file_loc ()) $1)) }
  | component_ports name_space_sugar { ($1, CfAst.Apply (CfParserUtil.current_file_loc (), "", CfAst.Comp (CfParserUtil.current_file_loc (), "", $1, $2), CfParserUtil.free_arg_list (CfParserUtil.current_file_loc ()) $1)) }
  ;

statements
  :                       { [] }
  | statements statement  { $1 @ [$2] }
  ;

statement
  : name_space      { CfAst.ApplyStmt $1 }
  | ifelse          { $1 }
  | component_named { $1 }
  | application     { CfAst.ApplyStmt $1 }
  | connect         { CfAst.ConnectStmt $1 }
  ;

name_space
  : LOCAL locals IS statements  END { CfParserUtil.application_expr_of_namespace (fst $1) "" $2 $4 }
  /* XXX annotate namespace */
  ;

name_space_sugar
  : WITH locals IS statements { [CfParserUtil.application_of_namespace (fst $1) "" $2 $4] }
  /* XXX annotate namespace */
  ;

locals
  : local { [$1] }
  | locals local { $1 @ [$2] }
  ;

local
  : IDENTIFIER { snd $1 }
  ;

ifelse
  : IF expression statements ifelse_else       { CfParserUtil.conditional_of_ifelse (fst $1) $2 $3 $4 }
  | IF expression name_space_sugar ifelse_else { CfParserUtil.conditional_of_ifelse (fst $1) $2 $3 $4 }
  ;

ifelse_else
  : END                                         { [] }
  | ELSE statements END                         { $2 }
  | ELSE name_space_sugar END                   { $2 }
  | EF expression statements ifelse_else        { [CfParserUtil.conditional_of_ifelse (fst $1) $2 $3 $4] }
  | EF expression name_space_sugar ifelse_else  { [CfParserUtil.conditional_of_ifelse (fst $1) $2 $3 $4] }
  ;

component_named
  : COMPONENT IDENTIFIER component_ports IS statements    END  { CfParserUtil.stmt_of_comp_named (fst $1) $2 $3 $5 }
  | COMPONENT IDENTIFIER component_ports name_space_sugar END  { CfParserUtil.stmt_of_comp_named (fst $1) $2 $3 $4 }
  ;

expression
  : expression_nonop { $1 }
  | expression_op    { $1 }
  ;

expression_nonop
  : PAREN_LEFT expression PAREN_RIGHT  { $2 }
  | STRING                             { CfParserUtil.application_of_string (fst $1) (CfParserUtil.format_string (snd $1)) }
  | INTEGER                            { CfAst.Integer (fst $1, snd $1) }
  | FLOAT                              { CfAst.Float   (fst $1, snd $1) }
  | BOOLEAN                            { CfAst.Boolean (fst $1, snd $1) }
  | VECTOR                             { CfAst.Vector  (fst $1, snd $1) }
  | IDENTIFIER                         { CfAst.Name (fst $1, snd $1) }
  | FREE                               { CfAst.Free (fst $1) }
  | list_sugar                         { $1 }
  | dot                                { $1 }
  | vector_select                      { $1 }
  | import_file                        { $1 }
  | component_anon                     { $1 }
  | primitive                          { $1 }
  | name_space                         { $1 }
  | application                        { $1 }
  | func                               { $1 }
  | record                             { $1 }
  | nil                                { $1 }
  ;

expression_op
  : connect                            { $1 }
  | prefix                             { $1 }
  | infix                              { $1 }
  | trifix                             { $1 }
  | conditional                        { $1 }
  ;

list_sugar
  : BRACKET_LEFT list_sugar_items BRACKET_RIGHT  { $2 }
  ;

dot
  : expression_nonop OP_DOT IDENTIFIER { CfAst.DotName     (fst $2, $1, snd $3) }
  | expression_nonop OP_DOT INTEGER    { CfAst.DotPosition (fst $2, $1, snd $3) }
  ;

vector_select
  : expression_nonop OP_VEC_SELECT expression_nonop { CfParserUtil.application_of_infix $2 $1 $3 }
  ;

import_file
  : IMPORT STRING { CfAst.Name (fst $1, CfParserUtil.add_import (CfParserUtil.format_string (snd $2))) }
  ;

record
  : PAREN_LEFT record_fields PAREN_RIGHT    { CfAst.Record (fst $1, List.rev $2) }
  | PAREN_LEFT PAREN_RIGHT                  { CfAst.Record (fst $1, []) }
  ;

record_fields
  :               IDENTIFIER COLON expression_nonop  { [(snd $1, $3)] }
  | record_fields IDENTIFIER COLON expression_nonop  { (snd $2, $4) :: $1 }
  ;

nil
  : BRACKET_LEFT BRACKET_RIGHT   { CfAst.Record (fst $1, []) }
  ;

component_anon
  : COMP component_ports IS statements    END  { CfAst.Comp (fst $1, "", $2, $4) }
  | COMP component_ports name_space_sugar END  { CfAst.Comp (fst $1, "", $2, $3) }
  ;

component_ports
  :                                             { [] }
  | component_ports direction_marker IDENTIFIER { $1 @ [snd $3] }
  ;

primitive
  : PRIM IDENTIFIER component_ports END  { CfAst.Prim (fst $1, snd $2, $3) }
  ;

application
  : BRACE_LEFT expression_nonop arguments BRACE_RIGHT
    { CfAst.Apply (fst $1, "", $2, $3) }
  | BRACE_LEFT IDENTIFIER COLON expression_nonop arguments BRACE_RIGHT
    { CfParserUtil.connect_name (fst $3) $2 (CfAst.Apply (fst $1, snd $2, $4, $5)) }
  ;

func
  : BRACE_LEFT expression_nonop arguments direction_marker DOLLAR arguments BRACE_RIGHT
    { CfParserUtil.select_position (fst $5)                                        (CfAst.Apply (fst $1, "", $2, ($3 @ (CfAst.Free (fst $5) :: $6))))  (List.length $3 + 1) }
  | BRACE_LEFT IDENTIFIER COLON expression_nonop arguments direction_marker DOLLAR arguments BRACE_RIGHT
    { CfParserUtil.select_position (fst $7) (CfParserUtil.connect_name (fst $3) $2 (CfAst.Apply (fst $1, snd $2, $4, ($5 @ (CfAst.Free (fst $7) :: $8))))) (List.length $5 + 1) }
  ;

arguments
  :                                             { [] }
  | arguments direction_marker expression_nonop { $1 @ [$3] }
  ;

direction_marker
  :        { () }
  | OP_ADD { () }
  | OP_SUB { () }
  | OP_MUL { () }
  ;

connect
  : expression OP_UNIFY         expression { CfAst.Connect (fst $2, $1, $3) }
  | expression OP_CONNECT_LEFT  expression { CfAst.Connect (fst $2, $1, $3) }
  | expression OP_CONNECT_RIGHT expression { CfAst.Connect (fst $2, $3, $1) }
  ;

prefix
  : OP_NOT         expression { CfParserUtil.application_of_prefix $1 $2 }
  | OP_BW_NOT      expression { CfParserUtil.application_of_prefix $1 $2 }
  | OP_HEAD        expression { CfParserUtil.application_of_prefix $1 $2 }
  | OP_TAIL        expression { CfParserUtil.application_of_prefix $1 $2 }
  | OP_LENGTH      expression { CfParserUtil.application_of_prefix $1 $2 }
  | OP_WIDTH       expression { CfParserUtil.application_of_prefix $1 $2 }
  | OP_VEC_NOT     expression { CfParserUtil.application_of_prefix $1 $2 }
  | OP_VEC_MSB     expression { CfParserUtil.application_of_prefix $1 $2 }
  | OP_VEC_MSBS    expression { CfParserUtil.application_of_prefix $1 $2 }
  | OP_VEC_LSB     expression { CfParserUtil.application_of_prefix $1 $2 }
  | OP_VEC_LSBS    expression { CfParserUtil.application_of_prefix $1 $2 }
  | OP_PROP_NOT    expression { CfParserUtil.application_of_prefix $1 $2 }
  | OP_PROP_X      expression { CfParserUtil.application_of_prefix $1 $2 }
  ;

infix
  : expression OP_BW_OR            expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_OR           expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_BW_XOR           expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_XOR          expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_BW_AND           expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_AND          expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_EQU              expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_NEQ              expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_EQU          expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_NEQ          expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_CONCAT           expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_RETURN           expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_CONCAT       expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_CONS             expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_REPEAT           expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_REPEAT       expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_LT               expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_GT               expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_LE               expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_GE               expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_LT           expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_GT           expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_LE           expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_GE           expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_LT_S         expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_GT_S         expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_LE_S         expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_GE_S         expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_LSHIFT           expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_RSHIFT           expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_LSHIFT       expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_RSHIFT       expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_RSHIFT_S     expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_ADD              expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_SUB              expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_ADD          expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_SUB          expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_MUL              expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_DIV              expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_MOD              expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_MUL          expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_DIV          expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_MOD          expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_MUL_S        expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_POW              expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_VEC_POW          expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_PROP_AND         expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_PROP_XOR         expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_PROP_OR          expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_PROP_EQUIV       expression { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression OP_PROP_IMPLY       expression { CfParserUtil.application_of_infix $2 $1 $3 }
  ;

trifix
  : expression OP_VEC_THEN expression OP_VEC_ELSE  expression { CfParserUtil.application_of_trifix $2 $1 $3 $5 }
  ;

conditional
  : expression OP_AND  expression                     { CfAst.Cond (fst $2, $1, $3, $1) }
  | expression OP_OR   expression                     { CfAst.Cond (fst $2, $1, $1, $3) }
  | expression OP_THEN expression ELSE expression     { CfAst.Cond (fst $2, $1, $3, $5) }
  ;

list_sugar_items
  : expression_nonop                                          { CfParserUtil.application_of_infix (CfAst.expr_loc $1, "::") $1 (CfAst.Record (CfAst.expr_loc $1, [])) }
  | expression_nonop list_sugar_items                         { CfParserUtil.application_of_infix (CfAst.expr_loc $1, "::") $1 $2 }
  | expression_nonop COLON expression_nonop                   { CfParserUtil.application_of_infix $2 $1 $3 }
  | expression_nonop COLON expression_nonop list_sugar_items  { CfParserUtil.application_of_infix (fst $2, "++") (CfParserUtil.application_of_infix $2 $1 $3) $4 }
  ;

%%
(* Post Code *)
