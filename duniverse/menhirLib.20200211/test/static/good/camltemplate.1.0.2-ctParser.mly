(* Original file: camltemplate.1.0.2/camltemplate-1.0.2/src/ctParser.mly *)
%{
  (*

    CamlTemplate: A template processor for Objective Caml programs.
    Copyright © 2003, 2004, 2005 Benjamin Geer

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St., 5th Floor, Boston MA 02110-1301
    USA

    In addition, as a special exception, Benjamin Geer gives permission
    to link the code of this program with the Apache HTTP Server (or
    with modified versions of Apache that use the same license as
    Apache), and distribute linked combinations including the two. You
    must obey the GNU General Public License in all respects for all of
    the code used other than Apache. If you modify this file, you may
    extend this exception to your version of the file, but you are not
    obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

  *)

  (* $Id: ctParser.mly,v 1.18 2005-06-08 15:22:09 ben Exp $ *)

  open Printf ;;
  open CtStatement ;;
  open CtExpression ;;
  open CtIdent ;;
  open CtLiteral ;;
  open CtUnop ;;
  open CtBinop ;;
  open CtFunctionCall ;;
  open CtHashLookup ;;
  open CtTemplateTypes ;;
  open CtMacro ;;
  open CtTemplateModel ;;
  open CtParserAux ;;

  (* An ocamlyacc parser for template source code. *)

  (* Convenience function that raises an exception for the position
    in a TEXT token. *)
  let parser_error_for_text (_, end_pos, _) =
    raise (ParserError end_pos) ;;

  (* Convenience function that gets the string from a TEXT token. *)
  let string_for_text (_, _, str) = str ;;
%}

%token <CtSourcePos.source_pos> START
%token <CtSourcePos.source_pos * CtSourcePos.source_pos * string> TEXT
%token <CtSourcePos.source_pos> EXPANSION
%token <CtSourcePos.source_pos> IF
%token <CtSourcePos.source_pos> ELSE
%token <CtSourcePos.source_pos> ELSEIF
%token <CtSourcePos.source_pos> FOREACH
%token <CtSourcePos.source_pos> IN
%token <CtSourcePos.source_pos> SET
%token <CtSourcePos.source_pos> VAR
%token <CtSourcePos.source_pos> END
%token <CtSourcePos.source_pos> INCLUDE
%token <CtSourcePos.source_pos> MACRO
%token <CtSourcePos.source_pos> OPEN
%token EOF

%token <CtSourcePos.source_pos * string> STRING
%token <CtSourcePos.source_pos * int> INT
%token <CtSourcePos.source_pos * float> FLOAT
%token <CtSourcePos.source_pos> TRUE
%token <CtSourcePos.source_pos> FALSE
%token <CtSourcePos.source_pos> NULL
%token <CtSourcePos.source_pos * string> IDENT
%token <CtSourcePos.source_pos> LPAREN
%token <CtSourcePos.source_pos> RPAREN
%token <CtSourcePos.source_pos> LBRACKET
%token <CtSourcePos.source_pos> RBRACKET
%token <CtSourcePos.source_pos> PLUS
%token <CtSourcePos.source_pos> MINUS
%token <CtSourcePos.source_pos> TIMES
%token <CtSourcePos.source_pos> DIV
%token <CtSourcePos.source_pos> MOD
%token <CtSourcePos.source_pos> EQUALS_EQUALS
%token <CtSourcePos.source_pos> NOT_EQUALS
%token <CtSourcePos.source_pos> LESS
%token <CtSourcePos.source_pos> GREATER
%token <CtSourcePos.source_pos> LESS_OR_EQUAL
%token <CtSourcePos.source_pos> GREATER_OR_EQUAL
%token <CtSourcePos.source_pos> AND
%token <CtSourcePos.source_pos> OR
%token <CtSourcePos.source_pos> NOT
%token <CtSourcePos.source_pos> DOT
%token <CtSourcePos.source_pos> COMMA
%token <CtSourcePos.source_pos> EQUALS

%left EQUALS
%left COMMA
%left OR
%left AND
%left EQUALS_EQUALS NOT_EQUALS
%left LESS GREATER LESS_OR_EQUAL GREATER_OR_EQUAL
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc NOT UMINUS
%left DOT

%start input
%type <CtStatement.statement list> input

%%

input:
    EOF { [] }
  | START stmts EOF { $2 }
  | START error { raise (ParserError $1) }
  | stmts EOF { $1 }
;

stmts:
    stmt { [$1] }
  | stmt stmts { $1 :: $2 }
;

stmt:
    EXPANSION expr { as_statement (new expansion_statement ~expr:$2) }
  | EXPANSION error { raise (ParserError $1) }
  | IF if_chain else_part END
      { as_statement (new if_statement ~cond_branches:$2 ~else_statements:$3) }
  | IF error { raise (ParserError $1) }
  | FOREACH LPAREN ident IN expr RPAREN stmts END
      { as_statement (new for_each_statement ~index:$3 ~list_expr:$5 ~statements:$7) }
  | FOREACH error { raise (ParserError $1) }
  | SET LPAREN ident EQUALS expr RPAREN
      { as_statement (new set_statement ~left:$3 ~right:$5) }
  | SET error { raise (ParserError $1) }
  | VAR LPAREN ident EQUALS expr RPAREN
      { as_statement (new var_statement ~left:$3 ~right:(Some $5)) }
  | VAR LPAREN ident RPAREN
      { as_statement (new var_statement ~left:$3 ~right:None) }
  | VAR error { raise (ParserError $1) }
  | TEXT { as_statement (new text_statement ~text:(string_for_text $1)) }
  | TEXT error { parser_error_for_text $1 }
  | INCLUDE LPAREN expr RPAREN
      { as_statement (new include_statement ~pos:$1 ~template_name:$3) }
  | INCLUDE error { raise (ParserError $1) }
  | MACRO ident macro_arg_names stmts END
      { add_macro_for_current_template (new macro_impl
                                          ~template_name:(!current_template_name)
                                          ~pos:$1
                                          ~macro_name:$2#get_name
                                          ~arg_names:$3
                                          ~statements:$4);
        as_statement (new null_statement) }
  | MACRO error { raise (ParserError $1) }
  | OPEN LPAREN expr RPAREN
      { as_statement (new open_statement ~pos:$1 ~template_name:$3)  }
  | OPEN error { raise (ParserError $1) }
  | ident function_args
      { as_statement (new macro_call_statement
                        ~pos:($1#get_pos)
                        ~macro_name:$1#get_name
                        ~args:$2) }
  | IDENT error { raise (ParserError (fst $1)) }
;

if_chain:
    LPAREN expr RPAREN stmts { [($2, $4)] }
  | LPAREN expr RPAREN stmts ELSEIF if_chain { ($2, $4) :: $6 }
  | LPAREN expr RPAREN stmts ELSEIF error { raise (ParserError $5) }
;

else_part:
  /* empty */ { [] }
  | ELSE stmts { $2 }
  | ELSE error { raise (ParserError $1) }
;

macro_arg_names:
    LPAREN RPAREN { [] }
  | LPAREN ident_list RPAREN { $2 }
;

ident_list:
    ident { [$1] }
  | ident COMMA ident_list { $1 :: $3 }
  | ident COMMA error { raise (ParserError $2) }
;

ident:
    IDENT { new ident ~pos:(fst $1) ~name:(snd $1) }
  | IDENT error { raise (ParserError (fst $1)) }
;

expr:
    ident { as_expression $1 }
  | INT { as_expression (new literal ~pos:(fst $1) ~value:(Tint (snd $1))) }
  | STRING { as_expression (new literal ~pos:(fst $1) ~value:(Tstr (snd $1))) }
  | FLOAT { as_expression (new literal ~pos:(fst $1) ~value:(Tfloat (snd $1))) }
  | TRUE { as_expression (new literal ~pos:$1 ~value:(Tbool true)) }
  | FALSE { as_expression (new literal ~pos:$1 ~value:(Tbool false)) }
  | NULL { as_expression (new literal ~pos:$1 ~value:(Tnull)) }
  | function_call { $1 }
  | dot_lookup { $1 }
  | bracket_lookup { $1 }
  | NOT expr { as_expression (new not_op ~pos:$1 ~arg:$2) }
  | MINUS expr %prec UMINUS { as_expression (new negative_op ~pos:$1 ~arg:$2) }
  | expr PLUS expr { as_expression (new plus_op ~pos:$2 ~left:$1 ~right:$3) }
  | expr MINUS expr { as_expression (new minus_op ~pos:$2 ~left:$1 ~right:$3) }
  | expr TIMES expr { as_expression (new times_op ~pos:$2 ~left:$1 ~right:$3) }
  | expr DIV expr { as_expression (new div_op ~pos:$2 ~left:$1 ~right:$3) }
  | expr MOD expr { as_expression (new mod_op ~pos:$2 ~left:$1 ~right:$3) }
  | expr EQUALS_EQUALS expr { as_expression (new equals_equals_op ~pos:$2 ~left:$1 ~right:$3) }
  | expr NOT_EQUALS expr { as_expression (new not_equals_op ~pos:$2 ~left:$1 ~right:$3) }
  | expr LESS expr { as_expression (new less_op ~pos:$2 ~left:$1 ~right:$3) }
  | expr GREATER expr { as_expression (new greater_op ~pos:$2 ~left:$1 ~right:$3) }
  | expr LESS_OR_EQUAL expr { as_expression (new less_or_equal_op ~pos:$2 ~left:$1 ~right:$3) }
  | expr GREATER_OR_EQUAL expr { as_expression (new greater_or_equal_op ~pos:$2 ~left:$1 ~right:$3) }
  | expr AND expr { as_expression (new and_op ~pos:$2 ~left:$1 ~right:$3) }
  | expr OR expr { as_expression (new or_op ~pos:$2 ~left:$1 ~right:$3) }
  | LPAREN expr RPAREN { $2 }
  | LPAREN error { raise (ParserError $1) }
;

function_call:
    ident function_args { as_expression (new function_call_op
                                           ~pos:($1#get_pos)
                                           ~left:(as_expression $1)
                                           ~params:$2) }
  | dot_lookup function_args { as_expression (new function_call_op
                                                 ~pos:($1#get_pos)
                                                 ~left:$1
                                                 ~params:$2) }
;

function_args:
    LPAREN RPAREN { [] }
  | LPAREN error { raise (ParserError $1) }
  | LPAREN expr_list RPAREN { $2 }
;

expr_list:
    expr { [$1] }
  | expr COMMA expr_list { $1 :: $3 }
  | expr COMMA error { raise (ParserError $2) }
;

dot_lookup:
    ident DOT ident { as_expression (new dot_op
                                       ~pos:($3#get_pos)
                                       ~left:(as_expression $1)
                                       ~key:$3) }
  | function_call DOT ident { as_expression (new dot_op
                                             ~pos:($3#get_pos)
                                             ~left:$1
                                             ~key:$3) }
;

bracket_lookup:
    ident LBRACKET expr RBRACKET { as_expression (new bracket_op
                                                    ~pos:$2
                                                    ~left:(as_expression $1)
                                                    ~key:$3) }
  | bracket_lookup LBRACKET expr RBRACKET { as_expression (new bracket_op
                                                            ~pos:$2
                                                            ~left:$1
                                                            ~key:$3) }
  | function_call LBRACKET expr RBRACKET { as_expression (new bracket_op
                                                            ~pos:$2
                                                            ~left:$1
                                                            ~key:$3) }
;
