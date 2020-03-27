/*
 * Parser for OMakefiles.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2007 Jason Hickey, Caltech
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
open Lm_printf
open Lm_symbol
open Lm_location

open Omake_pos
open Omake_ast
open Omake_symbol
open Omake_ast_util
open Omake_value_type

module Pos = MakePos (struct let name = "Omake_parse" end)
open Pos;;

(*
 * Define flags.
 *)
let define_flag (s, loc) =
   match s with
      "=" -> DefineNormal
    | "+=" -> DefineAppend
    | _ ->
       raise (OmakeException (loc_exp_pos loc, StringStringError ("undefined assignment operator", s)))

(*
 * Convert arguments to parameters.
 *)
let key_of_id s =
   Lm_symbol.add (String.sub s 1 (String.length s - 1))

let parse_id_param s loc =
   match s.[0] with
      '?' -> OptionalParam (key_of_id s, NullExp loc, loc)
    | '~' -> RequiredParam (key_of_id s, loc)
    | _   -> NormalParam (Lm_symbol.add s, loc)

let param_of_arg arg =
   match arg with
      IdArg (s, _, loc) ->
        parse_id_param s loc
    | NormalArg (KeyArg (v, e)) ->
        OptionalParam (v, e, loc_of_exp e)
    | NormalArg (ExpArg e) ->
        raise (OmakeException (loc_exp_pos (loc_of_exp e), StringAstError ("illegal function parameter", e)))
    | NormalArg (ArrowArg (_, e)) ->
        raise (OmakeException (loc_exp_pos (loc_of_exp e), StringAstError ("illegal function argument", e)))

let get_fun_params args =
   List.map param_of_arg args

(*
 * Remove the IdArg.
 *)
let arg_of_parse_arg = function
   IdArg (s, w, loc1) ->
      let id = StringIdExp (s, loc1) in
      let e =
         match w with
             Some (w, loc2) ->
                SequenceExp ([id; StringWhiteExp (w, loc2)], loc1)
           | None ->
                id
      in
          ExpArg e
 | NormalArg arg ->
      arg

let args_of_parse_args = List.map arg_of_parse_arg

(*
 * Utilities.
 *)
let rec simplify e =
   match e with
      SequenceExp ([e], _) ->
         simplify e
    | _ ->
         e

let sequence_exp l loc =
   match l with
      [e] ->
         e
    | _ ->
         SequenceExp (l, loc)


(*
 * Intern the method name.
 *)
let method_id_intern idl =
   List.map Lm_symbol.add idl

(*
 * Get a string from a method name.
 *)
let method_id_string idl =
   let buf = Buffer.create 32 in
   let rec collect idl =
      match idl with
         [id] ->
            Buffer.add_string buf id
       | id :: idl ->
            Buffer.add_string buf id;
            Buffer.add_char buf '.';
            collect idl
       | [] ->
            ()
   in
      collect idl;
      Buffer.contents buf

let rec method_id_rev_sequence loc items idl =
   match idl with
      [id] ->
         (StringIdExp (id, loc)) :: items
    | id :: idl ->
         let items = StringOpExp (".", loc) :: StringIdExp (id, loc) :: items in
            method_id_rev_sequence loc items idl
    | [] ->
         items

let method_id_sequence loc idl =
   List.rev (method_id_rev_sequence loc [] idl)

let method_id_string_exp idl loc =
   SequenceExp (method_id_sequence loc idl, loc)

let method_id_prefix_string_exp idl loc =
   let idl = List.rev (method_id_rev_sequence loc [StringOpExp (".", loc)] idl) in
      SequenceExp (idl, loc)

let var_quote (strategy, s, loc) =
   KeyExp (strategy, s, loc), loc

(*
 * Convert to a body flag and text.
 *)
let get_optcolon_text opt loc =
   match opt with
      None ->
         OptBody, NullExp loc
    | Some (body, arg) ->
         body, arg
(*
 * A 3-place rule.
 *)
let rule3 multiple (target, loc1) ploc pattern source loc2 body =
   let loc = union_loc loc1 loc2 in
      match pattern with
         Some (pattern, _) ->
            RuleExp (multiple, target, pattern, source, body, loc)
       | None ->
            RuleExp (multiple, target, NullExp loc2, source, body, loc)

let rule2 multiple target ploc source loc2 body =
   rule3 multiple target ploc None source loc2 body
%}

/*
 * Terminators
 */
%token <Lm_location.loc> TokEof
%token <Lm_location.loc> TokEol

/*
 * Whitespace.
 */
%token <string * Lm_location.loc> TokWhite

/*
 * Characters.
 */
%token <string * Lm_location.loc> TokLeftParen
%token <string * Lm_location.loc> TokRightParen
%token <string * Lm_location.loc> TokArrow
%token <string * Lm_location.loc> TokComma
%token <string * Lm_location.loc> TokColon
%token <string * Lm_location.loc> TokDoubleColon
%token <string * Lm_location.loc> TokNamedColon
%token <string * Omake_ast.apply_strategy * Lm_location.loc> TokDollar
%token <string * Lm_location.loc> TokEq
%token <string * Lm_location.loc> TokArray
%token <string * Lm_location.loc> TokDot

/*
 * Words.
 */
%token <string * Lm_location.loc> TokId
%token <string * Lm_location.loc> TokKey
%token <string * Lm_location.loc> TokKeyword
%token <string * Lm_location.loc> TokCatch
%token <string * Lm_location.loc> TokClass
%token <string * Lm_location.loc> TokOp
%token <string * Lm_location.loc> TokInt
%token <string * Lm_location.loc> TokFloat
%token <string * Lm_location.loc> TokString
%token <string * Lm_location.loc> TokBeginQuote
%token <string * Lm_location.loc> TokEndQuote
%token <string * Lm_location.loc> TokBeginQuoteString
%token <string * Lm_location.loc> TokEndQuoteString
%token <string * Lm_location.loc> TokStringQuote
%token <Omake_ast.apply_strategy * string * Lm_location.loc> TokVar
%token <Omake_ast.apply_strategy * string * Lm_location.loc> TokVarQuote

/*
 * A complete program.
 */
%start deps
%type <(Omake_ast.exp * Omake_ast.exp * Lm_location.loc) list> deps

%start shell
%start string
%type <Omake_ast.body_flag * Omake_ast.exp> shell
%type <Omake_ast.body_flag * Omake_ast.exp> string

%%

/*
 * A string is just some text.
 */
string:
          TokEof
          { raise End_of_file }
        | text TokEol TokEof
          { NoBody, sequence_exp $1 $2 }
        ;

/*
 * Commands in a shell.
 * Bodies are usually not allowed.
 */
shell:    TokEof
          { raise End_of_file }
        | shell_line TokEof
          { $1 }
        ;

shell_line:
          /* Blank lines */
          opt_white TokEol
          { NoBody, sequence_exp [] $2 }

          /* Builtin functions */
        | TokKeyword TokWhite keyword_text_optcolon TokEol
          { let id, loc1 = $1 in
            let body, arg = get_optcolon_text $3 $4 in
            let loc = union_loc loc1 $4 in
               body, CommandExp (Lm_symbol.add id, arg, [], loc)
          }
        | TokKeyword opt_literal_colon TokEol
          { let id, loc1 = $1 in
            let body = $2 in
            let loc = union_loc loc1 $3 in
            let arg = NullExp loc in
               body, CommandExp (Lm_symbol.add id, arg, [], loc)
          }

          /* Keyword applications */
        | TokKeyword opt_white TokLeftParen opt_args TokRightParen opt_colon TokEol
          { let id, loc1 = $1 in
            let body = $6 in
            let loc = union_loc loc1 $7 in
	    let args = args_of_parse_args $4 in
            let e = ApplyExp (CommandApply, Lm_symbol.add id, args, loc) in
               body, e
          }

          /* Catch expression is special */
        | TokCatch opt_white TokId opt_white TokLeftParen opt_white TokId opt_white TokRightParen opt_colon TokEol
          { let _, loc1 = $1 in
            let loc = union_loc loc1 $11 in
            let name, _ = $3 in
            let v, _ = $7 in
               $10, CatchExp (Lm_symbol.add name, Lm_symbol.add v, [], loc)
          }

          /* Class expression is special */
        | TokClass opt_id_list TokEol
          { let _, loc1 = $1 in
            let loc = union_loc loc1 $3 in
               NoBody, ClassExp (List.map Lm_symbol.add $2, loc)
          }

          /* Variable definition with a body */
        | method_id_opt_white TokEq opt_white TokEol
          { let id, loc1 = $1 in
            let loc2 = $4 in
            let id = method_id_intern id in
            let loc = union_loc loc1 loc2 in
            let add_flag = define_flag $2 in
               ColonBody, VarDefBodyExp (id, DefineString, add_flag, [], loc)
          }

          /* Object definition with a body */
        | method_id_prefix_opt_white TokEq opt_white TokEol
          { let id, loc1 = $1 in
            let loc2 = $4 in
            let id = method_id_intern id in
            let loc = union_loc loc1 loc2 in
            let add_flag = define_flag $2 in
               ColonBody, ObjectDefExp (id, add_flag, [], loc)
          }

          /* Variable definition on one line */
        | method_id_opt_white TokEq opt_white text_nonempty TokEol
          { let id, loc1 = $1 in
            let loc2 = $5 in
            let e = simplify $4 in
            let id = method_id_intern id in
            let loc = union_loc loc1 loc2 in
            let add_flag = define_flag $2 in
               NoBody, VarDefExp (id, DefineString, add_flag, e, loc)
          }

          /* Key definition with a body */
        | var_quote_opt_white TokEq opt_white TokEol
          { let _, id, loc1 = $1 in
            let loc2 = $4 in
            let loc = union_loc loc1 loc2 in
            let add_flag = define_flag $2 in
               ColonBody, KeyDefBodyExp (id, DefineString, add_flag, [], loc)
          }

          /* Key definition on one line */
        | var_quote_opt_white TokEq opt_white text_nonempty TokEol
          { let _, id, loc1 = $1 in
            let loc2 = $5 in
            let e = simplify $4 in
            let loc = union_loc loc1 loc2 in
            let add_flag = define_flag $2 in
               NoBody, KeyDefExp (id, DefineString, add_flag, e, loc)
          }

          /* Array definition */
        | method_id_opt_white TokArray opt_white TokEq opt_white TokEol
          { let id, loc1 = $1 in
            let loc2 = $6 in
            let id = method_id_intern id in
            let loc = union_loc loc1 loc2 in
            let add_flag = define_flag $4 in
               ArrayBody, VarDefBodyExp (id, DefineArray, add_flag, [], loc)
          }

          /* Array definition on one line */
        | method_id_opt_white TokArray opt_white TokEq opt_white text_nonempty TokEol
          { let id, loc1 = $1 in
            let loc2 = $7 in
            let id = method_id_intern id in
            let loc = union_loc loc1 loc2 in
            let add_flag = define_flag $4 in
               NoBody, VarDefExp (id, DefineArray, add_flag, $6, loc)
          }

          /* Applications that use parens may also have a body */
        | method_id_opt_white TokLeftParen opt_args TokRightParen opt_colon TokEol
          { let id, loc1 = $1 in
            let body = $5 in
            let loc = union_loc loc1 $6 in
            let args = args_of_parse_args $3 in
            let e =
               match id with
                  [id] ->
                     ApplyExp (CommandApply, Lm_symbol.add id, args, loc)
                | _ ->
                     MethodApplyExp (CommandApply, method_id_intern id, args, loc)
            in
               body, e
          }

          /* Applications with all binding vars */
        | method_id_opt_white TokLeftParen opt_args TokRightParen opt_colon TokArrow opt_white TokEol
          { let id, loc1 = $1 in
            let body = $5 in
            let loc = union_loc loc1 $8 in
            let params = get_fun_params $3 in
            let arg = ArrowArg (params, StringOpExp ("...", loc)) in
            let e =
               match id with
                  [id] ->
                     ApplyExp (CommandApply, Lm_symbol.add id, [arg], loc)
                | _ ->
                     MethodApplyExp (CommandApply, method_id_intern id, [arg], loc)
            in
               body, e
          }

          /* Function definition */
        | method_id_opt_white TokLeftParen opt_args TokRightParen opt_white TokEq opt_white TokEol
          { let id, loc1 = $1 in
            let id = method_id_intern id in
            let params = get_fun_params $3 in
            let loc = union_loc loc1 $8 in
               ColonBody, FunDefExp (id, params, [], loc)
          }

          /* 2-place rule definition that starts with a name */
        | other_id_target TokColon source TokEol
          { ColonBody, rule2 false $1 $2 $3 $4 [] }
        | other_id_target TokColon target TokColon source TokEol
          { ColonBody, rule3 false $1 $2 $3 $5 $6 [] }
        | other_target TokColon source TokEol
          { ColonBody, rule2 false $1 $2 $3 $4 [] }
        | other_target TokDoubleColon source TokEol
          { ColonBody, rule2 true $1 $2 $3 $4 [] }
        | other_target TokColon target TokColon source TokEol
          { ColonBody, rule3 false $1 $2 $3 $5 $6 [] }
        | other_target TokDoubleColon target TokColon source TokEol
          { ColonBody, rule3 true $1 $2 $3 $5 $6 [] }

          /*
           * Super section.
           * We have to be careful about distinguishing rules from
           * super calls.
           */
        | method_id_opt_white TokDoubleColon opt_white source_nonapply TokEol
          { let idl, loc = $1 in
            let e = method_id_string_exp idl loc in
               ColonBody, rule2 true (e, loc) $2 $4 $5 []
          }
        | method_id_prefix_opt_white TokDoubleColon source TokEol
          { let idl, loc = $1 in
            let e = method_id_prefix_string_exp idl loc in
               ColonBody, rule2 true (e, loc) $2 $3 $4 []
          }
        | method_id_opt_white TokDoubleColon opt_white method_id_opt_white TokLeftParen opt_args TokRightParen opt_colon TokEol
          { let super, loc1 = $1 in
            let name, _ = $4 in
            let body = $8 in
            let loc = union_loc loc1 $9 in
            let args = args_of_parse_args $6 in
            let e =
               match super, name with
                  [super], [name] ->
                     SuperApplyExp (CommandApply, Lm_symbol.add super, Lm_symbol.add name, args, loc)
                | _, [_] ->
                     raise (OmakeException (loc_exp_pos loc, StringStringError ("illegal super class", method_id_string super)))
                | _ ->
                     raise (OmakeException (loc_exp_pos loc, StringStringError ("illegal field name", method_id_string name)))
            in
               body, e
          }

          /* Anything else is a command to run */
        | other_id_target TokEol
          { let e, loc = $1 in
               NoBody, ShellExp (e, loc)
          }
        | other_target TokEol
          { let e, loc = $1 in
               NoBody, ShellExp (e, loc)
          }
        ;

/*
 * Dependencies only.
 */
deps:
          rev_deps TokEof
          { List.rev $1 }
        ;

rev_deps:
          /* empty */
          { [] }
        | rev_deps dep
          { $2 :: $1 }
        | rev_deps TokEol
          { $1 }
        ;

dep:      /* 2-place rule dependency */
          target TokColon target TokEol
          { let _, loc2 = $2 in
            let target, loc1 =
               match $1 with
                  Some (e, loc1) -> e, loc1
                | None ->
                     NullExp loc2, loc2
            in
            let source =
               match $3 with
                  Some (e, _) -> e
                | None -> NullExp loc2
            in
            let loc = union_loc loc1 $4 in
               target, source, loc
          }
        ;

/*
 * A variable lookup.
 */
apply:    TokDollar opt_white TokLeftParen opt_white method_name opt_apply_args TokRightParen
          { let _, strategy, loc1 = $1 in
            let _, loc2 = $7 in
            let idl, _ = $5 in
            let args = args_of_parse_args $6 in
            let loc = union_loc loc1 loc2 in
               match idl with
                  [id] ->
                     ApplyExp (strategy, Lm_symbol.add id, args, loc), loc
                | _ ->
                     MethodApplyExp (strategy, method_id_intern idl, args, loc), loc
          }
        | TokDollar opt_white TokLeftParen opt_white id TokDoubleColon id opt_apply_args TokRightParen
          { let _, strategy, loc1 = $1 in
            let _, loc2 = $9 in
            let super, _ = $5 in
            let v, _ = $7 in
            let args = args_of_parse_args $8 in
            let loc = union_loc loc1 loc2 in
               SuperApplyExp (strategy, Lm_symbol.add super, Lm_symbol.add v, args, loc), loc
          }
        | TokVar
          { let strategy, id, loc = $1 in
               ApplyExp (strategy, Lm_symbol.add id, [], loc), loc
          }
        | TokBeginQuote rev_text TokEndQuote
          { let id1, loc1 = $1 in
            let id2, loc2 = $3 in
            let loc = union_loc loc1 loc2 in
            let el = StringOtherExp (id1, loc1) :: List.rev_append $2 [StringOtherExp (id2, loc2)] in
               QuoteExp (el, loc), loc
          }
        | TokBeginQuoteString rev_text TokEndQuoteString
          { let id, loc1 = $1 in
            let _, loc2 = $3 in
            let loc = union_loc loc1 loc2 in
               QuoteStringExp (id.[0], List.rev $2, loc), loc
          }
        | TokStringQuote
          { let s, loc = $1 in
               QuoteExp ([StringOtherExp (s, loc)], loc), loc
          }
        ;

/*
 * A quoted variable.
 */
var_quote_opt_white:
          var_quote
          { $1 }
        | var_quote_white
          { let strategy, id, _, loc = $1 in
               strategy, id, loc
          }
        ;

var_quote_white:
          var_quote TokWhite
          { let strategy, id, loc = $1 in
            let s, _ = $2 in
               strategy, id, s, loc
          }
        ;

var_quote:
          TokVarQuote
          { $1 }
        ;

/*
 * Variable lookup.
 */
quote_opt_white:
          var_quote_opt_white
          { var_quote $1 }
        ;

quote_white:
          var_quote_white
          { let strategy, id, s, loc = $1 in
            let e, _ = var_quote (strategy, id, loc) in
               e, s, loc
          }
        ;

quote:    var_quote
          { var_quote $1 }
        ;

/*
 * Names separated by dots.
 */
method_name:
          rev_method_name
          { let idl, loc = $1 in
               List.rev idl, loc
          }
        ;

rev_method_name:
          id
          { let id, loc = $1 in
               [id], loc
          }
        | rev_method_name TokDot id
          { let idl, loc1 = $1 in
            let id, loc2 = $3 in
               id :: idl, union_loc loc1 loc2
          }
        ;

id:       TokId
          { $1 }
        | TokKeyword
          { $1 }
        | TokCatch
          { $1 }
        | TokClass
          { $1 }
        ;

opt_id_list:
          /* empty */
          { [] }
        | opt_id_list white
          { $1 }
        | opt_id_list id
          { let id, _ = $2 in
               id :: $1
          }
        ;

/*
 * A target after identifier text.
 * It may not begin with equals, left-paren, or .
 * and it may not contains colons.
 */
other_id_target:
          method_id_opt_white
          { let idl, loc = $1 in
               method_id_string_exp idl loc, loc
          }
        | method_id_prefix_opt_white
          { let idl, loc = $1 in
               method_id_prefix_string_exp idl loc, loc
          }
        | quote_opt_white
          { $1 }
        ;

method_id_opt_white:
          rev_method_id
          { let id, loc = $1 in
               List.rev id, loc
          }
        | rev_method_id_white
          { let id, _, loc = $1 in
               List.rev id, loc
          }
        ;

method_id_prefix_opt_white:
          rev_method_id_prefix
          { let id, loc = $1 in
               List.rev id, loc
          }
        | rev_method_id_prefix_white
          { let id, _, loc = $1 in
               List.rev id, loc
          }
        ;

rev_method_id_white:
          rev_method_id TokWhite
          { let id, loc1 = $1 in
            let s, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               id, s, loc
          }
        ;

rev_method_id_prefix_white:
          rev_method_id_prefix TokWhite
          { let id, loc1 = $1 in
            let s, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               id, s, loc
          }
        ;

rev_method_id:
          TokId
          { let id, loc = $1 in
               [id], loc
          }
        | rev_method_id_prefix id
          { let idl, loc1 = $1 in
            let id, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               id :: idl, loc
          }
        ;

rev_method_id_prefix:
          rev_method_id TokDot
          { let idl, loc1 = $1 in
            let _, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               idl, loc
          }
        | TokKeyword TokDot
          { let id, loc1 = $1 in
            let _, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               [id], loc
          }
        | TokClass TokDot
          { let id, loc1 = $1 in
            let _, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               [id], loc
          }
        | TokCatch TokDot
          { let id, loc1 = $1 in
            let _, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               [id], loc
          }
        ;

/*
 * The other_target collects all the other stuff that
 * is not a valid command prefix, but it does not allow colons.
 * Don't worry about catching all the other cases--here
 * are the things we should not match:
 *    TokKeyword anything
 *    TokCatch anything
 *    method_id_opt_white TokEq
 *    method_id_prefix_opt_white TokEq
 *    method_id_opt_white TokArray
 *    method_id_opt_white TokLeftParen
 *
 * So here are the sequences that put us into other mode:
 *    1. [^ TokKeyword TokCatch TokId TokColon]
 *    2. method_id [^ TokEq TokArray TokLeftParen TokDot TokWhite TokColon]
 *    3. method_id_white [^ TokEq TokArray TokLeftParen TokColon]
 *    4. method_id_prefix [^ TokEq TokWhite TokColon]
 *    5. method_id_prefix_white [^ TokEq TokColon]
 * Then collect anything except TokColon
 */
other_target:
          rev_other_target
          { let l, loc = $1 in
               sequence_exp (List.rev l) loc, loc
          }
        ;

rev_other_target:
          other_start
          { let e, loc = $1 in
               [e], loc
          }
        | rev_method_id other_method_id
          { let idl, loc1 = $1 in
            let e, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
            let el = [e; method_id_string_exp (List.rev idl) loc1] in
               el, loc
          }
        | rev_method_id_white other_method_id_white
          { let idl, s, loc1 = $1 in
            let e, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
            let el = [e; StringWhiteExp (s, loc1); method_id_string_exp (List.rev idl) loc1] in
               el, loc
          }
        | rev_method_id_prefix other_method_id_prefix
          { let idl, loc1 = $1 in
            let e, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
            let el = [e; method_id_prefix_string_exp (List.rev idl) loc1] in
               el, loc
          }
        | rev_method_id_prefix_white other_method_id_prefix_white
          { let idl, s, loc1 = $1 in
            let e, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
            let el = [e; StringWhiteExp (s, loc1); method_id_prefix_string_exp (List.rev idl) loc1] in
               el, loc
          }
        | quote other_quote_id
          { let id, loc1 = $1 in
            let e, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
            let el = [id; e] in
               el, loc
          }
        | quote_white other_quote_id_white
          { let id, s, loc1 = $1 in
            let e, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
            let el = [id; StringWhiteExp (s, loc1); e] in
               el, loc
          }
        | rev_other_target target_next
          { let el, loc1 = $1 in
            let e, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               e :: el, loc
          }
        ;

/************************************************************************
 * Source arguments allow named colons.
 */
source:
         target
         { match $1 with
              Some (e, _) ->
                 SymbolTable.add SymbolTable.empty normal_sym e
            | None ->
                 SymbolTable.empty
         }
       | source TokNamedColon target
         { let table = $1 in
           let name, _ = $2 in
              match $3 with
                 Some (e, _) ->
                    SymbolTable.add table (Lm_symbol.add name) e
               | None ->
                    table
         }
       ;

/*
 * This source cannot look like an application.
 */
source_nonapply:
         source_target
         { match $1 with
              Some (e, _) ->
                 SymbolTable.add SymbolTable.empty normal_sym e
            | None ->
                 SymbolTable.empty
         }
       | source_nonapply TokNamedColon target
         { let table = $1 in
           let name, _ = $2 in
              match $3 with
                 Some (e, _) ->
                    SymbolTable.add table (Lm_symbol.add name) e
               | None ->
                    table
         }
       ;

source_target:
          /* empty */
          { None }
        | other_id_target
          { Some $1 }
        | other_target
          { Some $1 }
        ;

/************************************************************************
 * Sequence sections.
 */

/*
 * text: [^ TokEol]*
 * text_next: [^ TokEol]
 * Leading whitespace is not stripped.
 */
text:
          rev_text
          { List.rev $1 }
        ;

rev_text:
          /* empty */
          { [] }
        | rev_text text_next
          { let e, _ = $2 in
               e :: $1
          }
        ;

/*
 * target: [^ TokEol TokColon TokNamedColon]*
 * Leading whitespace is stripped:
 *    target_start: [^ TokEol TokColon TokNamedColon TokWhite]
 *    target_next:  [^ TokEol TokColon TokNamedColon]
 */
target:
          opt_white
          { None }
        | opt_white rev_target
          { let l, loc = $2 in
               Some (sequence_exp (List.rev l) loc, loc)
          }
        ;

rev_target:
          target_start
          { let e, loc = $1 in
               [e], loc
          }
        | rev_target target_next
          { let l, loc1 = $1 in
            let e, loc2 = $2 in
               e :: l, union_loc loc1 loc2
          }
        ;

/*
 * text_optcolon: text_colon | text_noncolon
 *    text_colon:     [^ TokEol]* TokColon
 *    text_noncolon: ([^ TokEol]* [^ TokEol TokColon])?
 */
keyword_text_optcolon:
          /* empty */
          { None }
        | rev_keyword_text
          { let code, _, el, loc = $1 in
               Some (code, sequence_exp (List.rev el) loc)
          }
        ;

rev_keyword_text:
          keyword_target_start
          { let e, loc = $1 in
               OptBody, [], [e], loc
          }
        | colon
          { let e, loc = $1 in
               ColonBody, [e], [], loc
          }
        | rev_keyword_text white
          { let code, final, prefix, loc1 = $1 in
            let e, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               code, e :: final, prefix, loc
          }
        | rev_keyword_text target_start
          { let _, final, prefix, loc1 = $1 in
            let e, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               OptBody, [], e :: (final @ prefix), loc
          }
        | rev_keyword_text colon
          { let _, final, prefix, loc1 = $1 in
            let e, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               ColonBody, [e], final @ prefix, loc
          }
        ;

/*
 * Strip trailing whitespace from nonempty text.
 */
text_nonempty:
          rev_text_nonempty
          { let l, loc = $1 in
               sequence_exp (List.rev l) loc
          }
        ;

rev_text_nonempty:
          text_nonwhite
          { let e, loc = $1 in
               [e], loc
          }
        | rev_text_nonempty text_next
          { let l, loc1 = $1 in
            let e, loc2 = $2 in
               e :: l, union_loc loc1 loc2
          }
        ;
/*
 * arg: [^ TokEol TokComma TokArrow TokLeftParen TokRightParen]*
 * leading whitespace is stripped.
 */
opt_args:
          opt_white
          { [] }
        | args
          { $1 }
        ;

opt_apply_args:
          opt_white
          { [] }
        | white args
          { $2 }
        ;

args:     rev_args
          { List.rev $1 }
        | rev_arrow_args
          { List.rev $1 }
        | rev_arrow_args TokComma rev_args
          { List.rev_append $1 (List.rev $3) }
        ;

rev_arrow_args:
          arrow_arg
          { [$1] }
        | rev_arrow_args TokComma arrow_arg
          { $3 :: $1 }
        ;

arrow_arg:
          rev_args TokArrow opt_white rev_any_arg
          { let el, loc2 = $4 in
               NormalArg (ArrowArg (get_fun_params (List.rev $1), sequence_exp (List.rev el) loc2))
          }
        ;

rev_args:
          arg
          { [$1] }
        | rev_args TokComma arg
          { $3 :: $1 }
        ;

arg:      opt_white arg_inner
          { $2 }
        ;

arg_inner:
          rev_normal_arg
          { let el, loc = $1 in
            let e = sequence_exp (List.rev el) loc in
               NormalArg (ExpArg e)
          }
        | arg_id
          { let (id, loc0), w, loc = $1 in
	       IdArg (id, w, loc)
          }
        | arg_key
          { let (id, loc0), w, loc = $1 in
	       IdArg (id, w, loc)
          }
        | arg_key TokEq opt_white
          { let (id, _), _, loc1 = $1 in
	    let key = key_of_id id in
               NormalArg (KeyArg (key, NullExp loc1))
          }
        | arg_key TokEq opt_white rev_any_arg
          { let (id, _), _, _ = $1 in
	    let key = key_of_id id in
            let el, loc2 = $4 in
               NormalArg (KeyArg (key, sequence_exp (List.rev el) loc2))
          }
        ;

arg_id:   id opt_white
          { let id = $1 in
            let _, loc = id in
               id, $2, loc
          }
	;

arg_key:
          TokKey opt_white
          { let id = $1 in
            let _, loc = id in
               id, $2, loc
          }
        ;

rev_any_arg:
          paren_arg_any_start
          { let e, loc = $1 in
               [e], loc
          }
        | rev_any_arg paren_arg_next
          { let l, loc1 = $1 in
            let e, loc2 = $2 in
               e :: l, union_loc loc1 loc2
          }
        ;

rev_normal_arg:
          arg_key paren_arg_next_noneq
          { let (id, loc0), w, loc1 = $1 in
            let id = StringIdExp (id, loc0) in
            let e, loc2 = $2 in
            let el =
               match w with
                  Some (w, loc0) ->
                     [e; StringWhiteExp (w, loc0); id]
                | None ->
                     [e; id]
            in
               el, union_loc loc1 loc2
          }
        | arg_id paren_arg_any_start
          { let (id, loc0), w, loc1 = $1 in
            let id = StringIdExp (id, loc0) in
            let e, loc2 = $2 in
            let el =
               match w with
                  Some (w, loc3) ->
                     [e; StringWhiteExp (w, loc3); id]
                | None ->
                     [e; id]
            in
               el, union_loc loc1 loc2
          }
        | paren_arg_start
          { let e, loc = $1 in
               [e], loc
          }
        | rev_normal_arg paren_arg_next
          { let l, loc1 = $1 in
            let e, loc2 = $2 in
               e :: l, union_loc loc1 loc2
          }
        ;

paren_arg_any_start:
	  arg_any_start
	  { $1 }
	| paren_arg
	  { $1 }
	;

paren_arg_next_noneq:
	  arg_next_noneq
	  { $1 }
	| paren_arg
	  { $1 }
	;

paren_arg_start:
	  arg_start
	  { $1 }
	| paren_arg
	  { $1 }
	;

paren_arg_next:
	  arg_next
	  { $1 }
	| paren_arg
	  { $1 }
	;

paren_arg:
	  TokLeftParen rev_paren_text TokRightParen
          { let s1, loc1 = $1 in
            let sl = $2 in
            let s3, loc3 = $3 in
            let loc = union_loc loc1 loc3 in
            let el = StringOpExp (s1, loc1) :: (List.rev (StringOpExp (s3, loc3) :: sl)) in
               SequenceExp (el, loc), loc
          }
	;

rev_paren_text:
	  /* empty */
	  { [] }
	| rev_paren_text paren_next
	  { let s, _ = $2 in
               s :: $1
          }
        | rev_paren_text paren_arg
          { let s, _ = $2 in
               s :: $1
          }
	;

/*
 * Generated section.
 */
colon:
	| TokNamedColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDoubleColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	;
white:
	| TokWhite
		{ let (s, loc) = $1 in StringWhiteExp (s, loc), loc }
	;
text_next:
	| TokWhite
		{ let (s, loc) = $1 in StringWhiteExp (s, loc), loc }
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokLeftParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokRightParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArrow
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokComma
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDoubleColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokNamedColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokEq
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokId
		{ let (s, loc) = $1 in StringIdExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokKeyword
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokCatch
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokClass
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
text_nonwhite:
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokLeftParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokRightParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArrow
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokComma
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDoubleColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokNamedColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokEq
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokId
		{ let (s, loc) = $1 in StringIdExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokKeyword
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokCatch
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokClass
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
target_next:
	| TokWhite
		{ let (s, loc) = $1 in StringWhiteExp (s, loc), loc }
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokLeftParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokRightParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArrow
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokComma
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokEq
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokId
		{ let (s, loc) = $1 in StringIdExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokKeyword
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokCatch
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokClass
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
target_start:
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokLeftParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokRightParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArrow
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokComma
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokEq
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokId
		{ let (s, loc) = $1 in StringIdExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokKeyword
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokCatch
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokClass
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
keyword_target_start:
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokRightParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArrow
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokComma
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokEq
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokId
		{ let (s, loc) = $1 in StringIdExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokKeyword
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokCatch
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokClass
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
paren_next:
	| TokWhite
		{ let (s, loc) = $1 in StringWhiteExp (s, loc), loc }
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArrow
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokComma
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDoubleColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokNamedColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokEq
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokId
		{ let (s, loc) = $1 in StringIdExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokKeyword
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokCatch
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokClass
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
arg_next:
	| TokWhite
		{ let (s, loc) = $1 in StringWhiteExp (s, loc), loc }
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDoubleColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokNamedColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokEq
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokId
		{ let (s, loc) = $1 in StringIdExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokKeyword
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokCatch
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokClass
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
arg_start:
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDoubleColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokNamedColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokEq
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
arg_any_start:
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDoubleColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokNamedColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokEq
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokId
		{ let (s, loc) = $1 in StringIdExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokKeyword
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokCatch
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokClass
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
arg_next_noneq:
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDoubleColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokNamedColon
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokId
		{ let (s, loc) = $1 in StringIdExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokKeyword
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokCatch
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokClass
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
other_start:
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokLeftParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokRightParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArrow
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokComma
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokEq
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| apply
		{ $1 }
	;
other_method_id_white:
	| TokWhite
		{ let (s, loc) = $1 in StringWhiteExp (s, loc), loc }
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokRightParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArrow
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokComma
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokId
		{ let (s, loc) = $1 in StringIdExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokKeyword
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokCatch
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokClass
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
other_method_id:
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokRightParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArrow
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokComma
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokId
		{ let (s, loc) = $1 in StringIdExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokKeyword
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokCatch
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokClass
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
other_method_id_prefix_white:
	| TokWhite
		{ let (s, loc) = $1 in StringWhiteExp (s, loc), loc }
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokLeftParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokRightParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArrow
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokComma
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
other_method_id_prefix:
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokLeftParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokRightParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArrow
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokComma
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
other_quote_id_white:
	| TokWhite
		{ let (s, loc) = $1 in StringWhiteExp (s, loc), loc }
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokLeftParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokRightParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArrow
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokComma
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokId
		{ let (s, loc) = $1 in StringIdExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokKeyword
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokCatch
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokClass
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;
other_quote_id:
	| TokOp
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokLeftParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokRightParen
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArrow
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokComma
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokArray
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokDot
		{ let (s, loc) = $1 in StringOpExp (s, loc), loc }
	| TokId
		{ let (s, loc) = $1 in StringIdExp (s, loc), loc }
	| TokInt
		{ let (s, loc) = $1 in StringIntExp (s, loc), loc }
	| TokFloat
		{ let (s, loc) = $1 in StringFloatExp (s, loc), loc }
	| TokKey
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokKeyword
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokCatch
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokClass
		{ let (s, loc) = $1 in StringKeywordExp (s, loc), loc }
	| TokString
		{ let (s, loc) = $1 in StringOtherExp (s, loc), loc }
	| quote
		{ $1 }
	| apply
		{ $1 }
	;

/*
 * Optional white space.
 */
opt_literal_colon:
          /* empty */
          { OptBody }
        | colon opt_white
          { ColonBody }
        ;

opt_colon:
          opt_white
          { OptBody }
        | opt_white colon opt_white
          { ColonBody }
        ;

opt_white:
          /* empty */
          { None }
        | TokWhite
          { Some $1 }
        ;
