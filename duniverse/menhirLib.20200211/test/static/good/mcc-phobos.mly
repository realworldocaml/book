/*
 * Parser for Phobos files.
 * ----------------------------------------------------------------
 *
 * Copyright (C) Adam Granicz, Caltech
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
 * Author: Adam Granicz
 * Email: granicz@cs.caltech.edu
 */

%{
open Symbol
open Opname
open Mp_num
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Phobos_type
open Phobos_constants
open Phobos_marshal
open Phobos_parse_state
open Phobos_exn
open Phobos_util
open Phobos_rewrite
open Phobos_builtin

(*
 * This is the name of the module.
 * It is only used in the opname of a product term.
 *)
let module_name = ref ""

(*
 * There are terms that are private to a module.
 * If they are ever show up outside of the parser,
 * something is wrong.
 *)
let privates_module = "@"

(*
 * Support for module names.
 *)
let term_names = ref StringTable.empty

(*
 * Add each built-in term to the private list.
 *)
let _ =
   List.iter (fun s ->
      term_names := StringTable.add !term_names s (privates_module, -1)) (snd (List.hd built_in_terms))

let new_terms mdl decls =
   List.iter (fun ((id, pos), subterm_count) ->
      if StringTable.mem !term_names id then begin
         let mod_name, stc = StringTable.find !term_names id in
            Printf.eprintf "warning: %s : %s{?} has already been defined in another module [as %s{%d} in %s]\n" (**)
               (string_of_pos pos) id id stc mod_name
      end;
      if !Fir_state.debug_phobos then
         Format.print_string (Printf.sprintf "Adding term [%s]\n" id);
      term_names := StringTable.add !term_names id (mdl, subterm_count)) decls

let term_name_of (id, pos) =
   try
      let mod_name, subterm_count = StringTable.find !term_names id in
         if mod_name = privates_module then
            [id]
         else
            [id; mod_name]
   with
      Not_found ->
         raise (PhobosException (pos, Printf.sprintf "undefined term [%s]" id))

type param_type =
   TyString
 | TyNum
 | TyToken
 | TyLevel
 | TyVar

let pho_make_term id_pos_list params bterms =
   let opname =
      if List.length id_pos_list > 1 then
         List.map fst id_pos_list
      else
         term_name_of (List.hd id_pos_list)
   in
      mk_term (mk_op (make_opname opname) params) bterms

let pho_make_var_term (id, pos) =
   mk_var_term id

let pho_make_bterm id_pos_list pterm =
   let vars = List.map fst id_pos_list in
      mk_bterm vars (fst pterm)

let pho_make_unique_var_term = unique_var_term
let pho_make_token_term = token_term
let pho_make_prod_term = prod_term

let pho_make_number_term num =
   let param = make_param (Number (num_of_int (fst num))) in
      mk_term (mk_op (make_opname ["number"; "Itt_int_base"]) [param]) []

let rec make_rules (id, pos) = function
   head :: rest ->
      ((id, pos), head) :: make_rules (id, pos) rest
 | [] ->
      []

let make_rules head productions =
   List.rev (make_rules head productions)

let production_of_shorthand id_list opt_prec result =
   let from =
      List.map (fun id ->
         pho_make_unique_var_term id, snd id) id_list
   in
      id_list, opt_prec, [(from, result)]

let insert_rewrite_if_needed id_list = function
   [] ->
      let from_terms =
         List.map (fun (s, pos) ->
            pho_make_unique_var_term (s, pos)) id_list
      in
      let from = List.map2 (fun term (s, pos) -> term, pos) from_terms id_list in
      (match id_list with
         [s, pos] ->
               [from, List.hd from]
       | _ ->
            (* We use a bogus position for the result *)
            let result_pos = ("<prod>", 0, 0, 0, 0) in
            let result = pho_make_prod_term from_terms, result_pos in
               [from, result])
 | a ->
      a

let process_includes includes =
   List.iter (fun s ->
      if !Fir_state.debug_phobos then
         Format.print_string (Printf.sprintf "Loading %s..." s);
      let gst, _, _, _ = load_grammar s in
      if !Fir_state.debug_phobos then
         Format.print_string "done\n";
      let termsets = gst.grammar_termsets in
         List.iter (fun term_option_list ->
            List.iter (fun term_option ->
               if !Fir_state.debug_phobos then
                  Format.print_string ".";
               match term_option with
                  To_extend (mdl, decls) ->
                     new_terms mdl decls) term_option_list) termsets) includes;
      if !Fir_state.debug_phobos then
         Format.print_string "\nDone processing includes\n"

let include_built_in (s, pos) =
   let new_terms, found =
      List.fold_left (fun (new_terms, found) (mdl, terms) ->
         if s = mdl then
            new_terms @ terms, true
         else
            new_terms, found) ([], false) built_in_terms
   in
   if not found then
      raise (PhobosException (pos, Printf.sprintf (**)
         "Invalid internal module [%s]" s))
   else
      List.iter (fun term ->
         term_names := StringTable.add !term_names term (s, -1)) new_terms

%}

%token TokEof

%token <Phobos_type.pos> TokEq
%token <Phobos_type.pos> TokRuleEq
%token <Phobos_type.pos> TokArrow
%token <Phobos_type.pos> TokDoubledArrow
%token <Phobos_type.pos> TokPipe
%token <Phobos_type.pos> TokSemi
%token <Phobos_type.pos> TokColon
%token <Phobos_type.pos> TokComma
%token <Phobos_type.pos> TokLeftBrace
%token <Phobos_type.pos> TokRightBrace
%token <Phobos_type.pos> TokLeftBrack
%token <Phobos_type.pos> TokRightBrack
%token <Phobos_type.pos> TokIgnore
%token <Phobos_type.pos> TokBang
%token <Phobos_type.pos> TokDot
%token <Phobos_type.pos> TokQuestionMark

%token <Phobos_type.pos> TokStart
%token <Phobos_type.pos> TokLongest
%token <Phobos_type.pos> TokFirst
%token <Phobos_type.pos> TokExtend

%token <Phobos_type.pos> TokNonAssoc
%token <Phobos_type.pos> TokLeftAssoc
%token <Phobos_type.pos> TokRightAssoc
%token <Phobos_type.pos> TokPrec

%token <Phobos_type.pos> TokModule
%token <Phobos_type.pos> TokInclude
%token <Phobos_type.pos> TokTerms
%token <Phobos_type.pos> TokTokens
%token <Phobos_type.pos> TokGrammar
%token <Phobos_type.pos> TokDeclare
%token <Phobos_type.pos> TokRewrites
%token <Phobos_type.pos> TokInline

%token <string * Phobos_type.pos> TokOption
%token <string * Phobos_type.pos> TokString
%token <int * Phobos_type.pos> TokInt
%token <float * Phobos_type.pos> TokFloat

%token <string * Phobos_type.pos> TokId
%token <string * Phobos_type.pos> TokQuotedId

%start main
%type <Phobos_type.mp_pre_term> simple_term
%type <Phobos_type.mp_pre_rewrite list> term_match_list
%type <Phobos_type.mp_pre_rewrite list * Phobos_type.pos> body
%type <Phobos_type.pre_rule list> rule
%type <Phobos_type.pre_rule list> rule_list
%type <Phobos_type.pre_rule list> rules
%type <Phobos_type.pre_rule list * Phobos_type.goption list> grammar
%type <Phobos_type.phobos_parser_return_type> main
%%

main:
     module_name includes term_sections opt_preamble lexer opt_assocs grammar opt_rewrites_section opt_inline_forms TokEof
                              { { phobos_module_name = $1;
                                  phobos_includes = $2;
                                  phobos_termsets = $3;
                                  phobos_local_rewrites = $4;
                                  phobos_lexer_info = $5;
                                  phobos_assoc_info = $6;
                                  phobos_grammar_info = $7;
                                  phobos_post_rewrites = $8;
                                  phobos_inline_forms = $9
                                }
                              }

module_name:
     TokModule identifier     { module_name := fst $2; fst $2 }

/*
 * Includes.
 */
includes:
     /* empty */              { [] }
   | include_list_rev         { let mdl_names = List.flatten (List.rev $1) in
                                   process_includes mdl_names;
                                   mdl_names
                              }

include_list_rev:
     include_list_rev include_item
                              { $2 :: $1 }
   | include_item             { [$1] }

include_item:
     TokInclude string_list   { $2 }
   | TokInclude identifier    { include_built_in $2; [] }

string_list:
     string_list_rev          { List.rev $1 }

string_list_rev:
     string_list TokString    { (fst $2) :: $1 }
   | TokString                { [fst $1] }

/*
 * Term sections.
 */
term_sections:
     term_section_list        { List.rev $1 }

term_section_list:
     term_section             { [$1] }
   | term_section_list term_section
                              { $2 :: $1 }

term_section:
     TokTerms term_options    { $2 }

term_options:
     term_option_list         { List.rev $1 }

term_option_list:
     term_option              { [$1] }
   | term_option_list term_option
                              { $2 :: $1 }

term_option:
     TokExtend TokString TokLeftBrace opt_term_declarations TokRightBrace
                              { new_terms (fst $2) $4;
                                To_extend (fst $2, $4)
                              }
   | TokOption                { raise (ParseError (snd $1, string_add ["Invalid option \""; fst $1; "\""])) }

/*
 * Preamble.
 */
opt_preamble:
   /* empty */                { [] }
   | preamble                 { $1 }

preamble:
     TokLeftBrace new_rewrites TokRightBrace
                              { $2 }

/*
 * Lexer.
 */
lexer:
     TokTokens opt_lexer_options TokLeftBrace tokens TokRightBrace
                              { $4, $2 }

opt_lexer_options:
   /* empty */                { [] }
   | lexer_option_list        { List.rev $1 }

lexer_option_list:
     lexer_option_list lexer_option
                              { $2 :: $1 }
   | lexer_option             { [$1] }

lexer_option:
     TokLongest               { Lo_longest }
   | TokFirst                 { Lo_first }
   | TokOption                { raise (ParseError (snd $1, string_add ["Invalid option \""; fst $1; "\""])) }

tokens:
     token_list               { List.rev $1 }

token_list:
     token_list token         { $2 :: $1 }
   | token                    { [$1] }

token:
     TokId TokEq TokString token_body
                              { false, fst $1, fst $3, fst $4 }
   | TokIgnore TokId TokEq TokString token_body
                              { true, fst $2, fst $4, fst $5 }

identifiers:
     rev_identifier_list      { List.rev $1 }

rev_identifier_list:
     identifier               { [$1] }
   | rev_identifier_list identifier
                              { $2 :: $1 }

identifiers_with_comma:
     rev_identifier_list_with_comma
                              { List.rev $1 }

rev_identifier_list_with_comma:
     identifier               { [$1] }
   | rev_identifier_list_with_comma TokComma identifier
                              { $3 :: $1 }

identifier:
     TokId                    { $1 }

/*
 * Associativity and precedence.
 */
opt_assocs:
   /* empty */                { [] }
   | assoc_list               { List.rev $1 }

assoc_list:
     assoc_list assoc         { $2 :: $1 }
   | assoc                    { [$1] }

assoc:
     TokNonAssoc identifiers
                              { Dir_nonassoc $2 }
   | TokLeftAssoc identifiers
                              { Dir_leftassoc $2 }
   | TokRightAssoc identifiers
                              { Dir_rightassoc $2 }

/*
 * Grammar.
 */
grammar:
     TokGrammar opt_grammar_options TokLeftBrace rules TokRightBrace
                              { $4, $2 }

opt_grammar_options:
   /* empty */                { [] }
   | grammar_option_list      { List.rev $1 }

grammar_option_list:
     grammar_option_list grammar_option
                              { $2 :: $1 }
   | grammar_option           { [$1] }

grammar_option:
     TokStart identifier      { Go_start (fst $2) }

rules:
     rule_list                { $1 }

rule_list:
     rule_list rule           { $1 @ $2 }
   | rule                     { $1 }

rule:
     TokId TokRuleEq productions
                              { make_rules $1 $3 }

productions:
     prod_body prods          { $1 :: $2 }
   | prod_body                { [$1] }

opt_prec:
     /* empty */              { None }
   | TokPrec TokId            { Some $2 }

prods:
     prod_list                { List.rev $1 }

prod_list:
     prod_list prod_list_prim { $2 :: $1 }
   | prod_list_prim           { [$1] }

prod_list_prim:
     TokPipe prod_body
                              { $2 }

prod_body:
     identifiers opt_prec body
                              { $1, $2, insert_rewrite_if_needed $1 (fst $3) }
   | identifiers opt_prec TokDoubledArrow simple_term
                              { production_of_shorthand $1 $2 $4 }

token_body:
     TokLeftBrace token_term_matches TokRightBrace
                              { $2, union_pos $1 $3 }

body:
     TokLeftBrace opt_term_matches TokRightBrace
                              { $2, union_pos $1 $3 }

new_rewrites:
   /* empty */                { [] }
   | new_rewrite_list         { List.rev $1 }

new_rewrite_list:
     new_rewrite_list new_rewrite
                              { $2 :: $1 }
   | new_rewrite              { [$1] }

new_rewrite:
     from TokArrow simple_term
                              { $1, $3 }

token_term_matches:
   /* empty */                { let term = pho_make_token_term () in
                                   [([(term, bogus_pos)], (term, bogus_pos))]
                              }
   | term_match_list          { List.rev $1 }

opt_term_matches:
   /* empty */                { [] }
   | term_match_list          { List.rev $1 }

term_match_list:
     term_match_list TokPipe term_match
                              { $3 :: $1 }
   | term_match               { [$1] }

term_match:
     froms TokArrow simple_term
                              { $1, $3 }

froms:
     from_list                { List.rev $1 }

from_list:
     from_list from           { $2 :: $1 }
   | from                     { [$1] }

from:
     simple_term              { $1 }
   | identifier               { pho_make_token_term (), snd $1 }

quoted_identifier:
     TokQuotedId              { $1 }

simple_term:
     module_identifier opt_term_params subterms
                              { pho_make_term (fst $1) $2 (fst $3), union_pos (snd $1) (snd $3) }
   | module_identifier term_params
                              { pho_make_term (fst $1) (fst $2) [], union_pos (snd $1) (snd $2) }
   | quoted_identifier        { pho_make_var_term $1, snd $1 }
   | TokInt                   { pho_make_number_term $1, snd $1 }
   | TokQuestionMark          { pho_make_unique_var_term ("?", $1), $1 }

/* Opname */
/* REMARK: name parts are in reverse order */
module_identifier:
     modules_rev              { $1 }

modules_rev:
     identifier               { [$1], snd $1 }
   | modules_rev TokBang identifier
                              { $3 :: fst $1, union_pos (snd $1) (snd $3) }

/*
 * Subterms
 */
subterms:
     TokLeftBrace opt_subterm_list_semi TokRightBrace
                              { $2, union_pos $1 $3 }

opt_subterm_list_semi:
   /* empty */                { [] }
   | subterms_with_semi       { $1 }

subterms_with_semi:
     subterm_list_with_semi   { List.rev $1 }

subterm_list_with_semi:
     subterm_list_with_semi TokSemi sub_term
                              { $3 :: $1 }
   | sub_term                 { [$1] }

sub_term:
     simple_term              { pho_make_bterm [] $1 }
   | identifiers_with_comma TokDot simple_term
                              { pho_make_bterm $1 $3 }

/* Params */
opt_term_params:
   /* empty */                { [] }
   | term_params
                              { List.rev (fst $1) }

term_params:
     TokLeftBrack term_param_list TokRightBrack
                              { List.rev $2, union_pos $1 $3 }

term_param_list:
     term_param_list term_param
                              { $2 :: $1 }
   | term_param               { [$1] }

param_type_id:
     TokId                    { match fst $1 with
                                   "s" -> TyString, snd $1
                                 | "n" -> TyNum, snd $1
                                 | "v" -> TyVar, snd $1
                                 | "t" -> TyToken, snd $1
                                 | "l" -> TyLevel, snd $1
                                 | _ ->
                                       raise (ParseError (snd $1, "unknown parameter type"))
                              }

/* BUG: level parameters are not handled */
term_param:
   /* Meta-parameter */
     TokId TokColon TokId     { match fst $3 with
                                   "s" ->
                                       make_param (MString (fst $1))
                                 | "n" ->
                                       make_param (MNumber (fst $1))
                                 | "v" ->
                                       make_param (MVar (fst $1))
                                 | "t" ->
                                       make_param (MToken (fst $1))
                                 | _ ->
                                       raise (ParseError (snd $3, "unknown meta-parameter type"))
                              }
   /* Regular parameter */
   | TokInt TokColon param_type_id
                              { match fst $3 with
                                   TyNum ->
                                       make_param (Number (num_of_int (fst $1)))
                                 | _ ->
                                       raise (ParseError (snd $3, "invalid parameter type"))
                              }
   | TokString TokColon param_type_id
                              { match fst $3 with
                                   TyString ->
                                       make_param (String (fst $1))
                                 | TyToken ->
                                       make_param (Token (fst $1))
                                 | TyVar ->
                                       make_param (Var (fst $1))
                                 | _ ->
                                       raise (ParseError (snd $3, "invalid parameter type"))
                              }
   | TokString                { make_param (String (fst $1)) }
   | TokInt                   { make_param (Number (num_of_int (fst $1))) }
   /* Meta-parameter without type is assumed to be meta-string */
   | TokId                    { make_param (MString (fst $1)) }

/*
 * Term declarations (for the term set section).
 */
opt_term_declarations:
     /* empty */              { [] }
   | term_declaration_list    { $1 }

term_declaration_list:
     term_declaration         { $1 }
   | term_declaration_list term_declaration
                              { $2 @ $1 }

term_declaration:
     TokDeclare syntax_terms  { $2 }

syntax_terms:
     syntax_term_list_rev     { List.rev $1 }

syntax_term_list_rev:
     syntax_term              { [$1] }
   | syntax_term_list_rev TokComma syntax_term
                              { $3 :: $1 }

syntax_term:
     st_identifier            { $1, 0 }
   | st_identifier TokLeftBrace opt_syntax_subterms TokRightBrace
                              { $1, List.length $3 }

/* HACK: for now, we throw aways parameters in syntax declarations */
st_identifier:
     identifier               { $1 }
   | identifier TokLeftBrack param_type_id TokRightBrack
                              { $1 }

opt_syntax_subterms:
   /* empty */                { [] }
   | syntax_subterms          { $1 }

syntax_subterms:
     syntax_subterm_list      { List.rev $1 }

syntax_subterm_list:
     syntax_subterm           { [$1] }
   | syntax_subterm_list TokSemi syntax_subterm
                              { $3 :: $1 }

syntax_subterm:
     syntax_subterm_base      { $1 }
   | identifier TokDot syntax_subterm_base
                              { $3 }

syntax_subterm_base:
     quoted_identifier        { $1 }
   | quoted_identifier TokLeftBrack quoted_identifier TokRightBrack
                              { $1 }

/*
 * Rewrite section.
 */
opt_rewrites_section:
   /* empty */                { [] }
   | rev_rewrites_section_list
                              { List.rev $1 }

rev_rewrites_section_list:
     rewrites_section         { [$1] }
   | rev_rewrites_section_list rewrites_section
                              { $2 :: $1 }

rewrites_section:
     TokRewrites TokLeftBrace new_rewrites TokRightBrace
                              { $3 }

/*
 * Inline forms.
 */
opt_inline_forms:
   /* empty */                { [] }
   | inline_form_list_rev     { List.rev $1 }

inline_form_list_rev:
     inline_form              { [$1] }
   | inline_form_list_rev inline_form
                              { $2 :: $1 }

inline_form:
     TokInline simple_term    { $2 }
