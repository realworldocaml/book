(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

let value = Positions.value
(* The source. *)
module S = Syntax
(* The target. *)
module T = BasicSyntax

(* -------------------------------------------------------------------------- *)

(* Most of the translation is straightforward. *)

let drop_parameter (param : S.parameter) : S.symbol =
  match param with
  | S.ParameterVar sym ->
      value sym
  | S.ParameterApp _ ->
      (* The grammar should not have any parameterized symbols. *)
      assert false
  | S.ParameterAnonymous _ ->
      assert false

let drop_producer ((id, param, attrs) : S.producer) : T.producer =
  {
    T.producer_identifier = id;
    T.producer_symbol     = drop_parameter param;
    T.producer_attributes = attrs
  }

let drop_branch (branch : S.parameterized_branch) : T.branch =
  {
    T.branch_position         = branch.S.pr_branch_position;
    T.producers               = List.map drop_producer branch.S.pr_producers;
    T.action                  = branch.S.pr_action;
    T.branch_prec_annotation  = branch.S.pr_branch_prec_annotation;
    T.branch_production_level = branch.S.pr_branch_production_level
  }

let drop_rule (rule : S.parameterized_rule) : T.rule =
  (* The grammar should not have any parameterized symbols. *)
  assert (rule.S.pr_parameters = []);
  (* The [%public] flag is dropped. *)
  {
    T.branches = List.map drop_branch rule.S.pr_branches;
    T.positions = rule.S.pr_positions;
    T.inline_flag = rule.S.pr_inline_flag;
    T.attributes = rule.S.pr_attributes;
  }

(* -------------------------------------------------------------------------- *)

(* We must store [%type] declarations and [%on_error_reduce] declarations in
   StringMaps, whereas so far they were represented as lists. *)

let drop_declarations
  (kind : string)
  (f : 'info1 -> 'info2)
  (decls : (S.parameter * 'info1) list)
: 'info2 StringMap.t =

  (* Now is as good a time as any to check against multiple declarations
     concerning a single nonterminal symbol. Indeed, if we did not rule out
     this situation, then we would have to keep only one (arbitrarily chosen)
     declaration. To do this, we first build a map of symbols to info *and*
     position... *)
  List.fold_left (fun accu (param, info) ->
    let symbol = drop_parameter param in
    begin match StringMap.find symbol accu with
    | exception Not_found ->
        ()
    | (_, position) ->
        Error.error [position; Parameters.position param]
          "there are multiple %s declarations for the symbol %s."
          kind symbol
    end;
    StringMap.add symbol (f info, Parameters.position param) accu
  ) StringMap.empty decls
  (* ... then drop the positions. *)
  |> StringMap.map (fun (info, _) -> info)

let drop_type_declarations =
  drop_declarations "%type" value

let drop_on_error_reduce_declarations =
  drop_declarations "%on_error_reduce" (fun x -> x)

(* -------------------------------------------------------------------------- *)

(* We must eliminate (that is, desugar) [%attribute] declarations. We examine
   them one by one and attach these attributes with terminal or nonterminal
   symbols, as appropriate. This is entirely straightforward. *)

let add_attribute (g : T.grammar) param attr : T.grammar =
  let symbol = drop_parameter param in
  match StringMap.find symbol g.T.tokens with
  | props ->
      (* This is a terminal symbol. *)
      let props = { props with S.tk_attributes = attr :: props.S.tk_attributes } in
      { g with T.tokens = StringMap.add symbol props g.T.tokens }
  | exception Not_found ->
      match StringMap.find symbol g.T.rules with
      | rule ->
          (* This is a nonterminal symbol. *)
          let rule = { rule with T.attributes = attr :: rule.T.attributes } in
          { g with T.rules = StringMap.add symbol rule g.T.rules }
      | exception Not_found ->
          (* This is an unknown symbol. This should not happen. *)
          assert false

let add_attributes g (params, attrs) =
  List.fold_left (fun g param ->
    List.fold_left (fun g attr ->
      add_attribute g param attr
    ) g attrs
  ) g params

let add_attributes (decls : (S.parameter list * S.attributes) list) g =
  List.fold_left add_attributes g decls

(* -------------------------------------------------------------------------- *)

(* Putting it all together. *)

let drop (g : S.grammar) : T.grammar =
  {
    T.preludes        = g.S.p_preludes;
    T.postludes       = g.S.p_postludes;
    T.parameters      = g.S.p_parameters;
    T.start_symbols   = StringMap.domain g.S.p_start_symbols;
    T.types           = drop_type_declarations g.S.p_types;
    T.tokens          = g.S.p_tokens;
    T.on_error_reduce = drop_on_error_reduce_declarations g.S.p_on_error_reduce;
    T.gr_attributes   = g.S.p_grammar_attributes;
    T.rules           = StringMap.map drop_rule g.S.p_rules
  } |> add_attributes g.S.p_symbol_attributes
