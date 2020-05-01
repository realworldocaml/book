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

open Syntax

(* We first build an alias map, which records the token aliases declared
   across all partial grammars. This is a map of aliases to pairs of a
   terminal symbol and the position where this symbol is declared. Then, we
   walk the partial grammars before they are joined, expanding the token
   aliases along the way. *)

type aliasmap =
  (terminal * Positions.t) StringMap.t

(* -------------------------------------------------------------------------- *)

(* Extend an alias map with the token aliases present in a declaration. *)

let collect_aliases_from_declaration (aliasmap : aliasmap) decl : aliasmap =
  match Positions.value decl with
  | DToken (_, id, Some qid, _) ->
      begin match StringMap.find qid aliasmap with
      | exception Not_found ->
          (* Good: this alias does not exist yet. Record it. *)
          StringMap.add qid (id, Positions.position decl) aliasmap
      | id0, pos ->
          (* Oops: [qid] has already been declared as an alias for
             some other token. *)
          Error.error
            [Positions.position decl; pos]
            "%s cannot be declared as an alias for the symbol %s.\n\
             It has already been declared as an alias for %s."
            qid id id0
      end
  | _ ->
      aliasmap

(* Extend an alias map with the token aliases present in a partial grammar. *)

let collect_aliases_from_grammar aliasmap g =
  List.fold_left collect_aliases_from_declaration aliasmap g.pg_declarations

let collect_aliases_from_grammars gs : aliasmap =
  List.fold_left collect_aliases_from_grammar StringMap.empty gs

(* -------------------------------------------------------------------------- *)

(* Expand a possible alias, returning a name which definitely is not an
   alias (and may or may not be a valid terminal symbol). *)

let dealias_terminal (aliasmap : aliasmap) pos (t : terminal) : terminal =
  (* [t] is either a terminal symbol or a token alias. If it
     starts with double quote, then it must be a token alias. *)
  if t.[0] = '"' then
    match StringMap.find t aliasmap with
    | id, _ ->
        id
    | exception Not_found ->
        Error.error
          [pos]
          "the token alias %s was never declared." t
  else
    t

(* Perform alias expansion throughout a partial grammar.
   (Visitors could be useful here!) *)

let dealias_symbol aliasmap (sym : terminal Positions.located) =
  Positions.pmap (dealias_terminal aliasmap) sym

let rec dealias_parameter aliasmap (param : parameter) =
  match param with
  | ParameterVar sym ->
      ParameterVar (dealias_symbol aliasmap sym)
  | ParameterApp (sym, params) ->
      ParameterApp (
        dealias_symbol aliasmap sym,
        dealias_parameters aliasmap params
      )
  | ParameterAnonymous branches ->
      ParameterAnonymous (Positions.map (dealias_branches aliasmap) branches)

and dealias_parameters aliasmap params =
  List.map (dealias_parameter aliasmap) params

and dealias_producer aliasmap (producer : producer) =
  let id, param, attrs = producer in
  id, (dealias_parameter aliasmap param), attrs

and dealias_producers aliasmap producers =
  List.map (dealias_producer aliasmap) producers

and dealias_branch aliasmap (branch : parameterized_branch) =
  { branch with pr_producers = dealias_producers aliasmap branch.pr_producers }

and dealias_branches aliasmap branches =
  List.map (dealias_branch aliasmap) branches

let dealias_rule aliasmap rule =
  { rule with pr_branches = dealias_branches aliasmap rule.pr_branches }

let dealias_decl aliasmap (decl : declaration Positions.located) =
  Positions.pmap (fun pos (decl : declaration) ->
    match decl with
    | DCode _
    | DParameter _
    | DToken _
    | DStart _
    | DGrammarAttribute _ ->
        decl
    | DTokenProperties (t, assoc, prec) ->
        DTokenProperties (dealias_terminal aliasmap pos t, assoc, prec)
    | DType (ty, param) ->
        DType (ty, dealias_parameter aliasmap param)
    | DSymbolAttributes (params, attrs) ->
        DSymbolAttributes (dealias_parameters aliasmap params, attrs)
    | DOnErrorReduce (param, level) ->
        DOnErrorReduce (dealias_parameter aliasmap param, level)
  ) decl

let dealias_grammar aliasmap g =
  { g with
    pg_declarations = List.map (dealias_decl aliasmap) g.pg_declarations;
    pg_rules = List.map (dealias_rule aliasmap) g.pg_rules }

let dealias_grammars aliasmap gs =
  List.map (dealias_grammar aliasmap) gs

(* -------------------------------------------------------------------------- *)

(* The two phases above are combined as follows. *)

let dealias_grammars gs =
  let aliasmap = collect_aliases_from_grammars gs in
  dealias_grammars aliasmap gs
