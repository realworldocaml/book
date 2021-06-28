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

(* This module defines the data that is stored in .cmly files. In short, a
   .cmly file contains a value of type [grammar], defined below. *)

(* The type definitions in this module are used by [Cmly_write], which writes
   a .cmly file, and by [Cmly_read], which reads a .cmly file. They should not
   be used anywhere else. *)

(* All entities (terminal symbols, nonterminal symbols, and so on) are
   represented as integers. These integers serve as indices into arrays. This
   enables simple and efficient hashing, comparison, indexing, etc. *)

type terminal    = int
type nonterminal = int
type production  = int
type lr0         = int
type lr1         = int

type ocamltype   = string
type ocamlexpr   = string

type range = {
  r_start: Lexing.position;
  r_end: Lexing.position;
}

type attribute = {
  a_label: string;
  a_payload: string;
  a_position: range;
}

type attributes =
  attribute list

type terminal_def = {
  t_name: string;
  t_kind: [`REGULAR | `ERROR | `EOF | `PSEUDO];
  t_type: ocamltype option;
  t_attributes: attributes;
}

type nonterminal_def = {
  n_name: string;
  n_kind: [`REGULAR | `START];
  n_mangled_name: string;
  n_type: ocamltype option;
  n_positions: range list;
  n_nullable: bool;
  n_first: terminal list;
  n_attributes: attributes;
}

type symbol =
  | T of terminal
  | N of nonterminal

type identifier = string

type action = {
  a_expr: ocamlexpr;
  a_keywords: Keyword.keyword list;
}

type producer_def =
  symbol * identifier * attributes

type production_def = {
  p_kind: [`REGULAR | `START];
  p_lhs: nonterminal;
  p_rhs: producer_def array;
  p_positions: range list;
  p_action: action option;
  p_attributes: attributes;
}

type lr0_state_def = {
  lr0_incoming: symbol option;
  lr0_items: (production * int) list;
}

type lr1_state_def = {
  lr1_lr0: lr0;
  lr1_transitions: (symbol * lr1) list;
  lr1_reductions: (terminal * production list) list;
}

type grammar = {
  g_basename     : string;
  g_preludes     : string list;
  g_postludes    : string list;
  g_terminals    : terminal_def    array;
  g_nonterminals : nonterminal_def array;
  g_productions  : production_def  array;
  g_lr0_states   : lr0_state_def   array;
  g_lr1_states   : lr1_state_def   array;
  g_entry_points : (nonterminal * production * lr1) list;
  g_attributes   : attributes;
  g_parameters   : string list;
}
