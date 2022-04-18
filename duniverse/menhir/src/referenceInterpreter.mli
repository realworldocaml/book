(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Grammar
open Cst
open Logging

(* This reference interpreter animates the LR automaton. It uses the grammar
   and automaton descriptions, as provided by [Grammar] and [Lr1], as well as
   the generic LR engine in [MenhirLib.Engine]. *)

(* [interpret nt log lexer lexbuf] requires a start symbol [nt], a set of
   logging hooks [log] that can be used to produce a trace, a lexer [lexer],
   and a lexing buffer [lexbuf]. It either succeeds and returns [Some cst],
   where [cst] is a concrete syntax tree, or fails and returns [None]. *)

val interpret:
  Nonterminal.t ->
  log ->
  (Lexing.lexbuf -> Terminal.t) ->
  Lexing.lexbuf ->
  cst option

(* This variant of the reference interpreter is used internally by us. We use
   it to debug [LRijkstra]. It checks that a sentence leads to a syntax error
   in the expected state. It is also used by several of the command line
   options [--interpret-error], [--compile-errors], etc. *)

type spurious_reduction =
  Lr1.node * Production.index

type target =
  Lr1.node * spurious_reduction list

type check_error_path_outcome =
  (* Bad: the input was read past its end. *)
| OInputReadPastEnd
  (* Bad: a syntax error occurred before all of the input was read. *)
| OInputNotFullyConsumed
  (* Bad: the parser unexpectedly accepted (part of) this input. *)
| OUnexpectedAccept
  (* Good: a syntax error occurred after reading the last input token. We
     report in which state the error took place, as well as a list of spurious
     reductions. A non-default reduction that takes place after looking at the
     last input token (i.e., the erroneous token) is spurious. Furthermore, any
     reduction that takes place after a spurious reduction is itself spurious.
     We note that a spurious reduction can take place only in a non-canonical
     LR automaton. *)
| OK of target

val check_error_path:
  log ->             (* logging hooks *)
  Nonterminal.t ->   (* initial non-terminal symbol *)
  Terminal.t list -> (* input  *)
  check_error_path_outcome
