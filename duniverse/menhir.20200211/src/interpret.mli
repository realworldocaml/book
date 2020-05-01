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

(* [run()] is in charge of handling several command line options, namely
   [--interpret], [--interpret-error], [--compile-errors], [--compare-errors].
   If any of these options is present, the execution of Menhir stops here. *)

val run: unit -> unit

(* This default error message is produced by [--list-errors] when it creates a
   [.messages] file, and is recognized by [--compare-errors] when it compares
   two such files. *)

val default_message: string

(* [print_messages_item] displays one data item. The item is of the form [nt,
   sentence, target], which means that beginning at the start symbol [nt], the
   sentence [sentence] ends in an error in the target state given by [target].
   [target] also contains information about which spurious reductions are
   performed at the end. The display obeys the [.messages] file format. *)

open Grammar

val print_messages_item:
  Nonterminal.t * Terminal.t list * ReferenceInterpreter.target -> unit
