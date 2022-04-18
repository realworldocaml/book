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

(* The signature [MenhirLib.EngineTypes.LOG] defines a set of logging hooks.
   It is specialized here for the types of states, terminals and productions
   used inside Menhir. *)

module type LOG =
  MenhirLib.EngineTypes.LOG
    with type state := Lr1.node
     and type terminal := Terminal.t
     and type production := Production.index

(* The type of a first-class module whose signature is LOG. *)

type log =
  (module LOG)

(* [Make] constructs a set of logging hooks. *)

module Make (X : sig

  (* [show] determines whether the messages must be sent to [stderr]
     or suppressed. *)
  val show : bool

  (* The reference [count] is used to count the number of messages,
     regardless of whether these messages are visible. Each message
     occupies one line. *)
  val count : int ref

end) : LOG

(* [always] is a set of logging hooks that always log messages to the
   standard error channel. *)

val always : log

(* [never] is a set of logging hooks that hide all log messages. *)

val never : log

(* [maybe b] is [if b then always else never]. *)

val maybe : bool -> log
