(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open StackLang

(** The type [measure] can be used to measure the size of a StackLang program
    or to keep track of dynamic instruction execution counts while a StackLang
    program is executed. *)
type measure = {
  mutable push: int;
  mutable pop: int;
  mutable peek: int;
  mutable def: int;
  mutable prim: int;
  mutable trace: int;
  mutable dead: int;
  mutable stop: int;
  mutable return: int;
  mutable jump: int;
  mutable casetoken: int;
  mutable casetag: int;
  mutable total: int;
}

val zero : unit -> measure
(** [zero()] returns a fresh [measure] record. *)

val adjust_total : measure -> unit
(** [adjust_total] sets [m.total] to the sum of the other fields in [m]. *)

val measure : program -> measure
(** [measure program] computes static instruction counts for the program
    [program]. This information is intended to be used for debugging and
    engineering purposes. *)

val print : out_channel -> measure -> unit
(** [print c m] print the measure [m] to the output channel [c]. *)
