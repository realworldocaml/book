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

(* This module helps report errors. *)

(* ---------------------------------------------------------------------------- *)

(* A mechanism to turn all display (logging, warnings, errors) on and off. *)

val enable: unit -> unit
val disable: unit -> unit

(* ---------------------------------------------------------------------------- *)

(* Logging and log levels. *)

val logG: int -> (out_channel -> unit) -> unit
val logA: int -> (out_channel -> unit) -> unit
val logC: int -> (out_channel -> unit) -> unit

(* ---------------------------------------------------------------------------- *)

(* Errors and warnings. *)

(* [error ps format ...] displays the list of positions [ps], followed with the
   error message [format ...], and exits. The strings "Error: " and "\n" are
   automatically added at the beginning and end of the error message. The
   message should begin with a lowercase letter and end with a dot. *)

val error: Positions.positions -> ('a, out_channel, unit, 'b) format4 -> 'a

(* [errorp] is like [error], but uses the position range carried by [v]. *)

val errorp: _ Positions.located -> ('a, out_channel, unit, 'b) format4 -> 'a

(* [warning] is like [error], except it does not exit. *)

val warning: Positions.positions -> ('a, out_channel, unit, unit) format4 -> 'a

(* ---------------------------------------------------------------------------- *)

(* Delayed error reports -- where multiple errors can be reported at once. *)

(* A category of errors. *)

type category

(* [new_category()] creates a new category of errors. *)

val new_category: unit -> category

(* [signal category] is like [error], except it does not exit immediately. It
   records the fact that an error of this category has occurred. This can be
   later detected by [exit_if category]. *)

val signal: category -> Positions.positions -> ('a, out_channel, unit, unit) format4 -> 'a

(* [exit_if category] exits with exit code 1 if [signal category] was
   previously called. Together, [signal] and [exit_if] allow reporting
   multiple errors before aborting. *)

val exit_if: category -> unit

(* ---------------------------------------------------------------------------- *)

(* Certain warnings about the grammar can optionally be treated as errors. *)

val grammatical_error: category

(* [grammar_warning] emits a warning or error message, via either [warning] or
   [signal grammatical_error]. It does not stop the program; the client must
   at some point use [exit_if grammatical_error] and stop the program if any
   errors have been reported. *)

val grammar_warning: Positions.positions -> ('a, out_channel, unit, unit) format4 -> 'a
