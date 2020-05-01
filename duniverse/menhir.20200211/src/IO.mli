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

(* Input-output utilities. *)

(* [try/finally] has the same semantics as in Java. *)

val try_finally : (unit -> 'a) -> (unit -> unit) -> 'a

(* [moving_away filename action] moves the file [filename] away (if it exists),
   performs [action], then moves the file back into place (if it was moved
   away). *)

val moving_away: string -> (unit -> 'a) -> 'a

(* [with_file filename creation action] creates the file [filename] by
   running [creation], then runs [action], and ensures that the file
   is removed in the end. *)

val with_file: string -> (unit -> unit) -> (unit -> 'a) -> 'a

(* [exhaust channel] reads all of the data that's available on [channel].
   It does not assume that the length of the data is known ahead of time.
   It does not close the channel. *)

val exhaust: in_channel -> string

(* [invoke command] invokes an external command (which expects no input)
   and returns its output, if the command succeeds. It returns [None] if
   the command fails. *)

val invoke: string -> string option

(* [read_whole_file filename] reads the file [filename] in text mode and
   returns its contents as a string. *)

val read_whole_file: string -> string

