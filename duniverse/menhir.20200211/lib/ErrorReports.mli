(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)

(* The following functions help keep track of the start and end positions of
   the last two tokens in a two-place buffer. This is used to nicely display
   where a syntax error took place. *)

type 'a buffer

(* [wrap lexer] returns a pair of a new (initially empty) buffer and a lexer
   which internally relies on [lexer] and updates [buffer] on the fly whenever
   a token is demanded. *)

open Lexing

val wrap:
  (lexbuf -> 'token) ->
  (position * position) buffer * (lexbuf -> 'token)

(* [show f buffer] prints the contents of the buffer, producing a string that
   is typically of the form "after '%s' and before '%s'". The function [f] is
   used to print an element. The buffer MUST be nonempty. *)

val show: ('a -> string) -> 'a buffer -> string

(* [last buffer] returns the last element of the buffer. The buffer MUST be
   nonempty. *)

val last: 'a buffer -> 'a

(* -------------------------------------------------------------------------- *)
