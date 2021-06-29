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

(* The type of the buffer is [(position * position) buffer], which means that
   it stores two pairs of positions, which are the start and end positions of
   the last two tokens. *)

open Lexing

val wrap:
  (lexbuf -> 'token) ->
  (position * position) buffer * (lexbuf -> 'token)

val wrap_supplier:
  (unit -> 'token * position * position) ->
  (position * position) buffer * (unit -> 'token * position * position)

(* [show f buffer] prints the contents of the buffer, producing a string that
   is typically of the form "after '%s' and before '%s'". The function [f] is
   used to print an element. The buffer MUST be nonempty. *)

val show: ('a -> string) -> 'a buffer -> string

(* [last buffer] returns the last element of the buffer. The buffer MUST be
   nonempty. *)

val last: 'a buffer -> 'a

(* -------------------------------------------------------------------------- *)

(* [extract text (pos1, pos2)] extracts the sub-string of [text] delimited
   by the positions [pos1] and [pos2]. *)

val extract: string -> position * position -> string

(* [sanitize text] eliminates any special characters from the text [text].
   A special character is a character whose ASCII code is less than 32.
   Every special character is replaced with a single space character. *)

val sanitize: string -> string

(* [compress text] replaces every run of at least one whitespace character
   with exactly one space character. *)

val compress: string -> string

(* [shorten k text] limits the length of [text] to [2k+3] characters. If the
   text is too long, a fragment in the middle is replaced with an ellipsis. *)

val shorten: int -> string -> string

(* [expand f text] searches [text] for occurrences of [$k], where [k]
   is a nonnegative integer literal, and replaces each such occurrence
   with the string [f k]. *)

val expand: (int -> string) -> string -> string
