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

open Lexing

(* [init filename lexbuf] initializes the lexing buffer [lexbuf] so
   that the positions that are subsequently read from it refer to the
   file [filename]. It returns [lexbuf]. *)

val init: string -> lexbuf -> lexbuf

(* [read filename] reads the entire contents of the file [filename] and
   returns a pair of this content (a string) and a lexing buffer that
   has been initialized, based on this string. *)

val read: string -> string * lexbuf

(* [newline lexbuf] increments the line counter stored within [lexbuf]. It
   should be invoked by the lexer itself every time a newline character is
   consumed. This allows maintaining a current the line number in [lexbuf]. *)

val newline: lexbuf -> unit

(* [range (startpos, endpos)] prints a textual description of the range
   delimited by the start and end positions [startpos] and [endpos].
   This description is one line long and ends in a newline character.
   This description mentions the file name, the line number, and a range
   of characters on this line. The line number is correct only if [newline]
   has been correctly used, as described dabove. *)

val range: position * position -> string
