(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012-2013 OCamlPro                                          *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 2.1 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

(** Stream with efficient n-lookup *)

open Pos

(** Enhanced tokens *)
type token = {
  region  : Region.t;
  token   : Approx_lexer.token;
  newlines: int;
  between : string Lazy.t;
  substr  : string Lazy.t;
  offset  : int;
}

type t

(** Creates a stream from a string. Make sure you don't change the string
    in-place after calling [of_string], or anything could happen *)
val of_string: ?start_pos:Position.t -> ?start_offset:int -> string -> t

(** Creates a stream from a channel. Better if you don't want to block, but less
    efficient *)
val of_channel: ?start_pos:Position.t -> in_channel -> t

(** Get next token from the filter. Returns None after EOF *)
val next: t -> (token * t) option
