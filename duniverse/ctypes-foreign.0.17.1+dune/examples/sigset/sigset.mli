(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open PosixTypes
open Ctypes

type t = sigset_t ptr

val t : sigset_t ptr typ

val empty : unit -> t

val full : unit -> t

val add : t -> int -> unit

val del : t -> int -> unit

val mem : t -> int -> bool
