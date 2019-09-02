(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



(** This module defines utility functions for the report program. *)


val url : string
(** The Bisect URL, as a string. *)

module Infix :
sig
  val (++) : int -> int -> int
  (** Similar to [(+)] except that overflow is handled by returning:
      - [max_int] if the result should be above [max_int];
      - [min_int] if the result should be below [min_int]. *)

  val (--) : int -> int -> int
  (** Similar to [(-)] except that overflow is handled by returning:
      - [max_int] if the result should be above [max_int];
      - [min_int] if the result should be below [min_int]. *)

  val (+|) : int array -> int array -> int array
  (** Returns the sum of the passed arrays, using [(++)] to sum elements.
      The length of the returned array is the maximum of the lengths of
      the passed arrays, missing elements from the smallest array being
      supposed to be equal to [0]. *)

  val (-|) : int array -> int array -> int array
  (** Returns the difference of the passed arrays, using [(--)] to subtract
      elements. The length of the returned array is the maximum of the
      lengths of the passed arrays, missing elements from the smallest
      array being supposed to be equal to [0]. *)
end

val mkdirs : ?perm:Unix.file_perm -> string -> unit
(** Creates the directory whose path is passed, and all necessary parent
    directories. The optional [perms] parameter indicates the permissions
    used for directory creation(s), defaulting to [0o755].

    Raises [Unix.Unix_error] if creation fails. *)

val split : ('a -> bool) -> ('a list) -> 'a list * 'a list
(** [split p [e1; ...; en]] returns [([e1; ...; e(i-1)], [ei; ...; en])]
    where [i] is the lowest index such that [p ei] evaluates to false. *)

val split_after : int -> ('a list) -> 'a list * 'a list
(** [split_after k [e1; ...; en]] returns [([e1; ...; ek], [e(k+1); ...; en])]. *)

val open_both : string -> string -> in_channel * out_channel
(** [open_both in_file out_file] return a [(i, o)] couple where:
    - [i] is an input channel for [in_file];
    - [o] is an output channel for [out_file].

    Raises an exception if an error occurs; ensures that files are either
    both opened or both closed. *)

val output_strings : string list -> (string * string) list -> out_channel -> unit
(** [output_strings lines mapping ch] writes the elements of [lines]
    to the channel [ch]. Each line is written after substituting
    {i $(xyz)} sequences as described by [Buffer.add_substitute]. The
    substitution is based on the association list [mapping]; if no mapping
    is found, [""] is used.

    Raises an exception if an error occurs. *)

val output_bytes : int array -> string -> unit
(** [output_bytes data filename] creates the file [filename] and writes
    the bytes from [data] to it. Each array element is considered as a
    byte value.

    Raises an exception if an error occurs. *)

val current_time : unit -> string
(** Returns the current time as a string, using the following format:
    ["2001-01-01 01:01:01"]. *)

(** Types and functions related to visitation counts.
    All operations gracefully handle overflows by ensuring that:
    - a value above [max_int] is encoded by [max_int];
    - a value below [min_int] is encoded by [min_int]. *)

type counts = {
    mutable visited : int; (** Number of points actually visited. *)
    mutable total : int (** Total number of points. *)
  }
(** The type of visitation count statistics. These are used for each file, and
    for the whole project. *)

val make : unit -> counts
(** Evaluates to [{visited = 0; total = 0}]. *)

val update : counts -> bool -> unit
(** [update counts v] updates [counts]. [counts.total] is always incremented,
    while [counts.visited] is incremented iff [v] equals [true]. *)

val add : counts -> counts -> counts
(** [add x y] returns the sum of counts [x] and [y]. *)

val sum : counts list -> counts
(** [sum l] is a fold over [l] elements with function [add],
    using the value returned by [make] as the initial value. *)
