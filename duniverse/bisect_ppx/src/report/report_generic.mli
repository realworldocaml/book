(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



(** This module defines a generic output mode parametrized by an
    object. *)


class type converter =
  object
    method header : string
    (** Should return the overall header for output. *)

    method footer : string
    (** Should return the overall footer for output. *)

    method summary : Report_utils.counts -> string
    (** Should return the overall summary for passed statistics. *)

    method file_header : string -> string
    (** Should return the header for passed file. *)

    method file_footer : string -> string
    (** Should return the footer for passed file. *)

    method file_summary : Report_utils.counts -> string
    (** Should return the file summary for passed statistics. *)

    method point : int -> int -> string
    (** [point o n k] should return the output for a given point, [o]
        being the offset, and [n] the number of visits. *)
  end
(** The class type defining a generic output. *)

val output :
  (string -> unit) -> string -> converter -> (string, int array) Hashtbl.t ->
  (string, string) Hashtbl.t ->
    unit
(** [output verbose file conv data points] writes the element for [data] to file
    [file] using [conv] for data conversion, [verbose] for verbose output.
    [points] gives the marshalled locations of the points in the file. *)
