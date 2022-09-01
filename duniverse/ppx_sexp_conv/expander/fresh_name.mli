(** Represents freshly generated names at ppx expansion time. *)

open! Base
open Ppxlib

type t

(** Creates a new fresh name using the given string as a prefix. *)
val create : string -> loc:location -> t

(** [of_string_loc { loc; txt }] is equivalent to [create txt ~loc] *)
val of_string_loc : string loc -> t

(** Extracts the freshly created name and its location. *)
val to_string_loc : t -> string loc

(** Constructs an expression referring to the fresh name. *)
val expression : t -> expression

(** Constructs a pattern binding the fresh name. *)
val pattern : t -> pattern
