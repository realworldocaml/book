(** This module defines the integer type used in all languages. *)

type t = Int64.t

(** {2 Basic Values} *)

val zero : t

val one : t

(** {2 Arithmetic Operations} *)

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

(** {2 Conversions} *)

exception DoesNotFit

val of_int : int -> t

val to_int : t -> int

val of_string : string -> t

val to_string : t -> string

(** {2 Serialization} *)

val t_of_sexp : Sexplib.Sexp.t -> t

val sexp_of_t : t -> Sexplib.Sexp.t

(** {2 Low-level information} *)

val size_in_bytes : int
