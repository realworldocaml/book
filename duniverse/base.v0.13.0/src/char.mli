(** A type for 8-bit characters. *)

open! Import

(** An alias for the type of characters. *)
type t = char [@@deriving_inline enumerate, hash, sexp]
include
  sig
    [@@@ocaml.warning "-32"]
    val all : t list
    val hash_fold_t :
      Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
    val hash : t -> Ppx_hash_lib.Std.Hash.hash_value
    include Ppx_sexp_conv_lib.Sexpable.S with type  t :=  t
  end[@@ocaml.doc "@inline"]
[@@@end]

include Identifiable.S with type t := t
module O : Comparisons.Infix with type t := t

(** Returns the ASCII code of the argument. *)
val to_int : t -> int

(** Returns the character with the given ASCII code or [None] is the argument is outside
    the range 0 to 255. *)
val of_int : int -> t option

(** Returns the character with the given ASCII code. Raises [Failure] if the argument is
    outside the range 0 to 255. *)
val of_int_exn : int -> t

val unsafe_of_int : int -> t

(** Returns a string representing the given character, with special characters escaped
    following the lexical conventions of OCaml. *)
val escaped : t -> string

(** Converts the given character to its equivalent lowercase character. *)
val lowercase : t -> t

(** Converts the given character to its equivalent uppercase character. *)
val uppercase : t -> t

(** '0' - '9' *)
val is_digit : t -> bool

(** 'a' - 'z' *)
val is_lowercase : t -> bool

(** 'A' - 'Z' *)
val is_uppercase : t -> bool

(** 'a' - 'z' or 'A' - 'Z' *)
val is_alpha : t -> bool

(** 'a' - 'z' or 'A' - 'Z' or '0' - '9' *)
val is_alphanum : t -> bool

(** ' ' - '~' *)
val is_print : t -> bool

(** ' ' or '\t' or '\r' or '\n' *)
val is_whitespace : t -> bool

(** Returns [Some i] if [is_digit c] and [None] otherwise. *)
val get_digit : t -> int option

(** Returns [i] if [is_digit c] and raises [Failure] otherwise. *)
val get_digit_exn : t -> int

val min_value : t
val max_value : t
