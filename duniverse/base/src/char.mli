(** A type for 8-bit characters. *)

open! Import

(** An alias for the type of characters. *)
type t = char [@@deriving_inline enumerate, sexp, sexp_grammar]

include Ppx_enumerate_lib.Enumerable.S with type t := t
include Sexplib0.Sexpable.S with type t := t

val t_sexp_grammar : t Sexplib0.Sexp_grammar.t

[@@@end]

include Identifiable.S with type t := t
include Invariant.S with type t := t
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

(** '0' - '9' or 'a' - 'f' or 'A' - 'F' *)
val is_hex_digit : t -> bool

(** '0' - '9' or 'a' - 'f' *)
val is_hex_digit_lower : t -> bool

(** '0' - '9' or 'A' - 'F' *)
val is_hex_digit_upper : t -> bool

(** Returns [Some i] where [0 <= i && i < 16] if [is_hex_digit c] and [None] otherwise. *)
val get_hex_digit : t -> int option

(** Same as [get_hex_digit] but raises instead of returning None. *)
val get_hex_digit_exn : t -> int

val min_value : t
val max_value : t

(** [Caseless] compares and hashes characters ignoring case, so that for example
    [Caseless.equal 'A' 'a'] and [Caseless.('a' < 'B')] are [true]. *)
module Caseless : sig
  type nonrec t = t [@@deriving_inline hash, sexp, sexp_grammar]

  include Ppx_hash_lib.Hashable.S with type t := t
  include Sexplib0.Sexpable.S with type t := t

  val t_sexp_grammar : t Sexplib0.Sexp_grammar.t

  [@@@end]

  include Comparable.S with type t := t
end
