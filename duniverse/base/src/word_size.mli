(** For determining the word size that the program is using. *)

open! Import

type t =
  | W32
  | W64
[@@deriving_inline sexp_of]

val sexp_of_t : t -> Sexplib0.Sexp.t

[@@@end]

val num_bits : t -> int

(** Returns the word size of this program, not necessarily of the OS. *)
val word_size : t
