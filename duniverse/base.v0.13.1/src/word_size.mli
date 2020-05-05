(** For determining the word size that the program is using. *)

open! Import

type t =
  | W32
  | W64
[@@deriving_inline sexp_of]
include
  sig [@@@ocaml.warning "-32"] val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  end[@@ocaml.doc "@inline"]
[@@@end]

val num_bits : t -> int

(** Returns the word size of this program, not necessarily of the OS. *)
val word_size : t
