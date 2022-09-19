open! Core

type t =
  | Krb
  | Krb_test_mode
  | Rpc
[@@deriving compare, enumerate, sexp]

val magic_number : t -> int
val by_magic_number : t Int.Map.t

(** The number of bytes in the bin_io representation of a [magic_number t]. All magic
    numbers are represented in this fixed number of bytes. *)
val magic_number_bin_size : int
