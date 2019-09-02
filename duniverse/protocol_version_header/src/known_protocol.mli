open! Core_kernel

type t =
  | Krb
  | Krb_test_mode
  | Rpc
[@@deriving compare, enumerate, sexp]

val magic_number : t -> int
val by_magic_number : t Int.Map.t
