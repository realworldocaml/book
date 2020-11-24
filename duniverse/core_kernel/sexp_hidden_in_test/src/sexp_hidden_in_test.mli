open! Core_kernel

(** ['a t] is a type that uses [Core_kernel.am_running_test] to determine if it should
    use the ['a] sexp serializer, or serialize the type as '<hidden_in_test>'.
    It can be thought of as a form of [@sexp.opaque] that is conditional upon if tests are
    running. *)
type 'a t = 'a [@@deriving bin_io, compare, sexp_of]

(** This type also derives [sexp]. This will not allow you to roundtrip values you create
    in tests and should be used carefully. *)
module With_non_roundtripping_in_test_of_sexp : sig
  type 'a t = 'a [@@deriving bin_io, compare, sexp]
end
