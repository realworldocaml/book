open! Import

(* A [Time_ns] that uses its alternate sexp representation. *)
include (
  Time_ns :
    module type of struct
    include Time_ns
  end)

include (
  Alternate_sexp :
  sig
    type t [@@deriving sexp]
  end
  with type t := t)
