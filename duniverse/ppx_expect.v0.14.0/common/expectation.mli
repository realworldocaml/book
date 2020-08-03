open! Base
open Import

module Body : sig
  type 'a t =
    | Exact of string
    | Output
    | Pretty of 'a
    | Unreachable
  [@@deriving_inline sexp_of, compare, equal]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val map_pretty : 'a t -> f:('a -> 'b) -> 'b t
end

type 'a t =
  { tag : string option (** Tag of the string payload *)
  ; body : 'a Body.t
  ; extid_location : File.Location.t
  (** Location of the extension id ("expect" or
      "expect_exact") *)
  ; body_location : File.Location.t
  (** Location of the string payload of the extension
      point *)
  }
[@@deriving_inline sexp_of, compare, equal]

include sig
  [@@@ocaml.warning "-32"]

  val sexp_of_t : ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end
[@@ocaml.doc "@inline"]

[@@@end]

module Raw : sig
  type nonrec t = string t [@@deriving_inline sexp_of, compare]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val compare : t -> t -> int
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

val map_pretty : 'a t -> f:('a -> 'b) -> 'b t
