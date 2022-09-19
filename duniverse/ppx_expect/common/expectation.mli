open! Base
open Base.Exported_for_specific_uses (* for [Ppx_compare_lib] *)

module Body : sig
  type 'a t =
    | Exact of string
    | Output
    | Pretty of 'a
    | Unreachable
  [@@deriving_inline sexp_of, compare, equal]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t

    include Ppx_compare_lib.Comparable.S1 with type 'a t := 'a t
    include Ppx_compare_lib.Equal.S1 with type 'a t := 'a t
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

  val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t

  include Ppx_compare_lib.Comparable.S1 with type 'a t := 'a t
  include Ppx_compare_lib.Equal.S1 with type 'a t := 'a t
end
[@@ocaml.doc "@inline"]

[@@@end]

module Raw : sig
  type nonrec t = string t [@@deriving_inline sexp_of, compare]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Sexplib0.Sexp.t

    include Ppx_compare_lib.Comparable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

val map_pretty : 'a t -> f:('a -> 'b) -> 'b t
