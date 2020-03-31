(** This module extends {{!Base.Char}[Base.Char]}, adding [Identifiable] for making char
    identifiers and [Quickcheckable] to facilitate automated testing with pseudorandom
    data.
*)

type t = char [@@deriving typerep]

(** {2 The signature included from [Base.Char]} *)

(** @open *)
include module type of struct
  include Base.Char
end
with type t := t

(** {2 Extensions} *)

include Identifiable.S with type t := t and type comparator_witness := comparator_witness

(** {3 Quickcheck Support} *)

include Quickcheckable.S with type t := t

val gen_digit : t Quickcheck.Generator.t
val gen_lowercase : t Quickcheck.Generator.t
val gen_uppercase : t Quickcheck.Generator.t
val gen_alpha : t Quickcheck.Generator.t
val gen_alphanum : t Quickcheck.Generator.t
val gen_print : t Quickcheck.Generator.t
val gen_whitespace : t Quickcheck.Generator.t

(** Generates characters between the given inclusive bounds in ASCII order. Raises if
    bounds are in decreasing order. *)
val gen_uniform_inclusive : t -> t -> t Quickcheck.Generator.t
