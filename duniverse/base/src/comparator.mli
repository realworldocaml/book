(** A type-indexed value that allows one to compare (and for generating error messages,
    serialize) values of the type in question.

    One of the type parameters is a phantom parameter used to distinguish comparators
    potentially built on different comparison functions.  In particular, we want to
    distinguish those using polymorphic compare from those using a monomorphic compare. *)

open! Import

type ('a, 'witness) t = private
  { compare : 'a -> 'a -> int
  ; sexp_of_t : 'a -> Sexp.t
  }

type ('a, 'b) comparator = ('a, 'b) t

module type S = sig
  type t
  type comparator_witness

  val comparator : (t, comparator_witness) comparator
end

module type S1 = sig
  type 'a t
  type comparator_witness

  val comparator : ('a t, comparator_witness) comparator
end


module type S_fc = sig
  type comparable_t

  include S with type t := comparable_t
end

(** [make] creates a comparator witness for the given comparison. It is intended as a
    lightweight alternative to the functors below, to be used like so:
    [include (val Comparator.make ~compare ~sexp_of_t)] *)
val make
  :  compare:('a -> 'a -> int)
  -> sexp_of_t:('a -> Sexp.t)
  -> (module S_fc with type comparable_t = 'a)

module Poly : S1 with type 'a t = 'a

module S_to_S1 (S : S) :
  S1 with type 'a t = S.t with type comparator_witness = S.comparator_witness

(** [Make] creates a [comparator] value and its phantom [comparator_witness] type for a
    nullary type. *)
module Make (M : sig
    type t [@@deriving_inline compare, sexp_of]

    val compare : t -> t -> int
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]
  end) : S with type t := M.t

(** [Make1] creates a [comparator] value and its phantom [comparator_witness] type for a
    unary type.  It takes a [compare] and [sexp_of_t] that have
    non-standard types because the [Comparator.t] type doesn't allow passing in
    additional values for the type argument. *)
module Make1 (M : sig
    type 'a t

    val compare : 'a t -> 'a t -> int
    val sexp_of_t : _ t -> Sexp.t
  end) : S1 with type 'a t := 'a M.t

module type Derived = sig
  type 'a t
  type 'cmp comparator_witness

  val comparator : ('a, 'cmp) comparator -> ('a t, 'cmp comparator_witness) comparator
end

(** [Derived] creates a [comparator] function that constructs a comparator for the type
    ['a t] given a comparator for the type ['a]. *)
module Derived (M : sig
    type 'a t [@@deriving_inline compare, sexp_of]

    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val sexp_of_t : ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]
  end) : Derived with type 'a t := 'a M.t

module type Derived2 = sig
  type ('a, 'b) t
  type ('cmp_a, 'cmp_b) comparator_witness

  val comparator
    :  ('a, 'cmp_a) comparator
    -> ('b, 'cmp_b) comparator
    -> (('a, 'b) t, ('cmp_a, 'cmp_b) comparator_witness) comparator
end

(** [Derived2] creates a [comparator] function that constructs a comparator for the type
    [('a, 'b) t] given comparators for the type ['a] and ['b]. *)
module Derived2 (M : sig
    type ('a, 'b) t [@@deriving_inline compare, sexp_of]

    val compare : ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int

    val sexp_of_t
      :  ('a -> Ppx_sexp_conv_lib.Sexp.t)
      -> ('b -> Ppx_sexp_conv_lib.Sexp.t)
      -> ('a, 'b) t
      -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]
  end) : Derived2 with type ('a, 'b) t := ('a, 'b) M.t

module type Derived_phantom = sig
  type ('a, 'b) t
  type 'cmp comparator_witness

  val comparator
    :  ('a, 'cmp) comparator
    -> (('a, _) t, 'cmp comparator_witness) comparator
end

(** [Derived_phantom] creates a [comparator] function that constructs a comparator for the
    type [('a, 'b) t] given a comparator for the type ['a]. *)
module Derived_phantom (M : sig
    type ('a, 'b) t

    val compare : ('a -> 'a -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
    val sexp_of_t : ('a -> Sexp.t) -> ('a, _) t -> Sexp.t
  end) : Derived_phantom with type ('a, 'b) t := ('a, 'b) M.t
