open! Import

type ('a, 'witness) t =
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

let make (type t) ~compare ~sexp_of_t =
  (module struct
    type comparable_t = t
    type comparator_witness

    let comparator = { compare; sexp_of_t }
  end : S_fc
    with type comparable_t = t)
;;

module S_to_S1 (S : S) = struct
  type 'a t = S.t
  type comparator_witness = S.comparator_witness

  open S

  let comparator = comparator
end

module Make (M : sig
    type t [@@deriving_inline compare, sexp_of]

    val compare : t -> t -> int
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]
  end) =
struct
  include M

  type comparator_witness

  let comparator = M.{ compare; sexp_of_t }
end

module Make1 (M : sig
    type 'a t

    val compare : 'a t -> 'a t -> int
    val sexp_of_t : 'a t -> Sexp.t
  end) =
struct
  type comparator_witness

  let comparator = M.{ compare; sexp_of_t }
end

module Poly = struct
  type 'a t = 'a

  include Make1 (struct
      type 'a t = 'a

      let compare = Poly.compare
      let sexp_of_t _ = Sexp.Atom "_"
    end)
end

module type Derived = sig
  type 'a t
  type 'cmp comparator_witness

  val comparator : ('a, 'cmp) comparator -> ('a t, 'cmp comparator_witness) comparator
end

module Derived (M : sig
    type 'a t [@@deriving_inline compare, sexp_of]

    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val sexp_of_t : ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]
  end) =
struct
  type 'cmp comparator_witness

  let comparator a =
    { compare = M.compare a.compare; sexp_of_t = M.sexp_of_t a.sexp_of_t }
  ;;
end

module type Derived2 = sig
  type ('a, 'b) t
  type ('cmp_a, 'cmp_b) comparator_witness

  val comparator
    :  ('a, 'cmp_a) comparator
    -> ('b, 'cmp_b) comparator
    -> (('a, 'b) t, ('cmp_a, 'cmp_b) comparator_witness) comparator
end

module Derived2 (M : sig
    type ('a, 'b) t [@@deriving_inline compare, sexp_of]

    val compare : ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int

    val sexp_of_t
      :  ('a -> Ppx_sexp_conv_lib.Sexp.t)
      -> ('b -> Ppx_sexp_conv_lib.Sexp.t)
      -> ('a, 'b) t
      -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]
  end) =
struct
  type ('cmp_a, 'cmp_b) comparator_witness

  let comparator a b =
    { compare = M.compare a.compare b.compare
    ; sexp_of_t = M.sexp_of_t a.sexp_of_t b.sexp_of_t
    }
  ;;
end

module type Derived_phantom = sig
  type ('a, 'b) t
  type 'cmp comparator_witness

  val comparator
    :  ('a, 'cmp) comparator
    -> (('a, _) t, 'cmp comparator_witness) comparator
end

module Derived_phantom (M : sig
    type ('a, 'b) t

    val compare : ('a -> 'a -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
    val sexp_of_t : ('a -> Sexp.t) -> ('a, _) t -> Sexp.t
  end) =
struct
  type 'cmp_a comparator_witness

  let comparator a =
    { compare = M.compare a.compare; sexp_of_t = M.sexp_of_t a.sexp_of_t }
  ;;
end
