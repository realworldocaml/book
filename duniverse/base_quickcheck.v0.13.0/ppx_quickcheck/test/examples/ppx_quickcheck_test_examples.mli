open Base
open Base_quickcheck

module Simple_reference : sig
  type t = bool [@@deriving quickcheck]
end

module Dotted_reference : sig
  type t = Simple_reference.t [@@deriving quickcheck]
end

module Nonrec_reference : sig
  open Dotted_reference

  type nonrec t = t [@@deriving quickcheck]
end

module Application_of_polymorphic_type : sig
  type t = bool option [@@deriving quickcheck]
end

module Tuple : sig
  type t = bool * unit option [@@deriving quickcheck]
end

module Poly_variant : sig
  type t =
    [ `A
    | `B
    | `C of bool
    | `D of bool
    | `E of bool * unit option
    | `F of bool * unit option
    ]
  [@@deriving quickcheck]
end

module Inherit_poly_variant : sig
  type t =
    [ `X
    | Poly_variant.t
    | `Z of unit option
    ]
  [@@deriving quickcheck]
end

module Record_type : sig
  type t =
    { x : bool
    ; y : unit option
    }
  [@@deriving quickcheck]
end

module Nullary_and_unary_variant : sig
  type t =
    | A
    | B
    | C of unit
    | D of unit
  [@@deriving quickcheck]
end

module Binary_and_record_variant : sig
  type t =
    | A of bool * [ `X | `Y | `Z of unit ]
    | B of bool * [ `X | `Y | `Z of unit ]
    | C of
        { x : unit option
        ; mutable y : bool
        }
    | D of
        { x : unit option
        ; mutable y : bool
        }
  [@@deriving quickcheck]
end

module Simple_arrow : sig
  type t = unit option -> bool [@@deriving quickcheck]
end

module Named_arrow : sig
  type t = x:unit option -> bool [@@deriving quickcheck]
end

module Optional_arrow : sig
  type t = ?x:unit option -> unit -> bool [@@deriving quickcheck]
end

module Curried_arrow : sig
  type t = unit option -> bool option -> bool [@@deriving quickcheck]
end

module Simple_higher_order : sig
  type t = (unit option -> bool option) -> bool [@@deriving quickcheck]
end

module Named_higher_order : sig
  type t = (x:unit option -> bool option) -> bool [@@deriving quickcheck]
end

module Optional_higher_order : sig
  type t = (?x:unit option -> unit -> bool option) -> bool [@@deriving quickcheck]
end

module Poly_unary : sig
  type 'a t = 'a list [@@deriving quickcheck]
end

module Instance_of_unary : sig
  type t = bool Poly_unary.t [@@deriving quickcheck]
end

module Poly_binary : sig
  type ('a, 'b) t = 'a * 'b [@@deriving quickcheck]
end

module Instance_of_binary : sig
  type t = (bool, unit option) Poly_binary.t [@@deriving quickcheck]
end

module Poly_with_variance : sig
  type (-'a, +'b) t = 'b * ('a -> 'b) [@@deriving quickcheck]
end

module Instance_with_variance : sig
  type t = (bool, unit option) Poly_with_variance.t [@@deriving quickcheck]
end

module Poly_with_phantom : sig
  type _ t [@@deriving quickcheck]
end
with type _ t = unit option

module Instance_with_phantom : sig
  type t = [ `phantom ] Poly_with_phantom.t [@@deriving quickcheck]
end

module Recursive : sig
  type t =
    | Leaf
    | Node of t * t
  [@@deriving quickcheck]
end

module Recursive_with_indirect_base_case : sig
  type t = { children : t list } [@@deriving quickcheck]
end

module Mutually_recursive : sig
  type expr =
    | Constant of int64
    | Operator of op
    | Application of expr * args

  and op =
    [ `plus
    | `minus
    | `abs
    ]

  and args = expr list [@@deriving quickcheck]
end

module Extensions : sig
  type t =
    [ `A
    | `B of bool * unit option
    ]

  val quickcheck_generator : t Generator.t
  val quickcheck_observer : t Observer.t
  val quickcheck_shrinker : t Shrinker.t
end

module Escaped : sig
  type t = int * char * bool option

  val quickcheck_generator : t Generator.t
  val quickcheck_observer : t Observer.t
  val quickcheck_shrinker : t Shrinker.t
end

module Wildcard (Elt : sig
    type t

    val examples : t list
  end) : sig
  type t = Elt.t list [@@deriving quickcheck]
end

module Attribute_override : sig
  type t =
    | Null
    | Text of string
    | Number of float
  [@@deriving quickcheck]
end

module Attribute_override_recursive : sig
  type t =
    | Leaf
    | Node1 of t * int64 * t
    | Node2 of t * int64 * t * int64 * t
  [@@deriving quickcheck]
end

module Deriving_from_wildcard : sig
  type _ transparent = string [@@deriving quickcheck]
  type _ opaque [@@deriving quickcheck]

  val compare_opaque : ('a -> 'a -> int) -> 'a opaque -> 'a opaque -> int
  val sexp_of_opaque : ('a -> Sexp.t) -> 'a opaque -> Sexp.t
  val opaque_examples : int64 opaque list
end
