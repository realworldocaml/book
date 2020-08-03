open Base
open Base_quickcheck

module Simple_reference = struct
  type t = bool [@@deriving quickcheck]
end

module Dotted_reference = struct
  type t = Simple_reference.t [@@deriving quickcheck]
end

module Nonrec_reference = struct
  open Dotted_reference

  type nonrec t = t [@@deriving quickcheck]
end

module Application_of_polymorphic_type = struct
  type t = bool option [@@deriving quickcheck]
end

module Tuple = struct
  type t = bool * unit option [@@deriving quickcheck]
end

module Poly_variant = struct
  (* deliberately make pairs of isomorphic tags to make sure we hash tags properly *)
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

module Inherit_poly_variant = struct
  type t =
    [ `X
    | Poly_variant.t
    | `Z of unit option
    ]
  [@@deriving quickcheck]
end

module Record_type = struct
  type t =
    { x : bool
    ; y : unit option
    }
  [@@deriving quickcheck]
end

module Nullary_and_unary_variant = struct
  (* deliberately make pairs of isomorphic tags to make sure we hash tags properly *)
  type t =
    | A
    | B
    | C of unit
    | D of unit
  [@@deriving quickcheck]
end

module Binary_and_record_variant = struct
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

module Simple_arrow = struct
  type t = unit option -> bool [@@deriving quickcheck]
end

module Named_arrow = struct
  type t = x:unit option -> bool [@@deriving quickcheck]
end

module Optional_arrow = struct
  type t = ?x:unit option -> unit -> bool [@@deriving quickcheck]
end

module Curried_arrow = struct
  type t = unit option -> bool option -> bool [@@deriving quickcheck]
end

module Simple_higher_order = struct
  type t = (unit option -> bool option) -> bool [@@deriving quickcheck]
end

module Named_higher_order = struct
  type t = (x:unit option -> bool option) -> bool [@@deriving quickcheck]
end

module Optional_higher_order = struct
  type t = (?x:unit option -> unit -> bool option) -> bool [@@deriving quickcheck]
end

module Poly_unary = struct
  type 'a t = 'a list [@@deriving quickcheck]
end

module Instance_of_unary = struct
  type t = bool Poly_unary.t [@@deriving quickcheck]
end

module Poly_binary = struct
  type ('a, 'b) t = 'a * 'b [@@deriving quickcheck]
end

module Instance_of_binary = struct
  type t = (bool, unit option) Poly_binary.t [@@deriving quickcheck]
end

module Poly_with_variance = struct
  type (-'a, +'b) t = 'b * ('a -> 'b) [@@deriving quickcheck]
end

module Instance_with_variance = struct
  type t = (bool, unit option) Poly_with_variance.t

  (* We cannot use [@@deriving quickcheck] here because ppx_quickcheck cannot tell the
     [bool] argument needs to swap generators with observers. *)
  let quickcheck_generator =
    Poly_with_variance.quickcheck_generator
      quickcheck_observer_bool
      (quickcheck_generator_option quickcheck_generator_unit)
  ;;

  let quickcheck_observer =
    Poly_with_variance.quickcheck_observer
      quickcheck_generator_bool
      (quickcheck_observer_option quickcheck_observer_unit)
  ;;

  let quickcheck_shrinker =
    Poly_with_variance.quickcheck_shrinker
      quickcheck_shrinker_bool
      (quickcheck_shrinker_option quickcheck_shrinker_unit)
  ;;

  let _quickcheck_generator_with_custom =
    [%quickcheck.generator: [ `Foo of [%custom quickcheck_generator] ]]
  ;;
end

module Poly_with_phantom = struct
  type _ t = unit option [@@deriving quickcheck]
end

module Instance_with_phantom = struct
  type t = [ `phantom ] Poly_with_phantom.t [@@deriving quickcheck]
end

module Recursive = struct
  type t =
    | Leaf
    | Node of t * t
  [@@deriving quickcheck]
end

module Recursive_with_indirect_base_case = struct
  type t = { children : t list } [@@deriving quickcheck]
end

module Mutually_recursive = struct
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

module Extensions = struct
  type t =
    [ `A
    | `B of bool * unit option
    ]

  let quickcheck_generator = [%quickcheck.generator: [ `A | `B of bool * unit option ]]
  let quickcheck_observer = [%quickcheck.observer: [ `A | `B of bool * unit option ]]
  let quickcheck_shrinker = [%quickcheck.shrinker: [ `A | `B of bool * unit option ]]
end

module Escaped = struct
  type t = int * char * bool option

  let quickcheck_generator =
    [%quickcheck.generator:
      [%custom Generator.small_strictly_positive_int] * char * bool option]
  ;;

  let quickcheck_observer =
    [%quickcheck.observer: int * [%custom Observer.opaque] * bool option]
  ;;

  let quickcheck_shrinker =
    [%quickcheck.shrinker: int * char * [%custom Shrinker.atomic]]
  ;;
end

module Wildcard (Elt : sig
    type t

    val examples : t list
  end) =
struct
  type t = Elt.t list

  let quickcheck_generator = Generator.list (Generator.of_list Elt.examples)
  let quickcheck_observer : t Observer.t = [%quickcheck.observer: _ list]
  let quickcheck_shrinker : t Shrinker.t = [%quickcheck.shrinker: _ list]
end

module Attribute_override = struct
  type t =
    | Null [@quickcheck.weight 0.1]
    | Text of
        (string[@quickcheck.generator Generator.string_of Generator.char_lowercase])
    | Number of (float[@quickcheck.generator Generator.float_strictly_positive])
  [@@deriving quickcheck]
end

module Attribute_override_recursive = struct
  type t =
    | Leaf
    | Node1 of t * int64 * t [@quickcheck.weight 0.5]
    | Node2 of t * int64 * t * int64 * t [@quickcheck.weight 0.25]
  [@@deriving quickcheck]
end

module Deriving_from_wildcard = struct
  type _ transparent = string [@@deriving quickcheck]
  type 'a opaque = 'a option [@@deriving quickcheck]

  let compare_opaque = compare_option
  let sexp_of_opaque = sexp_of_option
  let opaque_examples = [ None; Some 0L; Some 1L ]
end
