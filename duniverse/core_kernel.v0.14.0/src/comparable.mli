(** Comparable extends {{!Base.Comparable}[Base.Comparable]} and provides functions for
    comparing like types.

    Usage example:

    {[
      module Foo = struct
        module T = struct
          type t = ... [@@deriving compare, sexp]
        end
        include T
        include Comparable.Make (T)
      end
    ]}

    Then include [Comparable.S] in the signature (see {!Comparable_intf} for an
    example).

    To add an [Infix] submodule:

    {[
      module C = Comparable.Make (T)
      include C
      module Infix = (C : Comparable.Infix with type t := t)
    ]}

    Common pattern: Define a module [O] with a restricted signature.  It aims to be
    (locally) opened to bring useful operators into scope without shadowing unexpected
    variable names.  E.g. in the [Date] module:

    {[
      module O = struct
        include (C : Comparable.Infix with type t := t)
        let to_string t = ..
      end
    ]}

    Opening [Date] would shadow [now], but opening [Date.O] doesn't:

    {[
      let now = .. in
      let someday = .. in
      Date.O.(now > someday)
    ]}
*)

open Comparable_intf

module type Infix = Infix
module type Map_and_set_binable = Map_and_set_binable
module type Polymorphic_compare = Polymorphic_compare
module type S_plain = S_plain
module type S = S
module type S_binable = S_binable
module type S_common = S_common
module type Validate = Validate
module type With_zero = With_zero

(** [lexicographic cmps x y] compares [x] and [y] lexicographically using functions in the
    list [cmps]. *)
val lexicographic : ('a -> 'a -> int) list -> 'a -> 'a -> int

(** [lift cmp ~f x y] compares [x] and [y] by comparing [f x] and [f y] via [cmp]. *)
val lift : ('a -> 'a -> 'int_or_bool) -> f:('b -> 'a) -> 'b -> 'b -> 'int_or_bool

(** [reverse cmp x y = cmp y x]

    Note: The [Comparable.S] interface exports both [ascending] and [descending]
    comparisons, so in most cases, it's better to use those. *)
val reverse : ('a -> 'a -> 'int_or_bool) -> 'a -> 'a -> 'int_or_bool

(** Inherit comparability from a component. *)
module Inherit (C : sig
    type t [@@deriving compare]
  end) (T : sig
          type t [@@deriving sexp]

          val component : t -> C.t
        end) : S with type t := T.t

(** {2 Comparison-only Functors}

    These functors require only [type t] and [val compare]. They do not require [val
    sexp_of_t], and do not generate container datatypes.
*)

module Infix (T : sig
    type t [@@deriving compare]
  end) : Infix with type t := T.t

module Polymorphic_compare (T : sig
    type t [@@deriving compare]
  end) : Polymorphic_compare with type t := T.t

(** {2 Make Functors}

    The Comparable Make functor family allows users to choose among the following
    attributes:

    - [*_using_comparator] or not
    - [*_binable] or not
    - [*_plain] or not

    Thus there are functors like [Make_plain] or [Make_binable_using_comparator], etc.
*)

module Make_plain (T : sig
    type t [@@deriving compare, sexp_of]
  end) : S_plain with type t := T.t

module Make (T : sig
    type t [@@deriving compare, sexp]
  end) : S with type t := T.t

module Make_plain_using_comparator (T : sig
    type t [@@deriving sexp_of]

    include Comparator.S with type t := t
  end) : S_plain with type t := T.t with type comparator_witness := T.comparator_witness

module Make_using_comparator (T : sig
    type t [@@deriving sexp]

    include Comparator.S with type t := t
  end) : S with type t := T.t with type comparator_witness := T.comparator_witness

module Make_binable (T : sig
    type t [@@deriving bin_io, compare, sexp]
  end) : S_binable with type t := T.t

module Make_binable_using_comparator (T : sig
    type t [@@deriving bin_io, sexp]

    include Comparator.S with type t := t
  end) : S_binable with type t := T.t with type comparator_witness := T.comparator_witness

module Extend
    (M : Base.Comparable.S) (X : sig
                               type t = M.t [@@deriving sexp]
                             end) : S with type t := M.t with type comparator_witness := M.comparator_witness

module Extend_binable
    (M : Base.Comparable.S) (X : sig
                               type t = M.t [@@deriving bin_io, sexp]
                             end) :
  S_binable with type t := M.t with type comparator_witness := M.comparator_witness

module Map_and_set_binable (T : sig
    type t [@@deriving bin_io, compare, sexp]
  end) : Map_and_set_binable with type t := T.t

module Map_and_set_binable_using_comparator (T : sig
    type t [@@deriving bin_io, compare, sexp]

    include Comparator.S with type t := t
  end) :
  Map_and_set_binable
  with type t := T.t
  with type comparator_witness := T.comparator_witness

module Poly (T : sig
    type t [@@deriving sexp]
  end) : S with type t := T.t

module Validate (T : sig
    type t [@@deriving compare, sexp]
  end) : Validate with type t := T.t

module With_zero (T : sig
    type t [@@deriving compare, sexp]

    val zero : t

    include Validate with type t := t
  end) : With_zero with type t := T.t

module Validate_with_zero (T : sig
    type t [@@deriving compare, sexp]

    val zero : t
  end) : sig
  include Validate with type t := T.t
  include With_zero with type t := T.t
end

(** The following module types and functors may be used to define stable modules: *)

module Stable : sig
  module V1 : sig
    module type S = sig
      type comparable
      type comparator_witness

      module Map :
        Map.Stable.V1.S
        with type key := comparable
        with type comparator_witness := comparator_witness

      module Set :
        Set.Stable.V1.S
        with type elt := comparable
        with type elt_comparator_witness := comparator_witness
    end

    module Make (X : Stable_module_types.S0) :
      S with type comparable := X.t with type comparator_witness := X.comparator_witness
  end
end
