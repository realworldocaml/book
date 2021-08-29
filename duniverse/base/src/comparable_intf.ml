open! Import

module type Infix = Comparisons.Infix
module type Polymorphic_compare = Comparisons.S

module type Validate = sig
  type t

  val validate_lbound : min:t Maybe_bound.t -> t Validate.check
  val validate_ubound : max:t Maybe_bound.t -> t Validate.check
  val validate_bound : min:t Maybe_bound.t -> max:t Maybe_bound.t -> t Validate.check
end

module type With_zero = sig
  type t

  val validate_positive : t Validate.check
  val validate_non_negative : t Validate.check
  val validate_negative : t Validate.check
  val validate_non_positive : t Validate.check
  val is_positive : t -> bool
  val is_non_negative : t -> bool
  val is_negative : t -> bool
  val is_non_positive : t -> bool

  (** Returns [Neg], [Zero], or [Pos] in a way consistent with the above functions. *)
  val sign : t -> Sign0.t
end

module type S = sig
  include Polymorphic_compare


  (** [ascending] is identical to [compare]. [descending x y = ascending y x].  These are
      intended to be mnemonic when used like [List.sort ~compare:ascending] and [List.sort
      ~cmp:descending], since they cause the list to be sorted in ascending or descending
      order, respectively. *)
  val ascending : t -> t -> int

  val descending : t -> t -> int

  (** [between t ~low ~high] means [low <= t <= high] *)
  val between : t -> low:t -> high:t -> bool

  (** [clamp_exn t ~min ~max] returns [t'], the closest value to [t] such that
      [between t' ~low:min ~high:max] is true.

      Raises if [not (min <= max)]. *)
  val clamp_exn : t -> min:t -> max:t -> t

  val clamp : t -> min:t -> max:t -> t Or_error.t

  include Comparator.S with type t := t
  include Validate with type t := t
end

(** Usage example:

    {[
      module Foo : sig
        type t = ...
        include Comparable.S with type t := t
      end
    ]}

    Then use [Comparable.Make] in the struct (see comparable.mli for an example). *)

module type Comparable = sig
  (** Defines functors for making modules comparable. *)


  (** Usage example:

      {[
        module Foo = struct
          module T = struct
            type t = ... [@@deriving compare, sexp]
          end
          include T
          include Comparable.Make (T)
        end
      ]}

      Then include [Comparable.S] in the signature

      {[
        module Foo : sig
          type t = ...
          include Comparable.S with type t := t
        end
      ]}

      To add an [Infix] submodule:

      {[
        module C = Comparable.Make (T)
        include C
        module Infix = (C : Comparable.Infix with type t := t)
      ]}

      A common pattern is to define a module [O] with a restricted signature. It aims to be
      (locally) opened to bring useful operators into scope without shadowing unexpected
      variable names. E.g., in the [Date] module:

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
      ]} *)

  module type Infix = Infix
  module type S = S
  module type Polymorphic_compare = Polymorphic_compare
  module type Validate = Validate
  module type With_zero = With_zero

  (** [lexicographic cmps x y] compares [x] and [y] lexicographically using functions in the
      list [cmps]. *)
  val lexicographic : ('a -> 'a -> int) list -> 'a -> 'a -> int

  (** [lift cmp ~f x y] compares [x] and [y] by comparing [f x] and [f y] via [cmp]. *)
  val lift : ('a -> 'a -> 'result) -> f:('b -> 'a) -> 'b -> 'b -> 'result

  (** [reverse cmp x y = cmp y x]

      Reverses the direction of asymmetric relations by swapping their arguments. Useful,
      e.g., for relations implementing "is a subset of" or "is a descendant of".

      Where reversed relations are already provided, use them directly. For example,
      [Comparable.S] provides [ascending] and [descending], which are more readable as a
      pair than [compare] and [reverse compare]. Similarly, [<=] is more idiomatic than
      [reverse (>=)]. *)
  val reverse : ('a -> 'a -> 'result) -> 'a -> 'a -> 'result

  (** The functions below are analogues of the type-specific functions exported by the
      [Comparable.S] interface. *)

  val equal : ('a -> 'a -> int) -> 'a -> 'a -> bool
  val max : ('a -> 'a -> int) -> 'a -> 'a -> 'a
  val min : ('a -> 'a -> int) -> 'a -> 'a -> 'a

  (** Derive [Infix] or [Polymorphic_compare] functions from just [[@@deriving compare]],
      without need for the [sexp_of_t] required by [Make*] (see below). *)

  module Infix (T : sig
      type t [@@deriving_inline compare]

      val compare : t -> t -> int

      [@@@end]
    end) : Infix with type t := T.t

  module Polymorphic_compare (T : sig
      type t [@@deriving_inline compare]

      val compare : t -> t -> int

      [@@@end]
    end) : Polymorphic_compare with type t := T.t

  (** Inherit comparability from a component. *)
  module Inherit (C : sig
      type t [@@deriving_inline compare]

      val compare : t -> t -> int

      [@@@end]
    end) (T : sig
            type t [@@deriving_inline sexp_of]

            val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

            [@@@end]

            val component : t -> C.t
          end) : S with type t := T.t

  module Make (T : sig
      type t [@@deriving_inline compare, sexp_of]

      val compare : t -> t -> int
      val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

      [@@@end]
    end) : S with type t := T.t

  module Make_using_comparator (T : sig
      type t [@@deriving_inline sexp_of]

      val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

      [@@@end]

      include Comparator.S with type t := t
    end) : S with type t := T.t with type comparator_witness := T.comparator_witness

  module Poly (T : sig
      type t [@@deriving_inline sexp_of]

      val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

      [@@@end]
    end) : S with type t := T.t

  module Validate (T : sig
      type t [@@deriving_inline compare, sexp_of]

      val compare : t -> t -> int
      val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

      [@@@end]
    end) : Validate with type t := T.t

  module With_zero (T : sig
      type t [@@deriving_inline compare, sexp_of]

      val compare : t -> t -> int
      val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

      [@@@end]

      val zero : t

      include Validate with type t := t
    end) : With_zero with type t := T.t

  module Validate_with_zero (T : sig
      type t [@@deriving_inline compare, sexp_of]

      val compare : t -> t -> int
      val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

      [@@@end]

      val zero : t
    end) : sig
    include Validate with type t := T.t
    include With_zero with type t := T.t
  end
end
