open! Import
include Comparable_intf

module Validate (T : sig
    type t [@@deriving_inline compare, sexp_of]

    val compare : t -> t -> int
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]
  end) : Validate with type t := T.t = struct
  module V = Validate
  open Maybe_bound

  let to_string t = Sexp.to_string (T.sexp_of_t t)

  let validate_bound ~min ~max t =
    V.bounded ~name:to_string ~lower:min ~upper:max ~compare:T.compare t
  ;;

  let validate_lbound ~min t = validate_bound ~min ~max:Unbounded t
  let validate_ubound ~max t = validate_bound ~max ~min:Unbounded t
end

module With_zero (T : sig
    type t [@@deriving_inline compare]

    val compare : t -> t -> int

    [@@@end]

    val zero : t

    include Validate with type t := t
  end) =
struct
  open T

  (* Preallocate the interesting bounds to minimize allocation in the implementations of
     [validate_*]. *)
  let excl_zero = Maybe_bound.Excl zero
  let incl_zero = Maybe_bound.Incl zero
  let validate_positive t = validate_lbound ~min:excl_zero t
  let validate_non_negative t = validate_lbound ~min:incl_zero t
  let validate_negative t = validate_ubound ~max:excl_zero t
  let validate_non_positive t = validate_ubound ~max:incl_zero t
  let is_positive t = compare t zero > 0
  let is_non_negative t = compare t zero >= 0
  let is_negative t = compare t zero < 0
  let is_non_positive t = compare t zero <= 0
  let sign t = Sign0.of_int (compare t zero)
end

module Validate_with_zero (T : sig
    type t [@@deriving_inline compare, sexp_of]

    val compare : t -> t -> int
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]

    val zero : t
  end) =
struct
  module V = Validate (T)
  include V

  include With_zero (struct
      include T
      include V
    end)
end

module Poly (T : sig
    type t [@@deriving_inline sexp_of]

    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]
  end) =
struct
  module Replace_polymorphic_compare = struct
    type t = T.t [@@deriving_inline sexp_of]

    let sexp_of_t = (T.sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t)

    [@@@end]

    include Poly
  end

  include Poly

  let between t ~low ~high = low <= t && t <= high
  let clamp_unchecked t ~min ~max = if t < min then min else if t <= max then t else max

  let clamp_exn t ~min ~max =
    assert (min <= max);
    clamp_unchecked t ~min ~max
  ;;

  let clamp t ~min ~max =
    if min > max
    then
      Or_error.error_s
        (Sexp.message
           "clamp requires [min <= max]"
           [ "min", T.sexp_of_t min; "max", T.sexp_of_t max ])
    else Ok (clamp_unchecked t ~min ~max)
  ;;

  module C = struct
    include T
    include Comparator.Make (Replace_polymorphic_compare)
  end

  include C

  include Validate (struct
      type nonrec t = t [@@deriving_inline compare, sexp_of]

      let compare = (compare : t -> t -> int)
      let sexp_of_t = (sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t)

      [@@@end]
    end)
end

let gt cmp a b = cmp a b > 0
let lt cmp a b = cmp a b < 0
let geq cmp a b = cmp a b >= 0
let leq cmp a b = cmp a b <= 0
let equal cmp a b = cmp a b = 0
let not_equal cmp a b = cmp a b <> 0
let min cmp t t' = if leq cmp t t' then t else t'
let max cmp t t' = if geq cmp t t' then t else t'

module Infix (T : sig
    type t [@@deriving_inline compare]

    val compare : t -> t -> int

    [@@@end]
  end) : Infix with type t := T.t = struct
  let ( > ) a b = gt T.compare a b
  let ( < ) a b = lt T.compare a b
  let ( >= ) a b = geq T.compare a b
  let ( <= ) a b = leq T.compare a b
  let ( = ) a b = equal T.compare a b
  let ( <> ) a b = not_equal T.compare a b
end

module Polymorphic_compare (T : sig
    type t [@@deriving_inline compare]

    val compare : t -> t -> int

    [@@@end]
  end) : Polymorphic_compare with type t := T.t = struct
  include Infix (T)

  let compare = T.compare
  let equal = ( = )
  let min t t' = min compare t t'
  let max t t' = max compare t t'
end

module Make_using_comparator (T : sig
    type t [@@deriving_inline sexp_of]

    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]

    include Comparator.S with type t := t
  end) : S with type t := T.t and type comparator_witness = T.comparator_witness = struct
  module T = struct
    include T

    let compare = comparator.compare
  end

  include T
  module Replace_polymorphic_compare = Polymorphic_compare (T)
  include Replace_polymorphic_compare

  let ascending = compare
  let descending t t' = compare t' t
  let between t ~low ~high = low <= t && t <= high
  let clamp_unchecked t ~min ~max = if t < min then min else if t <= max then t else max

  let clamp_exn t ~min ~max =
    assert (min <= max);
    clamp_unchecked t ~min ~max
  ;;

  let clamp t ~min ~max =
    if min > max
    then
      Or_error.error_s
        (Sexp.message
           "clamp requires [min <= max]"
           [ "min", T.sexp_of_t min; "max", T.sexp_of_t max ])
    else Ok (clamp_unchecked t ~min ~max)
  ;;

  include Validate (T)
end

module Make (T : sig
    type t [@@deriving_inline compare, sexp_of]

    val compare : t -> t -> int
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]
  end) =
  Make_using_comparator (struct
    include T
    include Comparator.Make (T)
  end)

module Inherit (C : sig
    type t [@@deriving_inline compare]

    val compare : t -> t -> int

    [@@@end]
  end) (T : sig
          type t [@@deriving_inline sexp_of]

          val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

          [@@@end]

          val component : t -> C.t
        end) =
  Make (struct
    type t = T.t [@@deriving_inline sexp_of]

    let sexp_of_t = (T.sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t)

    [@@@end]

    let compare t t' = C.compare (T.component t) (T.component t')
  end)

(* compare [x] and [y] lexicographically using functions in the list [cmps] *)
let lexicographic cmps x y =
  let rec loop = function
    | cmp :: cmps ->
      let res = cmp x y in
      if res = 0 then loop cmps else res
    | [] -> 0
  in
  loop cmps
;;

let lift cmp ~f x y = cmp (f x) (f y)
let reverse cmp x y = cmp y x
