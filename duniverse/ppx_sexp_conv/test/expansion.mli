open! Base

module Abstract : sig
  type t [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Tuple : sig
  type t = int * int * int [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Record : sig
  type t =
    { a : int
    ; b : int
    ; c : int
    }
  [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Mutable_record : sig
  type t =
    { mutable a : int
    ; mutable b : int
    ; mutable c : int
    }
  [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Variant : sig
  type t =
    | A
    | B of int * int
    | C of
        { a : int
        ; b : int
        ; d : int
        }
    | D of
        { mutable a : int
        ; mutable b : int
        ; mutable t : int
        }
  [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Poly_variant : sig
  type t =
    [ `A
    | `B of int
    ]
  [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Sexplib0.Sexp.t
    val t_of_sexp : Sexplib0.Sexp.t -> t
    val __t_of_sexp__ : Sexplib0.Sexp.t -> t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Inline_poly_variant : sig
  type t =
    [ Poly_variant.t
    | `C of int * int
    ]
  [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Sexplib0.Sexp.t
    val t_of_sexp : Sexplib0.Sexp.t -> t
    val __t_of_sexp__ : Sexplib0.Sexp.t -> t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Recursive : sig
  type t =
    | Banana of t
    | Orange
  [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Nonrecursive : sig
  open Recursive

  type nonrec t = t [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Mutually_recursive : sig
  type a =
    | A
    | B of b
    | C of
        { a : a
        ; b : b
        ; c : c
        }

  and b =
    { a : a
    ; b : b
    }

  and c = int [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_a : a -> Sexplib0.Sexp.t
    val sexp_of_b : b -> Sexplib0.Sexp.t
    val sexp_of_c : c -> Sexplib0.Sexp.t
    val a_of_sexp : Sexplib0.Sexp.t -> a
    val b_of_sexp : Sexplib0.Sexp.t -> b
    val c_of_sexp : Sexplib0.Sexp.t -> c
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Alias : sig
  type t = Recursive.t [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Re_export : sig
  type t = Recursive.t =
    | Banana of t
    | Orange
  [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Unary : sig
  type 'a t = 'a list option [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S1 with type 'a t := 'a t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Binary : sig
  type ('a, 'b) t = ('a, 'b) Either.t [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S2 with type ('a, 'b) t := ('a, 'b) t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module First_order : sig
  type 'a t = 'a -> 'a [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S1 with type 'a t := 'a t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Second_order : sig
  type ('a, 'b) t = ('a -> 'a) -> ('a -> 'b) -> ('b -> 'b) -> 'a -> 'b
  [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S2 with type ('a, 'b) t := ('a, 'b) t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Named_arguments : sig
  type t = ?a:int -> b:int -> int -> int [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Gadt : sig
  type _ t =
    | A : _ option t
    | B : int -> int t
    | C : 'a list -> unit t
  [@@deriving_inline sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : ('a__001_ -> Sexplib0.Sexp.t) -> 'a__001_ t -> Sexplib0.Sexp.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Recursive_record_containing_variant : sig
  type t =
    { a : [ `A of t ]
    ; b : [ `B ]
    }
  [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Poly_record : sig
  type t =
    { a : 'a. 'a list
    ; b : 'b. 'b option
    ; c : 'c. 'c
    }
  [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Record_with_defaults : sig
  type t =
    { a : int
    ; b : int
    ; c : int
    ; d : int
    ; e : int
    ; f : int
    }
  [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Record_with_special_types : sig
  type t =
    { a : int option
    ; b : int list
    ; c : int array
    ; d : bool
    }
  [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Record_with_omit_nil : sig
  type t =
    { a : int option
    ; b : int list
    ; c : unit
    ; d : int
    }
  [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Variant_with_sexp_list : sig
  type t = A of int list [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Poly_variant_with_sexp_list : sig
  type t = [ `A of int list ] [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Sexplib0.Sexp.t
    val t_of_sexp : Sexplib0.Sexp.t -> t
    val __t_of_sexp__ : Sexplib0.Sexp.t -> t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Record_allowing_extra_fields : sig
  type t = { a : int } [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Opaque : sig
  type t = int list [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end
