ppx_variants_conv
=================

Generation of accessor and iteration functions for ocaml variant types.

`ppx_variants_conv` is a ppx rewriter that can be used to define first
class values representing variant constructors, and additional
routines to fold, iterate and map over all constructors of a variant
type.

It provides corresponding functionality for variant types as
`ppx_fields_conv` provides for record types.

Basic use of `[@@deriving variants]` and variantslib
----------------------------------------------------

This code:

```ocaml
type 'a t =
  | A of 'a
  | B of char
  | C
  | D of int * int
  [@@deriving variants]
```

generates the following values:

```ocaml
(** first-class constructor functions *)
val a : 'a -> 'a t
val b : char -> 'a t
val c : 'a t
val d : int -> int -> 'a t

(** higher order variants and functions over all variants *)
module Variants : sig
  val a : ('a -> 'a t)         Variant.t
  val b : (char -> 'a t)       Variant.t
  val c : ('a t)               Variant.t
  val d : (int -> int -> 'a t) Variant.t

  val fold :
    init: 'b
    -> a:('b -> ('a -> 'a t)         Variant.t -> 'c)
    -> b:('c -> (char -> 'a t)       Variant.t -> 'd)
    -> c:('d -> ('a t)               Variant.t -> 'e)
    -> d:('e -> (int -> int -> 'a t) Variant.t -> 'f)
    -> 'f

  val iter :
       a: (('a -> 'a t)         Variant.t -> unit)
    -> b: ((char -> 'a t)       Variant.t -> unit)
    -> c: (('a t)               Variant.t -> unit)
    -> d: ((int -> int -> 'a t) Variant.t -> unit)
    -> unit

  val map :
    'a t
    -> a: (('a -> 'a t)         Variant.t -> 'a                 -> 'r)
    -> b: ((char -> 'a t)       Variant.t -> char               -> 'r)
    -> c: (('a t)               Variant.t                       -> 'r)
    -> d: ((int -> int -> 'a t) Variant.t -> int -> int -> 'a t -> 'r)
    -> 'r

  val make_matcher :
       a:(('a -> 'a t)         Variant.t -> 'b -> ('c -> 'd)         * 'e)
    -> b:((char -> 'f t)       Variant.t -> 'e -> (char -> 'd)       * 'g)
    -> c:('h t                 Variant.t -> 'g -> (unit -> 'd)       * 'i)
    -> d:((int -> int -> 'j t) Variant.t -> 'i -> (int -> int -> 'd) * 'k)
    -> 'b
    -> ('c t -> 'd) * 'k

  val to_rank : _ t -> int
  val to_name : _ t -> string

  (** name * number of arguments, ie [("A", 1); ("B", 1); ("C", 0); ("D", 2)]. *)
  val descriptions : (string * int) list
end
```

Variant.t is defined in Variantslib as follows:

```ocaml
module Variant = struct
  type 'constructor t = {
    name : string;
    (* the position of the constructor in the type definition, starting from 0 *)
    rank : int;
    constructor : 'constructor
  }
end
```

The fold, iter, and map functions are useful in dealing with the totality of variants.
For example, to get a list of all variants when all the constructors are nullary:

```ocaml
type t =
  | First
  | Second
  | Third
  [@@deriving variants]
```

```ocaml
let all =
  let add acc var = var.Variantslib.Variant.constructor :: acc in
  Variants.fold ~init:[]
    ~first:add
    ~second:add
    ~third:add
```

Just like with `[@@deriving fields]`, if the type changes, the
compiler will complain until this definition is updated as well.

`ppx_variant_libs` works similarly on simple polymorphic variants
(without row variables and without inclusion).
