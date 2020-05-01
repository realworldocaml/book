open! Core_kernel
open! Import

module type Slots = sig
  (** [Slots] has types [t1], ..., [t12] of arities 1 to 12 that are isomorphic to tuple
      types of the corresponding arities.  Type [('a0, ..., 'a<N-1>) t<N>] corresponds to
      ['a0 * ... * 'a<N-1>].

      Each type [ti] is an instance of type [('tuple, 'variant) t], in which ['tuple] is
      the tuple type ['a0 * ... * 'a<N-1>] and ['variant] is an encoding of the tuple type
      in the form: [[ `S0 of `a0 | `S1 of `a1 | ... | `S<N-1> of `a<N-1> ]].

      The encoding of the slots using a polymorphic variant allows one to write functions
      that are polymorphic in the tuple type, and require that a tuple have a certain
      slot, but allow more slots.

      We make [t] itself a polymorphic variant type so that one can easily encode cyclic
      types, e.g. lists, like:

      {[
        type 'a slots = ('a, 'a slots Pointer.t) Slots.t2
      ]}

      Observe that [slots] in the above is cyclic, but that OCaml allows it because the
      definition expands to:

      {[
        type 'a slots = [ `Slots of ('a * 'a slots Pointer.t,
                                     [ `S0 of 'a
                                     | `S1 of 'a slots Pointer.t
                                     ]
                                    ) u
                        ]
      ]}

      Ultimately, a [Slots.t] is used as a phantom type that ensures consistent usage of
      the tuples in the data structure containing them. *)

  type ('tuple, 'variant) u
  type ('tuple, 'variant) t = [ `Slots of ('tuple, 'variant) u ] [@@deriving sexp_of]

  val slots_per_tuple : (_, _) t -> int

  type 'a0 t1 = ('a0, [ `S0 of 'a0 ]) t [@@deriving sexp_of]
  type ('a0, 'a1) t2 = ('a0 * 'a1, [ `S0 of 'a0 | `S1 of 'a1 ]) t [@@deriving sexp_of]

  type ('a0, 'a1, 'a2) t3 = ('a0 * 'a1 * 'a2, [ `S0 of 'a0 | `S1 of 'a1 | `S2 of 'a2 ]) t
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3) t4 =
    ('a0 * 'a1 * 'a2 * 'a3, [ `S0 of 'a0 | `S1 of 'a1 | `S2 of 'a2 | `S3 of 'a3 ]) t
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4) t5 =
    ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4
    , [ `S0 of 'a0 | `S1 of 'a1 | `S2 of 'a2 | `S3 of 'a3 | `S4 of 'a4 ] )
      t
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5) t6 =
    ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5
    , [ `S0 of 'a0 | `S1 of 'a1 | `S2 of 'a2 | `S3 of 'a3 | `S4 of 'a4 | `S5 of 'a5 ] )
      t
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6) t7 =
    ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6
    , [ `S0 of 'a0
      | `S1 of 'a1
      | `S2 of 'a2
      | `S3 of 'a3
      | `S4 of 'a4
      | `S5 of 'a5
      | `S6 of 'a6
      ] )
      t
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) t8 =
    ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7
    , [ `S0 of 'a0
      | `S1 of 'a1
      | `S2 of 'a2
      | `S3 of 'a3
      | `S4 of 'a4
      | `S5 of 'a5
      | `S6 of 'a6
      | `S7 of 'a7
      ] )
      t
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) t9 =
    ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8
    , [ `S0 of 'a0
      | `S1 of 'a1
      | `S2 of 'a2
      | `S3 of 'a3
      | `S4 of 'a4
      | `S5 of 'a5
      | `S6 of 'a6
      | `S7 of 'a7
      | `S8 of 'a8
      ] )
      t
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9) t10 =
    ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9
    , [ `S0 of 'a0
      | `S1 of 'a1
      | `S2 of 'a2
      | `S3 of 'a3
      | `S4 of 'a4
      | `S5 of 'a5
      | `S6 of 'a6
      | `S7 of 'a7
      | `S8 of 'a8
      | `S9 of 'a9
      ] )
      t
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10) t11 =
    ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10
    , [ `S0 of 'a0
      | `S1 of 'a1
      | `S2 of 'a2
      | `S3 of 'a3
      | `S4 of 'a4
      | `S5 of 'a5
      | `S6 of 'a6
      | `S7 of 'a7
      | `S8 of 'a8
      | `S9 of 'a9
      | `S10 of 'a10
      ] )
      t
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10, 'a11) t12 =
    ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11
    , [ `S0 of 'a0
      | `S1 of 'a1
      | `S2 of 'a2
      | `S3 of 'a3
      | `S4 of 'a4
      | `S5 of 'a5
      | `S6 of 'a6
      | `S7 of 'a7
      | `S8 of 'a8
      | `S9 of 'a9
      | `S10 of 'a10
      | `S11 of 'a11
      ] )
      t
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10, 'a11, 'a12) t13 =
    ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11 * 'a12
    , [ `S0 of 'a0
      | `S1 of 'a1
      | `S2 of 'a2
      | `S3 of 'a3
      | `S4 of 'a4
      | `S5 of 'a5
      | `S6 of 'a6
      | `S7 of 'a7
      | `S8 of 'a8
      | `S9 of 'a9
      | `S10 of 'a10
      | `S11 of 'a11
      | `S12 of 'a12
      ] )
      t
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10, 'a11, 'a12, 'a13) t14 =
    ( 'a0
      * 'a1
      * 'a2
      * 'a3
      * 'a4
      * 'a5
      * 'a6
      * 'a7
      * 'a8
      * 'a9
      * 'a10
      * 'a11
      * 'a12
      * 'a13
    , [ `S0 of 'a0
      | `S1 of 'a1
      | `S2 of 'a2
      | `S3 of 'a3
      | `S4 of 'a4
      | `S5 of 'a5
      | `S6 of 'a6
      | `S7 of 'a7
      | `S8 of 'a8
      | `S9 of 'a9
      | `S10 of 'a10
      | `S11 of 'a11
      | `S12 of 'a12
      | `S13 of 'a13
      ] )
      t
  [@@deriving sexp_of]

  val t1 : _ t1
  val t2 : (_, _) t2
  val t3 : (_, _, _) t3
  val t4 : (_, _, _, _) t4
  val t5 : (_, _, _, _, _) t5
  val t6 : (_, _, _, _, _, _) t6
  val t7 : (_, _, _, _, _, _, _) t7
  val t8 : (_, _, _, _, _, _, _, _) t8
  val t9 : (_, _, _, _, _, _, _, _, _) t9
  val t10 : (_, _, _, _, _, _, _, _, _, _) t10
  val t11 : (_, _, _, _, _, _, _, _, _, _, _) t11
  val t12 : (_, _, _, _, _, _, _, _, _, _, _, _) t12
  val t13 : (_, _, _, _, _, _, _, _, _, _, _, _, _) t13
  val t14 : (_, _, _, _, _, _, _, _, _, _, _, _, _, _) t14
end

module type Slot = sig
  (** A [Slot.t] represents a slot in a tuple type. *)
  type ('variant, 'a) t [@@deriving sexp_of]

  val equal : ('v, 'a) t -> ('v, 'a) t -> bool

  (** [ti] is the [i]'th slot. *)

  val t0 : ([> `S0 of 'a ], 'a) t
  val t1 : ([> `S1 of 'a ], 'a) t
  val t2 : ([> `S2 of 'a ], 'a) t
  val t3 : ([> `S3 of 'a ], 'a) t
  val t4 : ([> `S4 of 'a ], 'a) t
  val t5 : ([> `S5 of 'a ], 'a) t
  val t6 : ([> `S6 of 'a ], 'a) t
  val t7 : ([> `S7 of 'a ], 'a) t
  val t8 : ([> `S8 of 'a ], 'a) t
  val t9 : ([> `S9 of 'a ], 'a) t
  val t10 : ([> `S10 of 'a ], 'a) t
  val t11 : ([> `S11 of 'a ], 'a) t
  val t12 : ([> `S12 of 'a ], 'a) t
  val t13 : ([> `S13 of 'a ], 'a) t
end
