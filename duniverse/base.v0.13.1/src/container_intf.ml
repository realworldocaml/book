(** Provides generic signatures for container data structures.

    These signatures include functions ([iter], [fold], [exists], [for_all], ...) that
    you would expect to find in any container. Used by including [Container.S0] or
    [Container.S1] in the signature for every container-like data structure ([Array],
    [List], [String], ...) to ensure a consistent interface. *)

open! Import

module Export = struct
  (** [Continue_or_stop.t] is used by the [f] argument to [fold_until] in order to
      indicate whether folding should continue, or stop early. *)
  module Continue_or_stop = struct
    type ('a, 'b) t =
      | Continue of 'a
      | Stop of 'b
  end
end

include Export

module type Summable = sig
  type t

  (** The result of summing no values. *)
  val zero : t

  (** An operation that combines two [t]'s and handles [zero + x] by just returning [x],
      as well as in the symmetric case. *)
  val ( + ) : t -> t -> t
end

(** Signature for monomorphic container, e.g., string. *)
module type S0 = sig
  type t
  type elt

  (** Checks whether the provided element is there, using equality on [elt]s. *)
  val mem : t -> elt -> bool

  val length : t -> int
  val is_empty : t -> bool

  (** [iter] must allow exceptions raised in [f] to escape, terminating the iteration
      cleanly.  The same holds for all functions below taking an [f]. *)
  val iter : t -> f:(elt -> unit) -> unit

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t]. *)
  val fold : t -> init:'accum -> f:('accum -> elt -> 'accum) -> 'accum

  (** [fold_result t ~init ~f] is a short-circuiting version of [fold] that runs in the
      [Result] monad.  If [f] returns an [Error _], that value is returned without any
      additional invocations of [f]. *)
  val fold_result
    :  t
    -> init:'accum
    -> f:('accum -> elt -> ('accum, 'e) Result.t)
    -> ('accum, 'e) Result.t

  (** [fold_until t ~init ~f ~finish] is a short-circuiting version of [fold]. If [f]
      returns [Stop _] the computation ceases and results in that value. If [f] returns
      [Continue _], the fold will proceed. If [f] never returns [Stop _], the final result
      is computed by [finish].

      Example:

      {[
        type maybe_negative =
          | Found_negative of int
          | All_nonnegative of { sum : int }

        (** [first_neg_or_sum list] returns the first negative number in [list], if any,
            otherwise returns the sum of the list. *)
        let first_neg_or_sum =
          List.fold_until ~init:0
            ~f:(fun sum x ->
              if x < 0
              then Stop (Found_negative x)
              else Continue (sum + x))
            ~finish:(fun sum -> All_nonnegative { sum })
        ;;

        let x = first_neg_or_sum [1; 2; 3; 4; 5]
        val x : maybe_negative = All_nonnegative {sum = 15}

        let y = first_neg_or_sum [1; 2; -3; 4; 5]
        val y : maybe_negative = Found_negative -3
      ]} *)
  val fold_until
    :  t
    -> init:'accum
    -> f:('accum -> elt -> ('accum, 'final) Continue_or_stop.t)
    -> finish:('accum -> 'final)
    -> 'final

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true]. This is a short-circuiting operation. *)
  val exists : t -> f:(elt -> bool) -> bool

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements. This is a short-circuiting operation. *)
  val for_all : t -> f:(elt -> bool) -> bool

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count : t -> f:(elt -> bool) -> int

  (** Returns the sum of [f i] for all [i] in the container. *)
  val sum : (module Summable with type t = 'sum) -> t -> f:(elt -> 'sum) -> 'sum

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find : t -> f:(elt -> bool) -> elt option

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element.  *)
  val find_map : t -> f:(elt -> 'a option) -> 'a option

  val to_list : t -> elt list
  val to_array : t -> elt array

  (** Returns a min (resp. max) element from the collection using the provided [compare]
      function. In case of a tie, the first element encountered while traversing the
      collection is returned. The implementation uses [fold] so it has the same
      complexity as [fold]. Returns [None] iff the collection is empty. *)
  val min_elt : t -> compare:(elt -> elt -> int) -> elt option

  val max_elt : t -> compare:(elt -> elt -> int) -> elt option
end

module type S0_phantom = sig
  type elt
  type 'a t

  (** Checks whether the provided element is there, using equality on [elt]s. *)
  val mem : _ t -> elt -> bool

  val length : _ t -> int
  val is_empty : _ t -> bool
  val iter : _ t -> f:(elt -> unit) -> unit

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t]. *)
  val fold : _ t -> init:'accum -> f:('accum -> elt -> 'accum) -> 'accum

  (** [fold_result t ~init ~f] is a short-circuiting version of [fold] that runs in the
      [Result] monad.  If [f] returns an [Error _], that value is returned without any
      additional invocations of [f]. *)
  val fold_result
    :  _ t
    -> init:'accum
    -> f:('accum -> elt -> ('accum, 'e) Result.t)
    -> ('accum, 'e) Result.t

  (** [fold_until t ~init ~f ~finish] is a short-circuiting version of [fold]. If [f]
      returns [Stop _] the computation ceases and results in that value. If [f] returns
      [Continue _], the fold will proceed. If [f] never returns [Stop _], the final result
      is computed by [finish].

      Example:

      {[
        type maybe_negative =
          | Found_negative of int
          | All_nonnegative of { sum : int }

        (** [first_neg_or_sum list] returns the first negative number in [list], if any,
            otherwise returns the sum of the list. *)
        let first_neg_or_sum =
          List.fold_until ~init:0
            ~f:(fun sum x ->
              if x < 0
              then Stop (Found_negative x)
              else Continue (sum + x))
            ~finish:(fun sum -> All_nonnegative { sum })
        ;;

        let x = first_neg_or_sum [1; 2; 3; 4; 5]
        val x : maybe_negative = All_nonnegative {sum = 15}

        let y = first_neg_or_sum [1; 2; -3; 4; 5]
        val y : maybe_negative = Found_negative -3
      ]} *)
  val fold_until
    :  _ t
    -> init:'accum
    -> f:('accum -> elt -> ('accum, 'final) Continue_or_stop.t)
    -> finish:('accum -> 'final)
    -> 'final

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true].  This is a short-circuiting operation. *)
  val exists : _ t -> f:(elt -> bool) -> bool

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements.  This is a short-circuiting operation. *)
  val for_all : _ t -> f:(elt -> bool) -> bool

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count : _ t -> f:(elt -> bool) -> int

  (** Returns the sum of [f i] for all [i] in the container. The order in which the
      elements will be summed is unspecified. *)
  val sum : (module Summable with type t = 'sum) -> _ t -> f:(elt -> 'sum) -> 'sum

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find : _ t -> f:(elt -> bool) -> elt option

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element.  *)
  val find_map : _ t -> f:(elt -> 'a option) -> 'a option

  val to_list : _ t -> elt list
  val to_array : _ t -> elt array

  (** Returns a min (resp max) element from the collection using the provided [compare]
      function, or [None] if the collection is empty.  In case of a tie, the first element
      encountered while traversing the collection is returned. *)
  val min_elt : _ t -> compare:(elt -> elt -> int) -> elt option

  val max_elt : _ t -> compare:(elt -> elt -> int) -> elt option
end

(** Signature for polymorphic container, e.g., ['a list] or ['a array]. *)
module type S1 = sig
  type 'a t

  (** Checks whether the provided element is there, using [equal]. *)
  val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool

  val length : 'a t -> int
  val is_empty : 'a t -> bool
  val iter : 'a t -> f:('a -> unit) -> unit

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t]  *)
  val fold : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

  (** [fold_result t ~init ~f] is a short-circuiting version of [fold] that runs in the
      [Result] monad.  If [f] returns an [Error _], that value is returned without any
      additional invocations of [f]. *)
  val fold_result
    :  'a t
    -> init:'accum
    -> f:('accum -> 'a -> ('accum, 'e) Result.t)
    -> ('accum, 'e) Result.t

  (** [fold_until t ~init ~f ~finish] is a short-circuiting version of [fold]. If [f]
      returns [Stop _] the computation ceases and results in that value. If [f] returns
      [Continue _], the fold will proceed. If [f] never returns [Stop _], the final result
      is computed by [finish].

      Example:

      {[
        type maybe_negative =
          | Found_negative of int
          | All_nonnegative of { sum : int }

        (** [first_neg_or_sum list] returns the first negative number in [list], if any,
            otherwise returns the sum of the list. *)
        let first_neg_or_sum =
          List.fold_until ~init:0
            ~f:(fun sum x ->
              if x < 0
              then Stop (Found_negative x)
              else Continue (sum + x))
            ~finish:(fun sum -> All_nonnegative { sum })
        ;;

        let x = first_neg_or_sum [1; 2; 3; 4; 5]
        val x : maybe_negative = All_nonnegative {sum = 15}

        let y = first_neg_or_sum [1; 2; -3; 4; 5]
        val y : maybe_negative = Found_negative -3
      ]} *)
  val fold_until
    :  'a t
    -> init:'accum
    -> f:('accum -> 'a -> ('accum, 'final) Continue_or_stop.t)
    -> finish:('accum -> 'final)
    -> 'final

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true].  This is a short-circuiting operation. *)
  val exists : 'a t -> f:('a -> bool) -> bool

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements.  This is a short-circuiting operation. *)
  val for_all : 'a t -> f:('a -> bool) -> bool

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count : 'a t -> f:('a -> bool) -> int

  (** Returns the sum of [f i] for all [i] in the container. *)
  val sum : (module Summable with type t = 'sum) -> 'a t -> f:('a -> 'sum) -> 'sum

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find : 'a t -> f:('a -> bool) -> 'a option

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element.  *)
  val find_map : 'a t -> f:('a -> 'b option) -> 'b option

  val to_list : 'a t -> 'a list
  val to_array : 'a t -> 'a array

  (** Returns a minimum (resp maximum) element from the collection using the provided
      [compare] function, or [None] if the collection is empty. In case of a tie, the first
      element encountered while traversing the collection is returned. The implementation
      uses [fold] so it has the same complexity as [fold]. *)
  val min_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option

  val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option
end

module type S1_phantom_invariant = sig
  type ('a, 'phantom) t

  (** Checks whether the provided element is there, using [equal]. *)
  val mem : ('a, _) t -> 'a -> equal:('a -> 'a -> bool) -> bool

  val length : (_, _) t -> int
  val is_empty : (_, _) t -> bool
  val iter : ('a, _) t -> f:('a -> unit) -> unit

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t]. *)
  val fold : ('a, _) t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

  (** [fold_result t ~init ~f] is a short-circuiting version of [fold] that runs in the
      [Result] monad.  If [f] returns an [Error _], that value is returned without any
      additional invocations of [f]. *)
  val fold_result
    :  ('a, _) t
    -> init:'accum
    -> f:('accum -> 'a -> ('accum, 'e) Result.t)
    -> ('accum, 'e) Result.t

  (** [fold_until t ~init ~f ~finish] is a short-circuiting version of [fold]. If [f]
      returns [Stop _] the computation ceases and results in that value. If [f] returns
      [Continue _], the fold will proceed. If [f] never returns [Stop _], the final result
      is computed by [finish].

      Example:

      {[
        type maybe_negative =
          | Found_negative of int
          | All_nonnegative of { sum : int }

        (** [first_neg_or_sum list] returns the first negative number in [list], if any,
            otherwise returns the sum of the list. *)
        let first_neg_or_sum =
          List.fold_until ~init:0
            ~f:(fun sum x ->
              if x < 0
              then Stop (Found_negative x)
              else Continue (sum + x))
            ~finish:(fun sum -> All_nonnegative { sum })
        ;;

        let x = first_neg_or_sum [1; 2; 3; 4; 5]
        val x : maybe_negative = All_nonnegative {sum = 15}

        let y = first_neg_or_sum [1; 2; -3; 4; 5]
        val y : maybe_negative = Found_negative -3
      ]} *)
  val fold_until
    :  ('a, _) t
    -> init:'accum
    -> f:('accum -> 'a -> ('accum, 'final) Continue_or_stop.t)
    -> finish:('accum -> 'final)
    -> 'final

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true].  This is a short-circuiting operation. *)
  val exists : ('a, _) t -> f:('a -> bool) -> bool

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements.  This is a short-circuiting operation. *)
  val for_all : ('a, _) t -> f:('a -> bool) -> bool

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count : ('a, _) t -> f:('a -> bool) -> int

  (** Returns the sum of [f i] for all [i] in the container. *)
  val sum : (module Summable with type t = 'sum) -> ('a, _) t -> f:('a -> 'sum) -> 'sum

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find : ('a, _) t -> f:('a -> bool) -> 'a option

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element.  *)
  val find_map : ('a, _) t -> f:('a -> 'b option) -> 'b option

  val to_list : ('a, _) t -> 'a list
  val to_array : ('a, _) t -> 'a array

  (** Returns a min (resp max) element from the collection using the provided [compare]
      function. In case of a tie, the first element encountered while traversing the
      collection is returned. The implementation uses [fold] so it has the same complexity
      as [fold]. Returns [None] iff the collection is empty. *)
  val min_elt : ('a, _) t -> compare:('a -> 'a -> int) -> 'a option

  val max_elt : ('a, _) t -> compare:('a -> 'a -> int) -> 'a option
end

module type S1_phantom = sig
  type ('a, +'phantom) t

  include S1_phantom_invariant with type ('a, 'phantom) t := ('a, 'phantom) t
end

module type Generic = sig
  type 'a t
  type 'a elt

  val length : _ t -> int
  val is_empty : _ t -> bool
  val iter : 'a t -> f:('a elt -> unit) -> unit
  val fold : 'a t -> init:'accum -> f:('accum -> 'a elt -> 'accum) -> 'accum

  val fold_result
    :  'a t
    -> init:'accum
    -> f:('accum -> 'a elt -> ('accum, 'e) Result.t)
    -> ('accum, 'e) Result.t

  val fold_until
    :  'a t
    -> init:'accum
    -> f:('accum -> 'a elt -> ('accum, 'final) Continue_or_stop.t)
    -> finish:('accum -> 'final)
    -> 'final

  val exists : 'a t -> f:('a elt -> bool) -> bool
  val for_all : 'a t -> f:('a elt -> bool) -> bool
  val count : 'a t -> f:('a elt -> bool) -> int
  val sum : (module Summable with type t = 'sum) -> 'a t -> f:('a elt -> 'sum) -> 'sum
  val find : 'a t -> f:('a elt -> bool) -> 'a elt option
  val find_map : 'a t -> f:('a elt -> 'b option) -> 'b option
  val to_list : 'a t -> 'a elt list
  val to_array : 'a t -> 'a elt array
  val min_elt : 'a t -> compare:('a elt -> 'a elt -> int) -> 'a elt option
  val max_elt : 'a t -> compare:('a elt -> 'a elt -> int) -> 'a elt option
end

module type Generic_phantom = sig
  type ('a, 'phantom) t
  type 'a elt

  val length : (_, _) t -> int
  val is_empty : (_, _) t -> bool
  val iter : ('a, _) t -> f:('a elt -> unit) -> unit
  val fold : ('a, _) t -> init:'accum -> f:('accum -> 'a elt -> 'accum) -> 'accum

  val fold_result
    :  ('a, _) t
    -> init:'accum
    -> f:('accum -> 'a elt -> ('accum, 'e) Result.t)
    -> ('accum, 'e) Result.t

  val fold_until
    :  ('a, _) t
    -> init:'accum
    -> f:('accum -> 'a elt -> ('accum, 'final) Continue_or_stop.t)
    -> finish:('accum -> 'final)
    -> 'final

  val exists : ('a, _) t -> f:('a elt -> bool) -> bool
  val for_all : ('a, _) t -> f:('a elt -> bool) -> bool
  val count : ('a, _) t -> f:('a elt -> bool) -> int

  val sum
    :  (module Summable with type t = 'sum)
    -> ('a, _) t
    -> f:('a elt -> 'sum)
    -> 'sum

  val find : ('a, _) t -> f:('a elt -> bool) -> 'a elt option
  val find_map : ('a, _) t -> f:('a elt -> 'b option) -> 'b option
  val to_list : ('a, _) t -> 'a elt list
  val to_array : ('a, _) t -> 'a elt array
  val min_elt : ('a, _) t -> compare:('a elt -> 'a elt -> int) -> 'a elt option
  val max_elt : ('a, _) t -> compare:('a elt -> 'a elt -> int) -> 'a elt option
end

module type Make_gen_arg = sig
  type 'a t
  type 'a elt

  val fold : 'a t -> init:'accum -> f:('accum -> 'a elt -> 'accum) -> 'accum

  (** The [iter] argument to [Container.Make] specifies how to implement the
      container's [iter] function.  [`Define_using_fold] means to define [iter]
      via:

      {[
        iter t ~f = Container.iter ~fold t ~f
      ]}

      [`Custom] overrides the default implementation, presumably with something more
      efficient.  Several other functions returned by [Container.Make] are defined in
      terms of [iter], so passing in a more efficient [iter] will improve their efficiency
      as well. *)
  val iter : [ `Define_using_fold | `Custom of 'a t -> f:('a elt -> unit) -> unit ]

  (** The [length] argument to [Container.Make] specifies how to implement the
      container's [length] function.  [`Define_using_fold] means to define
      [length] via:

      {[
        length t ~f = Container.length ~fold t ~f
      ]}

      [`Custom] overrides the default implementation, presumably with something more
      efficient.  Several other functions returned by [Container.Make] are defined in
      terms of [length], so passing in a more efficient [length] will improve their
      efficiency as well. *)
  val length : [ `Define_using_fold | `Custom of 'a t -> int ]
end

module type Make_arg = Make_gen_arg with type 'a elt := 'a Monad.Ident.t

module type Make0_arg = sig
  module Elt : sig
    type t

    val equal : t -> t -> bool
  end

  type t

  val fold : t -> init:'accum -> f:('accum -> Elt.t -> 'accum) -> 'accum
  val iter : [ `Define_using_fold | `Custom of t -> f:(Elt.t -> unit) -> unit ]
  val length : [ `Define_using_fold | `Custom of t -> int ]
end

module type Container = sig
  include module type of struct
    include Export
  end

  module type S0 = S0
  module type S0_phantom = S0_phantom
  module type S1 = S1
  module type S1_phantom_invariant = S1_phantom_invariant
  module type S1_phantom = S1_phantom
  module type Generic = Generic
  module type Generic_phantom = Generic_phantom
  module type Summable = Summable

  (** Generic definitions of container operations in terms of [fold].

      E.g.: [iter ~fold t ~f = fold t ~init:() ~f:(fun () a -> f a)]. *)

  type ('t, 'a, 'accum) fold = 't -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
  type ('t, 'a) iter = 't -> f:('a -> unit) -> unit
  type 't length = 't -> int

  val iter : fold:('t, 'a, unit) fold -> ('t, 'a) iter
  val count : fold:('t, 'a, int) fold -> 't -> f:('a -> bool) -> int

  val min_elt
    :  fold:('t, 'a, 'a option) fold
    -> 't
    -> compare:('a -> 'a -> int)
    -> 'a option

  val max_elt
    :  fold:('t, 'a, 'a option) fold
    -> 't
    -> compare:('a -> 'a -> int)
    -> 'a option

  val length : fold:('t, _, int) fold -> 't -> int
  val to_list : fold:('t, 'a, 'a list) fold -> 't -> 'a list

  val sum
    :  fold:('t, 'a, 'sum) fold
    -> (module Summable with type t = 'sum)
    -> 't
    -> f:('a -> 'sum)
    -> 'sum

  val fold_result
    :  fold:('t, 'a, 'b) fold
    -> init:'b
    -> f:('b -> 'a -> ('b, 'e) Result.t)
    -> 't
    -> ('b, 'e) Result.t

  val fold_until
    :  fold:('t, 'a, 'b) fold
    -> init:'b
    -> f:('b -> 'a -> ('b, 'final) Continue_or_stop.t)
    -> finish:('b -> 'final)
    -> 't
    -> 'final

  (** Generic definitions of container operations in terms of [iter] and [length]. *)
  val is_empty : iter:('t, 'a) iter -> 't -> bool

  val exists : iter:('t, 'a) iter -> 't -> f:('a -> bool) -> bool
  val for_all : iter:('t, 'a) iter -> 't -> f:('a -> bool) -> bool
  val find : iter:('t, 'a) iter -> 't -> f:('a -> bool) -> 'a option
  val find_map : iter:('t, 'a) iter -> 't -> f:('a -> 'b option) -> 'b option
  val to_array : length:'t length -> iter:('t, 'a) iter -> 't -> 'a array

  (** The idiom for using [Container.Make] is to bind the resulting module and to
      explicitly import each of the functions that one wants:

      {[
        module C = Container.Make (struct ... end)
        let count    = C.count
        let exists   = C.exists
        let find     = C.find
        (* ... *)
      ]}

      This is preferable to:

      {[
        include Container.Make (struct ... end)
      ]}

      because the [include] makes it too easy to shadow specialized implementations of
      container functions ([length] being a common one).

      [Container.Make0] is like [Container.Make], but for monomorphic containers like
      [string]. *)
  module Make (T : Make_arg) : S1 with type 'a t := 'a T.t

  module Make0 (T : Make0_arg) : S0 with type t := T.t and type elt := T.Elt.t
end
