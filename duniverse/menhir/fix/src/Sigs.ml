(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)

(**A type alone. *)
module type TYPE = sig
  type t
end

(* -------------------------------------------------------------------------- *)

(**An ordered type. *)
module type OrderedType =
  Map.OrderedType

(**A hashed type. *)
module type HashedType =
  Hashtbl.HashedType

(* -------------------------------------------------------------------------- *)

(**A type whose elements can be enumerated. *)
module type FINITE_TYPE = sig
  type t
  val foreach: (t -> unit) -> unit
end

(* -------------------------------------------------------------------------- *)

(* Association maps. *)

(**{!PERSISTENT_MAPS} is a fragment of the standard signature [Map.S]. *)
module type PERSISTENT_MAPS = sig

  (**The type of keys. *)
  type key

  (**The type of association maps. *)
  type 'data t

  (**The empty map. *)
  val empty: 'data t

  (**{!add} inserts a new entry or replaces an existing entry. *)
  val add: key -> 'data -> 'data t -> 'data t

  (**{!find} raises [Not_found] if the key is not in the domain of
     the map. *)
  val find: key -> 'data t -> 'data

  (**{!iter} iterates over all entries. *)
  val iter: (key -> 'data -> unit) -> 'data t -> unit

end

module type MINIMAL_IMPERATIVE_MAPS = sig

  (**The type of keys. *)
  type key

  (**The type of association maps. *)
  type 'data t

  (**[create()] creates a fresh empty map. *)
  val create: unit -> 'data t

  (**{!add} inserts a new entry or replaces an existing entry.
     The map is updated in place. *)
  val add: key -> 'data -> 'data t -> unit

  (**{!find} raises [Not_found] if the key is not in the domain of
     the map. *)
  val find: key -> 'data t -> 'data

end

module type IMPERATIVE_MAPS = sig

  include MINIMAL_IMPERATIVE_MAPS

  (**{!clear} empties a map. *)
  val clear: 'data t -> unit

  (**{!iter} iterates over all entries. *)
  val iter: (key -> 'data -> unit) -> 'data t -> unit

end

(**An instance of the signature [ARRAY] represents one mutable map.
   There is no type ['data t] and no [create] operation; there exists
   just one map. Furthermore, the type [value], which corresponds to
   ['data] in the previous signatures, is fixed.

   The domain of the map never changes:
   - [set] does not extend the map,
   - [get] cannot raise [Not_found]. *)
module type ARRAY = sig

  (**The type of keys. *)
  type key

  (**The type of values. *)
  type value

  (**[get] looks up the map. It cannot raise an exception. *)
  val get : key -> value

  (**[set] updates the map at an existing key. *)
  val set : key -> value -> unit

end

(* -------------------------------------------------------------------------- *)

(**The signature {!PROPERTY} is used by {!Fix.Make}, the least fixed point
   computation algorithm. *)
module type PROPERTY = sig

  (**The type {!property} must form a partial order, and must be equipped with
     a least element {!bottom} and with an equality test {!equal}. The partial
     order must satisfy the ascending chain condition: every monotone sequence
     must eventually stabilize.

     We do not require an ordering test [leq] or a join operation [join]. *)
  type property

  (**{!bottom} is the least property. *)
  val bottom: property

  (**[equal p q] determines whether the properties [p] and [q] are equal.
     In the implementation of this test, it is permitted to assume that
     [p <= q] holds. *)
  val equal: property -> property -> bool

  (**[is_maximal p] determines whether the property [p] is maximal with
     respect to the partial order. A conservative check suffices: it is always
     permitted for [is_maximal p] to be [false]. If [is_maximal p] is [true],
     then [p] must have no strict upper bound. In particular, if properties
     form a lattice, then [is_maximal p = true] implies that [p] is the top
     element of the lattice. *)
  val is_maximal: property -> bool

end

(* -------------------------------------------------------------------------- *)

(**The signature {!SEMI_LATTICE} offers separate [leq] and [join] functions.
   The functor {!Glue.MinimalSemiLattice} can be used, if necessary, to
   convert this signature to {!MINIMAL_SEMI_LATTICE}. *)
module type SEMI_LATTICE = sig

  (**The type {!property} must form a partial order, which must satisfy the
     ascending chain condition: every monotone sequence must eventually
     stabilize. *)
  type property

  (**[leq p q] determines whether [p <= q] holds. *)
  val leq: property -> property -> bool

  (**[join p q] is the least upper bound of the properties [p] and [q]. *)
  val join: property -> property -> property

end

(**The signature {!MINIMAL_SEMI_LATTICE} is used by {!DataFlow.Run} and
   friends. *)
module type MINIMAL_SEMI_LATTICE = sig

  (**The type {!property} must form a partial order, which must satisfy the
     ascending chain condition: every monotone sequence must eventually
     stabilize. *)
  type property

  (** [leq_join p q] must compute the join of [p] and [q]. If the result
      is logically equal to [q], then [q] itself must be returned. Thus,
      we have [leq_join p q == q] if and only if [leq p q] holds. *)
  val leq_join: property -> property -> property

end

(* -------------------------------------------------------------------------- *)

(**['a fix] is the type of a fixed point combinator that constructs a value of
   type ['a]. *)
type 'a fix =
  ('a -> 'a) -> 'a

(* -------------------------------------------------------------------------- *)

(**A memoizer is a higher-order function that constructs memoizing functions. *)
module type MEMOIZER = sig

  (**The type of keys. *)
  type key

  (**{!memoize} is a memoization combinator for the type {!key}.
     The function call [memoize f] produces a function [f'] that
     behaves extensionally like [f], but is memoized. *)
  val memoize: (key -> 'a) -> (key -> 'a)

  (**The type of memoization tables. *)
  type 'a t

  (**{!visibly_memoize} is a memoization combinator that exposes the
     memoization table. The function call [visibly_memoize f] returns
     a pair of a memoized function [f'] and a memoization table. *)
  val visibly_memoize: (key -> 'a) -> (key -> 'a) * 'a t

  (**{!val-fix} is a recursive memoization combinator. *)
  val fix: (key -> 'a) fix

  (**{!Cycle} is raised by {!defensive_fix} when a dependency cycle is
     detected. *)
  exception Cycle of key list * key

  (**{!defensive_fix} works like {!val-fix}, except it detects circular
     dependencies, which can arise if the second-order function supplied by
     the user does not follow a well-founded recursion pattern. When the user
     invokes [f x], where [f] is the function returned by {!defensive_fix}, if
     a cyclic dependency is detected, then [Cycle (zs, z)] is raised, where
     the list [zs] begins with [z] and continues with a series of intermediate
     keys, leading back to [z]. Note that undetected divergence remains
     possible; this corresponds to an infinite dependency chain, without a
     cycle. *)
  val defensive_fix: (key -> 'a) fix

  (**{!curried} can be used to obtain a curried version of {!val-fix} or
     {!defensive_fix} in a concrete instance where the type {!key} is a
     product type. *)
  val curried: ('a * 'b -> 'c) fix -> ('a -> 'b -> 'c) fix

end

(* -------------------------------------------------------------------------- *)

(**A tabulator is a higher-order function that constructs tabulated
   functions. *)
module type TABULATOR = sig

  (**The type of keys. *)
  type key

  (**{!tabulate} is a tabulation combinator for the type {!key}. The
     function call [tabulate f] produces a function [f'] that behaves
     extensionally like [f], but is tabulated.

     Like memoization, tabulation guarantees that, for every key [x], the
     image [f x] is computed at most once. Unlike memoization, where this
     computation takes place on demand, here, the computation of [f x] for
     every [x] takes place eagerly, when [tabulate] is invoked. The graph of
     the function [f], a table, is constructed and held in memory. *)
  val tabulate: (key -> 'a) -> (key -> 'a)

end

(* -------------------------------------------------------------------------- *)

(**A solver is a higher-order function that computes the least solution of a
   monotone system of equations. *)
module type SOLVER = sig

  (**The type of variables. *)
  type variable

  (**The type of properties. *)
  type property

  (**A valuation is a mapping of variables to properties. *)
  type valuation = variable -> property

  (**A right-hand side, when supplied with a valuation that gives
     meaning to its free variables, evaluates to a property. More
     precisely, a right-hand side is a monotone function of
     valuations to properties. *)
  type rhs = valuation -> property

  (**A system of equations is a mapping of variables to right-hand
     sides. *)
  type equations = variable -> rhs

  (**[lfp eqs] produces the least solution of the system of monotone
     equations [eqs].

     It is guaranteed that, for each variable [v], the application [eqs v]
     is performed at most once (whereas the right-hand side produced by this
     application is, in general, evaluated multiple times). This guarantee
     can be used to perform costly pre-computation, or memory allocation,
     when [eqs] is applied to its first argument.

     When {!lfp} is applied to a system of equations [eqs], it performs no
     actual computation. It produces a valuation, [get], which represents
     the least solution of the system of equations. The actual fixed point
     computation takes place, on demand, when [get] is applied. *)
  val lfp: equations -> valuation

end

(* -------------------------------------------------------------------------- *)

(**The signature [SOLUTION] describes the result of {!DataFlow.Run} and
   friends. *)
module type SOLUTION = sig

  (**The type of variables. *)
  type variable

  (**The type of properties. *)
  type property

  (**The least solution of the system of monotone equations. *)
  val solution: variable -> property

end

(* -------------------------------------------------------------------------- *)

(**The signature {!GRAPH} describes a directed, rooted graph. It is used
   by {!GraphNumbering.Make} and friends. *)
module type GRAPH = sig

  (**The type of vertices. *)
  type t

  (**[foreach_root yield] must call [yield x] at least once for every vertex
     [x] that is considered a root (an entry point) of the graph. It may
     call [yield x] several times at a single vertex [x]. *)
  val foreach_root: (t -> unit) -> unit

  (**[foreach_successor x yield] must call [yield y] for every vertex [y]
     that is a successor of the vertex [x] in the graph. *)
  val foreach_successor: t -> (t -> unit) -> unit

end

(* -------------------------------------------------------------------------- *)

(**The signature {!DATA_FLOW_GRAPH} describes a data flow analysis
    problem. It is used by {!DataFlow.Run} and friends. *)
module type DATA_FLOW_GRAPH = sig

  (**The type of variables, or graph vertices. *)
  type variable

  (**The type of properties. *)
  type property

  (**{!foreach_root} describes the root nodes of the data flow graph as well
     as the properties associated with them. [foreach_root contribute] must
     call [contribute x p] to indicate that [x] is a root and that [p] is a
     lower bound on the solution at [x]. It may call [contribute x _]
     several times at a single root [x]. *)
  val foreach_root:
    (variable -> property -> unit) -> unit

  (**{!foreach_successor} describes the edges of the data flow graph as well
     as the manner in which a property at the source of an edge is
     transformed into a property at the target. The property at the target
     must of course be a monotonic function of the property at the source. *)
  val foreach_successor:
    variable -> property ->
    (variable -> property -> unit) -> unit

end

(* -------------------------------------------------------------------------- *)

(**An ongoing numbering of (a subset of) a type [t]. *)
module type ONGOING_NUMBERING = sig

  (**The type {!t} of values of interest. *)
  type t

  (**{!encode} maps a value of type {!t} to a unique integer code. If
     applied twice to the same value, {!encode} returns the same code; if
     applied to a value that has never been encountered, it returns a fresh
     code.*)
  val encode: t -> int

  (**[current()] returns the next available code, which is also the number
     of values that have been encoded so far. *)
  val current: unit -> int

  (**[has_been_encoded x] determines whether the value [x] has been encoded
     already. *)
  val has_been_encoded: t -> bool

end

(**A fixed numbering of (a subset of) a type [t]. *)
module type NUMBERING = sig

  (**The type {!t} of values of interest. *)
  type t

  (**{!n} is the number of values of type {!t} that have been encoded. The
   functions {!encode} and {!decode} represent an isomorphism between this
   subset of [t] and the interval [\[0..n)]. *)
  val n: int

  (**{!encode} maps a value of type {!t} to an integer code in the interval
     [\[0..n)]. *)
  val encode: t -> int

  (**{!decode} maps an integer code in the interval [\[0..n)] back to a
     value of type {!t}. *)
  val decode: int -> t

end

(**The signature {!TWO_PHASE_NUMBERING} combines the signatures
   {!ONGOING_NUMBERING} and {!NUMBERING}. It describes a numbering process
   that is organized in two phases. During the first phase, the numbering is
   ongoing: one can encode keys, but not decode. Applying the functor
   [Done()] ends the first phase. A fixed numbering then becomes available,
   which gives access to the total number [n] of encoded keys and to both
   [encode] and [decode] functions. *)
module type TWO_PHASE_NUMBERING = sig

  include ONGOING_NUMBERING

  (**The functor {!Done} ends the numbering process. *)
  module Done () : NUMBERING with type t = t

end

(* -------------------------------------------------------------------------- *)

(**An injection of a type into a type. *)
module type INJECTION = sig

  (**The source type of the injection. *)
  type t

  (**The destination type of the injection. *)
  type u

  (**An injection of [t] into [u] is an injective function of type [t -> u].
     Because {!encode} is injective, the value [encode x] can be thought of
     as the identity of the object [x]. *)
  val encode: t -> u

end
