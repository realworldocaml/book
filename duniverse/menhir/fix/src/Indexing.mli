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

(**This module offers {b a safe API for manipulating indices into fixed-size
   arrays}.

   It provides support for constructing finite sets at the type level and
   for encoding the inhabitants of these sets as runtime integers. These
   runtime integers are statically branded with the name of the set that
   they inhabit, so two inhabitants of two distinct sets cannot be
   inadvertently confused. *)

(**If [n] is a type-level name for a finite set, then a value of type
   [n cardinal] is a runtime integer that represents the cardinal of
   the set [n].

   In the following, the functor {!Gensym} allows creating open-ended
   sets, which can grow over time. If [n] is such a set, then a value
   of type [n cardinal] can be thought of as the as-yet-undetermined
   cardinal of the set. *)
type 'n cardinal

(**If [n] is the cardinal of the set [n], then [cardinal n] returns the
   cardinal of this set, as a concrete integer.

   In the following, the functor {!Gensym} allows creating open-ended sets,
   which can grow over time. If [n] is such a set, then calling [cardinal n]
   has the side-effect of freezing this set, thereby fixing its cardinal:
   thereafter, calling [fresh] becomes forbidden, so no new elements can be
   added. *)
val cardinal : 'n cardinal -> int

(**If [n] is a type-level name for a finite set, then a value [i] of type
   [n index] is an integer value that is guaranteed to inhabit the set [n].

   If [n] has type [n cardinal], then [0 <= i < cardinal n] must hold.

   The main reason why elements of a finite set are called "indices" is that
   their main purpose is to serve as indices in fixed-size vectors. See the
   submodule [Vector] below. *)
type 'n index =
  private int

(**A new type-level set is created by an application of the functors {!Const},
   {!Gensym}, and {!Sum} below. Each functor application creates a fresh type
   name [n]. More precisely, each functor application returns a module whose
   signature is {!CARDINAL}: it contains both a fresh abstract type [n] and a
   value [n] of type [n cardinal] that represents the cardinal of the
   newly-created set. *)
module type CARDINAL = sig
  type n
  val n : n cardinal
end

(**[Const(struct let cardinal = c end)] creates a fresh type-level name for a
   set whose cardinal is [c]. [c] must be nonnegative. *)
module Const (X : sig val cardinal : int end) : CARDINAL

(**The function {!const} is a value-level analogue of the functor {!Const}. *)
val const : int -> (module CARDINAL)

(**{!Empty} contains a type-level name for the empty set. *)
module Empty: CARDINAL

(**[Gensym()] creates an open-ended type-level set, whose cardinality is not
   known a priori. As long as the cardinal of the set has not been observed by
   invoking {!val-cardinal}, new elements can be added to the set by invoking
   [fresh]. *)
module Gensym () : sig

  (** The type-level name [n] of the set and its cardinal [n]. *)
  include CARDINAL

  (**If [cardinal n] has not been invoked yet, then [fresh()] adds a new
     element to the set. Otherwise, calling [fresh] is forbidden and causes a
     runtime failure. *)
  val fresh : unit -> n index

end

(**The type [('l, 'r) either] represents the disjoint sum of the types ['l]
   and ['r]. It is isomorphic to the type [either] found in [Stdlib.Either]
   in OCaml 4.12.0. *)
type ('l, 'r) either =
  | L of 'l
  | R of 'r

(**The signature {!SUM} extends {!CARDINAL} with an explicit isomorphism between
   the set [n] and the disjoint sum [l + r]. The functions [inj_l] and [inj_r]
   convert an index into [l] or an index into [r] into an index into [n].
   Conversely, the function [prj] converts an index into [r] into either an
   index into [l] or an index into [r]. *)
module type SUM = sig
  type l and r
  include CARDINAL
  val inj_l : l index -> n index
  val inj_r : r index -> n index
  val prj : n index -> (l index, r index) either
end

(**[Sum(L)(R)] creates a new type-level set, which is the disjoint sums of the
   sets [L] and [R]. The functor application [Sum(L)(R)] involves a call to
   [cardinal L.n], thereby fixing the cardinal of the set [L], if it was an
   open-ended set. The cardinal of the set [R] is not fixed: if [R] is an
   open-ended set, then the new set is open-ended as well, and it is still
   permitted to add new elements to [R] by calling [R.fresh()]. Fixing the
   cardinal of the new set fixes the cardinal of [R]. *)
module Sum (L : CARDINAL)(R : CARDINAL) :
  SUM with type l := L.n
       and type r := R.n

(**The function {!sum} is a value-level analogue of the functor {!Sum}. *)
val sum : 'l cardinal -> 'r cardinal ->
  (module SUM with type l = 'l and type r = 'r)

(**The submodule {!Index} allows safely manipulating indices
   into a finite set. *)
module Index : sig

  type 'n t = 'n index

  (**If [n] is the cardinal of the type-level set [n], then [of_int n] casts
     an integer [i] of type [int] into an index: that is, [of_int n i] returns
     [i] at type [n index]. The integer [i] must lie in the semi-open interval
     [\[0, n)]. This is enforced by a runtime check. Calling [of_int n i]
     fixes the cardinal [n]. *)
  val of_int : 'n cardinal -> int -> 'n index

  (**{!to_int} casts an index [i] back to an ordinary integer value. *)
  val to_int : 'n index -> int

  (**[iter n yield] calls [yield i] successively for every index in the range
     [\[0, n)], in increasing order. *)
  val iter : 'n cardinal -> ('n index -> unit) -> unit

  (**This exception is raised by an iterator (created by {!enumerate}) that is
     queried after it has been exhausted. *)
  exception End_of_set

  (**[enumerate n] returns an imperative iterator, which produces all indices
     in the range [\[0, n)] in increasing order. Querying the iterator after
     all elements have been produced causes the exception {!End_of_set} to be
     raised. *)
  val enumerate : 'n cardinal -> (unit -> 'n index)

end

(**A vector of type [(n, a) vector] is a (fixed-size) array whose indices lie
   in the type-level set [n] and whose elements have type [a]. *)
type ('n, 'a) vector =
  private 'a array

(**The submodule {!Vector} allows safely manipulating indices into a vector. *)
module Vector : sig

  type ('n, 'a) t = ('n, 'a) vector

  (**{!length} is analogous to [Array.length], but returns a cardinal instead
     of an ordinary integer. *)
  val length : ('n, 'a) t -> 'n cardinal

  (**{!get} is [Array.get], but expects an index instead of an ordinary
     integer. This guarantees that the index is within bounds. *)
  val get : ('n, 'a) t -> 'n index -> 'a

  (**{!set} is [Array.set], but expects an index instead of an ordinary
     integer. This guarantees that the index is within bounds. *)
  val set : ('n, 'a) t -> 'n index -> 'a -> unit

  (**[set_cons t i x] is short for [set t i (x :: get t i)]. *)
  val set_cons : ('n, 'a list) t -> 'n index -> 'a -> unit

  (**{!empty} is the empty vector. *)
  val empty : (Empty.n, _) t

  (**{!make} is analogous to [Array.make]. Invoking [make n x] fixes the
     cardinal [n]. *)
  val make : 'n cardinal -> 'a -> ('n, 'a) t

  (**[make' n f] is roughly analogous to [make n (f())], but removes the need
     to exhibit a value of type ['a] when [n] is zero. The function call [f()]
     takes place only if [n] is greater than zero. It takes place at most
     once. Invoking [make' n f] fixes the cardinal [n]. *)
  val make' : 'n cardinal -> (unit -> 'a) -> ('n, 'a) t

  (**{!init} is analogous to [Array.init]. Invoking [init n f] fixes the
     cardinal [n]. *)
  val init : 'n cardinal -> ('n index -> 'a) -> ('n, 'a) t

  (**{!map} is analogous to [Array.map]. *)
  val map : ('a -> 'b) -> ('n, 'a) t -> ('n, 'b) t

end
