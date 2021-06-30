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

(**[Fix] offers support for computing the least solution of a set of monotone
   equations, as described in the unpublished paper "Lazy Least Fixed Points
   in ML". In other words, it allows defining a recursive function of type
   [variable -> property], where cyclic dependencies between variables are
   allowed, and properties must be equipped with a partial order. The function
   thus obtained performs the fixed point computation on demand, in an
   incremental manner, and is memoizing. *)

open Sigs

(**[Make] constructs a solver for a type [key] that is equipped with
   an implementation of imperative maps and a type [property] that is
   equipped with [bottom], [equal], and [is_maximal] functions. *)

module Make
  (M : IMPERATIVE_MAPS)
  (P : PROPERTY)
     : SOLVER
       with type variable = M.key
        and type property = P.property

(**[ForOrderedType] is a special case of [Make] where it
   suffices to pass an ordered type [T] as an argument.
   A reference to a persistent map is used to hold the
   memoization table. *)

module ForOrderedType
  (T : OrderedType)
  (P : PROPERTY)
     : SOLVER
       with type variable = T.t
        and type property = P.property

(**[ForHashedType] is a special case of [Make] where it
   suffices to pass a hashed type [T] as an argument. A
   hash table is used to hold the memoization table. *)

module ForHashedType
  (T : HashedType)
  (P : PROPERTY)
     : SOLVER
       with type variable = T.t
        and type property = P.property

(**[ForType] is a special case of [Make] where it suffices
   to pass an arbitrary type [T] as an argument. A hash table
   is used to hold the memoization table. OCaml's built-in
   generic equality and hash functions are used. *)

module ForType
  (T : TYPE)
  (P : PROPERTY)
     : SOLVER
       with type variable = T.t
        and type property = P.property
