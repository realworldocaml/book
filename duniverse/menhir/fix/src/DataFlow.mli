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

(**This module performs a {b forward data flow analysis} over a (possibly
   cyclic) directed graph. Like [Fix.Fix], it computes the least function of
   type [variable -> property] that satisfies a fixed point equation. It is
   less widely applicable than [Fix.Fix], but, when it is applicable, it can
   be both easier to use and more efficient. It does not perform dynamic
   dependency discovery. The submodule [Fix.DataFlow.ForCustomMaps] is
   particularly {b tuned for performance}. *)

open Sigs

(**{!Run} requires a type [variable] that is equipped with an implementation of
   imperative maps, a type [property] that is equipped with [leq] and [join]
   functions, and a data flow graph whose edges describe the propagation of
   properties. It performs a forward data flow analysis and returns its
   result.

   The function [solution] has type [variable -> property option]. A reachable
   variable is mapped to [Some _]; an unreachable one is mapped to [None]. *)
module Run
  (M : MINIMAL_IMPERATIVE_MAPS)
  (P : MINIMAL_SEMI_LATTICE)
  (G : DATA_FLOW_GRAPH with type variable = M.key and type property = P.property)
     : SOLUTION
       with type variable = G.variable
        and type property = P.property option

(**{!ForOrderedType} is a special case of {!Run} where it
   suffices to pass an ordered type [T] as an argument.
   A reference to a persistent map is used to hold the
   memoization table. *)
module ForOrderedType
  (T : OrderedType)
  (P : MINIMAL_SEMI_LATTICE)
  (G : DATA_FLOW_GRAPH with type variable = T.t and type property = P.property)
     : SOLUTION
       with type variable = G.variable
        and type property = P.property option

(**{!ForHashedType} is a special case of {!Run} where it
   suffices to pass a hashed type [T] as an argument. A
   hash table is used to hold the memoization table. *)
module ForHashedType
  (T : HashedType)
  (P : MINIMAL_SEMI_LATTICE)
  (G : DATA_FLOW_GRAPH with type variable = T.t and type property = P.property)
     : SOLUTION
       with type variable = G.variable
        and type property = P.property option

(**{!ForType} is a special case of {!Run} where it suffices
   to pass an arbitrary type [T] as an argument. A hash table
   is used to hold the memoization table. OCaml's built-in
   generic equality and hash functions are used. *)
module ForType
  (T : TYPE)
  (P : MINIMAL_SEMI_LATTICE)
  (G : DATA_FLOW_GRAPH with type variable = T.t and type property = P.property)
     : SOLUTION
       with type variable = G.variable
        and type property = P.property option

(**{!ForIntSegment} is a special case of {!Run} where the type of variables
   is the integer segment [\[0..n)]. An array is used to hold the table. *)
module ForIntSegment
  (K : sig val n: int end)
  (P : MINIMAL_SEMI_LATTICE)
  (G : DATA_FLOW_GRAPH with type variable = int and type property = P.property)
     : SOLUTION
       with type variable = G.variable
        and type property = P.property option

(**{!ForCustomMaps} is a forward data flow analysis that is tuned for greater
   performance. It internally relies on {!CompactQueue}, instead of [Queue].
   Furthermore, instead of relying on a full-fledged implementation of maps
   as described by {!MINIMAL_IMPERATIVE_MAPS}, it expects the user to create
   and initialize two maps [V] and [B] that satisfy the signature {!ARRAY}.
   This typically allows the user to choose an efficient, specialized data
   representation.

   The map [V] must be initialized with [bottom] everywhere.
   The map [B] must be initialized with [false] everywhere.

   The functor returns nothing: the map [V] is modified in place and can be
   read by the user after the fixed point has been reached. *)
module ForCustomMaps
  (P : MINIMAL_SEMI_LATTICE)
  (G : DATA_FLOW_GRAPH with type property := P.property)
  (V : ARRAY with type key := G.variable and type value := P.property)
  (B : ARRAY with type key := G.variable and type value := bool)
    : sig end
