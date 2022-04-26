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

(**This module offers support for {b setting up a hash-consed data type},
   that is, a data type whose values carry unique integer identifiers. *)

open Sigs

(**The type ['data cell] describes a cell that carries a unique identifier
   [id] as well as a payload [data].

   This type is marked [private], which means that the user has no direct
   way of allocating cells. Instead, the user must apply the functor {!Make}
   (below) to obtain a function [make] which either allocates a fresh cell
   or returns an existing cell. The user is still allowed to read existing
   cells. *)
type 'data cell = private
  { id: int; data: 'data }

(**[id cell] returns the integer identifier of the cell [cell]. *)
val id  : 'data cell -> int

(**[data cell] returns the payload of the cell [cell]. *)
val data: 'data cell -> 'data

(**Cells come with an equality test {!equal}, a comparison function
   {!compare}, and and a hash function {!hash}. These functions exploit the
   cell's unique identifier only: the data is ignored.

   As a result, wherever a module of signature [HashedType with type t = foo
   cell] is expected, the module {!HashCons} can be supplied. This holds
   regardless of the type [foo]. *)

(**[equal] determines whether two cells are the same cell.
   It is based on the cells' integer identifiers. *)
val equal: 'data cell -> 'data cell -> bool

(**[compare] implements a total order on cells,
   It is based on the cells' integer identifiers. *)
val compare: 'data cell -> 'data cell -> int

(**[hash] is a hash function on cells.
   It is based on the cells' integer identifiers. *)
val hash : 'data cell -> int

(**A hash-consing service allocates uniquely-numbered cells for data. The
   smart constructor [make] either allocates a fresh cell or returns an
   existing cell, as appropriate. *)
module type SERVICE = sig
  type data
  val make: data -> data cell
end

(**The functor {!Make} expects a type [data] for which a memoizer exists, and
   produces a hash-consing service for it. *)
module Make
  (M : MEMOIZER)
     : SERVICE with type data = M.key

(**{!ForHashedType} is a special case of {!Make} where it
   suffices to pass a hashed type [T] as an argument. A
   hash table is used to hold the memoization table. *)
module ForHashedType
  (T : HashedType)
     : SERVICE with type data = T.t

(**{!ForHashedTypeWeak} is a special case of {!Make} where it
   suffices to pass a hashed type [T] as an argument. A weak
   hash table is used to hold the memoization table. *)
module ForHashedTypeWeak
  (T : HashedType)
     : SERVICE with type data = T.t
