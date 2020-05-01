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

open Sigs

(* The type ['data cell] describes a cell that carries a unique identifier
   [id] as well as a payload [data]. *)

(* This type is marked [private], which means that the user has no direct
   way of allocating cells. Instead, the user must apply the functor [Make]
   (below) to obtain a function [make] which either allocates a fresh cell
   or returns an existing cell. The user is still allowed to read existing
   cells. *)

type 'data cell = private
  { id: int; data: 'data }

(* Accessors. *)

val id  : 'data cell -> int
val data: 'data cell -> 'data

(* Cells come with an equality test, a comparison function, and and a hash
   function. These functions exploit the cell's unique identifier only -- the
   data is ignored. *)

(* Wherever a module of signature [HashedType with type t = foo cell] is
   expected, the module [HashCons] can be supplied. This holds regardless
   of the type [foo]. *)

val equal: 'data cell -> 'data cell -> bool
val compare: 'data cell -> 'data cell -> int
val hash : 'data cell -> int

(* A hash-consing service allocates uniquely-numbered cells for data. The
   smart constructor [make] either allocates a fresh cell or returns an
   existing cell, as appropriate. *)

module type SERVICE = sig
  type data
  val make: data -> data cell
end

(* The functor [Make] expects a type [data] for which a memoizer exists, and
   produces a hash-consing service for it. *)

module Make
  (M : MEMOIZER)
     : SERVICE with type data = M.key

(* [ForHashedType] is a special case of [Make] where it
   suffices to pass a hashed type [T] as an argument. A
   hash table is used to hold the memoization table. *)

module ForHashedType
  (T : HashedType)
     : SERVICE with type data = T.t
