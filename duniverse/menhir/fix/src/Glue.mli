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

(* -------------------------------------------------------------------------- *)

(* Some common types, packaged as modules of signature [TYPE]. *)

module CHAR : TYPE with type t = char
module INT : TYPE with type t = int
module STRING : TYPE with type t = string

(* -------------------------------------------------------------------------- *)

(* An arbitrary type can be equipped with an ordering function,
   just by using OCaml's built-in generic comparison function. *)

module TrivialOrderedType
  (T : TYPE)
     : OrderedType with type t = T.t

(* An arbitrary type can be equipped with equality and hash functions,
   just by using OCaml's built-in generic equality and hash functions. *)

module TrivialHashedType
  (T : TYPE)
     : HashedType with type t = T.t

(* -------------------------------------------------------------------------- *)

(* If there is an injection of [t] into [u], then an ordering on [u] gives
   rise to an ordering on [t]. *)

module InjectOrderedType
  (U : OrderedType)
  (I : INJECTION with type u := U.t)
     : OrderedType with type t = I.t

(* If there is an injection of [t] into [u], then a hashed-type structure
   on [u] can be transported to [t]. *)

module InjectHashedType
  (U : HashedType)
  (I : INJECTION with type u := U.t)
     : HashedType with type t = I.t

(* If there is an injection of [t] into [u], then an implementation of minimal
   imperative maps for the type [u] can be transported to the type [t]. *)

module InjectMinimalImperativeMaps
  (M : MINIMAL_IMPERATIVE_MAPS)
  (I : INJECTION with type u := M.key)
     : MINIMAL_IMPERATIVE_MAPS with type key = I.t

(* If there is an injection of [t] into [u], and if the inverse mapping can be
   effectively computed, then an implementation of imperative maps for the
   type [u] can be transported to the type [t]. *)

module InjectImperativeMaps
  (M : IMPERATIVE_MAPS)
  (I : INJECTION with type u := M.key)
  (J : sig val decode: M.key -> I.t end)
     : IMPERATIVE_MAPS with type key = I.t

(* -------------------------------------------------------------------------- *)

(* Implementations of various map signatures. *)

(* An implementation of persistent maps can be made to satisfy the interface
   of imperative maps. An imperative map is represented as a persistent map,
   wrapped within a reference cell. *)

module PersistentMapsToImperativeMaps
  (M : PERSISTENT_MAPS)
     : IMPERATIVE_MAPS with type key = M.key
		        and type 'data t = 'data M.t ref

(* An implementation of imperative maps as arrays is possible if keys
   are consecutive integers. *)

module ArraysAsImperativeMaps
  (K : sig val n: int end)
  : IMPERATIVE_MAPS with type key = int
                     and type 'data t = 'data option array

(* An implementation of imperative maps as a hash table. *)

module HashTablesAsImperativeMaps
  (H : HashedType)
     : IMPERATIVE_MAPS with type key = H.t
                        and type 'data t = 'data Hashtbl.Make(H).t
