(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* This module defines three signatures for association maps, together
   with a number of conversion functors. *)

(* Following the convention of the ocaml standard library, the [find]
   functions raise [Not_found] when the key is not a member of the
   domain of the map. By contrast, [get] returns an option. *)

(* BEGIN PERSISTENT_MAPS *)
module type PERSISTENT_MAPS = sig
  type key
  type 'data t
  val empty: 'data t
  val add: key -> 'data -> 'data t -> 'data t
  val find: key -> 'data t -> 'data
  val iter: (key -> 'data -> unit) -> 'data t -> unit
end
(* END PERSISTENT_MAPS *)

(* BEGIN IMPERATIVE_MAPS *)
module type IMPERATIVE_MAPS = sig
  type key
  type 'data t
  val create: unit -> 'data t
  val clear: 'data t -> unit
  val add: key -> 'data -> 'data t -> unit
  val find: key -> 'data t -> 'data
  val iter: (key -> 'data -> unit) -> 'data t -> unit
end
(* END IMPERATIVE_MAPS *)

(* BEGIN IMPERATIVE_MAP *)
module type IMPERATIVE_MAP = sig
  type key
  type data
  val set: key -> data -> unit
  val get: key -> data option
end
(* END IMPERATIVE_MAP *)

(* An implementation of persistent maps can be made to satisfy the interface
   of imperative maps. An imperative map is represented as a persistent map,
   wrapped within a reference cell. *)

module PersistentMapsToImperativeMaps
  (M : PERSISTENT_MAPS)
     : IMPERATIVE_MAPS with type key = M.key
                        and type 'data t = 'data M.t ref

(* An implementation of imperative maps can be made to satisfy the interface
   of a single imperative map. This map is obtained via a single call to [create]. *)

module ImperativeMapsToImperativeMap
  (M : IMPERATIVE_MAPS)
  (D : sig type data end)
     : IMPERATIVE_MAP with type key = M.key
                       and type data = D.data

(* An implementation of imperative maps as arrays is possible if keys
   are consecutive integers. *)

module ArrayAsImperativeMaps
  (K : sig val n: int end)
  : IMPERATIVE_MAPS with type key = int
                     and type 'data t = 'data option array

(* An implementation of imperative maps as a hash table. *)

module HashTableAsImperativeMaps
  (H : Hashtbl.HashedType)
     : IMPERATIVE_MAPS with type key = H.t

(* A trivial implementation of equality and hashing. *)

module TrivialHashedType
  (T : sig type t end)
     : Hashtbl.HashedType with type t = T.t

