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

include Map.S with type key = string

(* [cardinal m] is the cardinal of the map [m]. *)

val cardinal : 'a t -> int

(* [restrict s m] restricts the domain of the map [m] to (its
   intersection with) the set [s]. *)

val restrict: StringSet.t -> 'a t -> 'a t

(* [filter pred m] restricts the domain of the map [m] to
   (key, value) couples that verify [pred]. *)

val filter: (string -> 'a -> bool) -> 'a t -> 'a t

(* [domain m] returns the domain of the map [m]. *)

val domain: 'a t -> StringSet.t

(* [multiple_add k v m] adds the key-value pair [k, v] to the map [m],
   which maps keys to *lists* of values. The list currently associated
   with [k] is extended with the value [v]. *)

val multiple_add: key -> 'a -> 'a list t -> 'a list t
