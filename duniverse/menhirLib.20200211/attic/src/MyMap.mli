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

(* This is a stripped-down copy of the [Map] module from OCaml's standard
   library. The only difference is that [add x d f m] takes both a datum
   [default] and a function [f] of data to data. If the key [x] is absent in the
   map [m], then a mapping of [x] to [f default] is added. If the key [x] is
   present, then the existing datum if passed to [f], which produces a new
   datum. If the old and new data are physically the same, then the map [m] is
   returned, physically unchanged. Otherwise, an updated map is returned. This
   yields fewer memory allocations and an easy way of testing whether the
   binding was already present in the set before it was added. *)

module Make (Ord: Map.OrderedType) : sig

  type key = Ord.t
  type 'a t

  val empty: 'a t
  val add: key -> 'a -> ('a -> 'a) -> 'a t -> 'a t
  val find: key -> 'a t -> 'a (* may raise [Not_found] *)
  val iter: (key -> 'a -> unit) -> 'a t -> unit

end
