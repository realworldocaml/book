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

(* This is a stripped-down copy of the [Set] module from OCaml's standard
   library. The only difference is that [add x t] guarantees that it returns
   [t] (physically unchanged) if [x] is already a member of [t]. This yields
   fewer memory allocations and an easy way of testing whether the element was
   already present in the set before it was added. *)

module Make (Ord: Map.OrderedType) : sig

  type elt = Ord.t
  type t

  val empty: t
  val add: elt -> t -> t
  val find: elt -> t -> elt (* may raise [Not_found] *)
  val iter: (elt -> unit) -> t -> unit

end
