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

(* The lattice of sets. The ordering is set inclusion. Therefore,
   the empty set is the bottom element. *)

open Sigs

module Set (X : sig
  type t
  val empty: t
  val equal: t -> t -> bool
end) : PROPERTY with type property = X.t
