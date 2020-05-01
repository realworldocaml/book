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

(* [Make(M)(G)] produces a numbering of the graph [G], or more precisely, of
   the subset of the vertices of [G] that are reachable from the roots. The
   type of the vertices must be equipped with an implementation of imperative
   maps. *)

module Make
  (M : IMPERATIVE_MAPS)
  (G : GRAPH with type t = M.key)
     : NUMBERING with type t = G.t

(* [ForOrderedType] is a special case of [Make] where it suffices for
   the vertices of [G] to be ordered. *)

module ForOrderedType
  (T : OrderedType)
  (G : GRAPH with type t = T.t)
     : NUMBERING with type t = G.t

(* [ForHashedType] is a special case of [Make] where it suffices for
   the vertices of [G] to be hashed. *)

module ForHashedType
  (T : HashedType)
  (G : GRAPH with type t = T.t)
     : NUMBERING with type t = G.t

(* [ForType] is a special case of [Make] where the vertices of [G] can
   have arbitrary type. OCaml's built-in generic equality and hash
   functions are used. *)

module ForType
  (T : TYPE)
  (G : GRAPH with type t = T.t)
     : NUMBERING with type t = G.t
