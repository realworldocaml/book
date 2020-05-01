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

(* The functor [Make] requires an implementation of maps for the type [M.key]
   and offers a two-phase numbering facility. The function [encode] is backed
   by a map, therefore runs in logarithmic time or constant time, depending on
   the type of map that is used. The function [decode] is backed by an array
   of size [n], therefore runs in constant time. *)

module Make
  (M : IMPERATIVE_MAPS)
     : TWO_PHASE_NUMBERING with type t = M.key

(* [ForOrderedType] is a special case of [Make] where it suffices for
   keys to be ordered. *)

module ForOrderedType
  (T : OrderedType)
     : TWO_PHASE_NUMBERING with type t = T.t

(* [ForHashedType] is a special case of [Make] where it suffices for
   keys to be hashed. *)

module ForHashedType
  (T : HashedType)
     : TWO_PHASE_NUMBERING with type t = T.t

(* [ForType] is a special case of [Make] where keys can have arbitrary type.
   OCaml's built-in generic equality and hash functions are used. *)

module ForType
  (T : TYPE)
     : TWO_PHASE_NUMBERING with type t = T.t
