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

(* [Make] constructs a tabulator for a finite type that is
   equipped with an implementation of imperative maps. *)

module Make
  (F : FINITE_TYPE)
  (M : MINIMAL_IMPERATIVE_MAPS with type key = F.t)
     : TABULATOR with type key = F.t

(* [ForOrderedType] is a special case of [Make] where it
   suffices to pass a finite ordered type as an argument.
   A reference to a persistent map is used to hold the table. *)

module ForOrderedType
  (F : FINITE_TYPE)
  (T : OrderedType with type t = F.t)
     : TABULATOR with type key = F.t

(* [ForOrderedType] is a special case of [Make] where it
   suffices to pass a finite hashed type as an argument.
   A reference to a persistent map is used to hold the table. *)

module ForHashedType
  (F : FINITE_TYPE)
  (T : HashedType with type t = F.t)
     : TABULATOR with type key = F.t

(* [ForOrderedType] is a special case of [Make] where it suffices to
   pass an arbitrary finite type as an argument. A reference to a
   persistent map is used to hold the table. *)

module ForType (F : FINITE_TYPE)
     : TABULATOR with type key = F.t

(* [ForIntSegment] constructs a tabulator for the integer segment [0..n).
   An array is used to hold the table. *)

module ForIntSegment
  (K : sig val n: int end)
     : TABULATOR with type key = int
