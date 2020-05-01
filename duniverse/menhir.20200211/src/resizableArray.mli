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

(* This module implements resizable arrays, that is, arrays that can
   grow upon explicit request. *)

type 'a t

(* [make capacity default init] creates a resizable array of logical length 0,
   whose physical length is initially [capacity], and whose default element is
   [default]. The default element is used to fill empty slots in the physical
   array; it is otherwise irrelevant. The [init] function is used to
   initialize new logical slots when the logical size of the array grows, so,
   unlike [default], it is semantically meaningful. *)

val make: int -> 'a -> (int -> 'a) -> 'a t

(* [make_] is a simplified variant of [make] where the [init] function always
   returns [default], i.e., where new logical slots are initialized with
   [default] when the array is grown. *)

val make_: int -> 'a -> 'a t

(* [length a] returns the current logical length of the array [a]. *)

val length: 'a t -> int

(* [resize a n] changes the logical length of the array [a] to [n]. If the
   length decreases, any excess elements are lost. The capacity of the
   underlying physical array remains the same. If the length increases, the
   new positions are filled with the array's default element, as initially
   supplied to [make]. The capacity of the underlying physical array grows
   by at least a factor of two. *)

val resize: 'a t -> int -> unit

(* [get a i] returns the element contained at offset [i] in the array [a].
   Slots are numbered 0 and up. [i] must be strictly less than the array's
   current logical length. *)

val get: 'a t -> int -> 'a

(* [set a i x] sets the element contained at offset [i] in the array [a]
   to [x]. Slots are numbered 0 and up. [i] must be strictly less than
   the array's current logical length. *)

val set: 'a t -> int -> 'a -> unit

(* [push a x] appends the element [x] at the end of the array [a], whose
   length increases by one. *)

val push: 'a t -> 'a -> unit

(* [pop a] removes the element [x] found at the end of the array [a], whose
   length decreases by one. The array must have nonzero length. *)

val pop: 'a t -> 'a

(* [default a] returns the default value that was used when the array [a]
   was created. This should be seldom useful, but can be convenient. *)

val default: 'a t -> 'a

