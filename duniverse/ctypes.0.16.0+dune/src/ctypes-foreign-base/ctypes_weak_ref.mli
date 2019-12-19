(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** A single-cell variant of the weak arrays in the standard library. *)

exception EmptyWeakReference
(** An expired weak reference was accessed. *)

type 'a t
(** The type of weak references.. *)

val make : 'a -> 'a t
(** Obtain a weak reference from a strong reference. *)

val set : 'a t -> 'a -> unit
(** Update a weak reference. *)

val get : 'a t -> 'a
(** Obtain a strong reference from a weak reference. *)

val is_empty : 'a t -> bool
(** Whether a weak reference is still live. *)
