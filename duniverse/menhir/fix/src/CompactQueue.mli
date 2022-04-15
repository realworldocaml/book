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

(**This module offers {b a minimalist mutable FIFO queue} that is
   tuned for performance.

   In comparison with OCaml's [Queue] module, it uses a more compact
   internal representation: elements are stored contiguously in an
   array. This has a positive impact on performance: both time and
   memory consumption are reduced.

   The implementation is optimized for maximum throughput. The
   internal array is never shrunk, so a queue that has contained many
   elements in the past remains large. Furthermore, array slots are
   not overwritten when elements are extracted; thus, an element that
   has appeared in the queue in the past can remain reachable. These
   drawbacks are usually acceptable provided that the queue is thrown
   away after use. *)

type 'a t
(**The type of a queue. *)

val create : unit -> 'a t
(**[create()] creates an empty queue. *)

val is_empty : 'a t -> bool
(**[is_empty q] tests whether the queue [q] is empty. *)

val add : 'a -> 'a t -> unit
(**[add x q] adds the element [x] at the end of the queue [q]. *)

exception Empty
(**[Empty] is raised when {!take} is applied to an empty queue. *)

val take : 'a t -> 'a
(**[take q] removes and returns the first element of the queue [q].
   It raises {!Empty} if the queue is empty. *)
