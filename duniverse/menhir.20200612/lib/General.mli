(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

(* This module offers general-purpose functions on lists and streams. *)

(* As of 2017/03/31, this module is DEPRECATED. It might be removed in
   the future. *)

(* --------------------------------------------------------------------------- *)

(* Lists. *)

(* [take n xs] returns the [n] first elements of the list [xs]. It is
   acceptable  for the list [xs] to have length less than [n], in
   which case [xs] itself is returned. *)

val take: int -> 'a list -> 'a list

(* [drop n xs] returns the list [xs], deprived of its [n] first elements.
   It is acceptable for the list [xs] to have length less than [n], in
   which case an empty list is returned. *)

val drop: int -> 'a list -> 'a list

(* [uniq cmp xs] assumes that the list [xs] is sorted according to the
   ordering [cmp] and returns the list [xs] deprived of any duplicate
   elements. *)

val uniq: ('a -> 'a -> int) -> 'a list -> 'a list

(* [weed cmp xs] returns the list [xs] deprived of any duplicate elements. *)

val weed: ('a -> 'a -> int) -> 'a list -> 'a list

(* --------------------------------------------------------------------------- *)

(* A stream is a list whose elements are produced on demand. *)

type 'a stream =
    'a head Lazy.t

and 'a head =
  | Nil
  | Cons of 'a * 'a stream

(* The length of a stream. *)

val length: 'a stream -> int

(* Folding over a stream. *)

val foldr: ('a -> 'b -> 'b) -> 'a stream -> 'b -> 'b
