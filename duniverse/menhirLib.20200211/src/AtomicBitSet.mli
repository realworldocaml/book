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

(* This module offers bitsets that fit within an OCaml integer. This can be
   used to represent sets of integers in the semi-open interval [0, bound),
   where [bound] is [Sys.word_size - 1], that is, usually 63. *)

val bound: int

include GSet.S with type element = int

(* [iter_delta] and [fold_delta] are slightly generalized variants of [iter]
   and [fold]. They add the constant [delta] on the fly to each set element
   before presenting this set element to the user function [f]. *)

val iter_delta: int -> (element -> unit) -> t -> unit
val fold_delta: int -> (element -> 'b -> 'b) -> t -> 'b -> 'b
