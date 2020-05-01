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

(* This module offers bitsets that fit within *four* OCaml integers. This can
   be used to represent sets of integers in the semi-open interval [0, bound),
   where [bound] is [4 * AtomicBitSet.bound], that is, usually 252. *)

val bound: int

include GSet.S with type element = int
