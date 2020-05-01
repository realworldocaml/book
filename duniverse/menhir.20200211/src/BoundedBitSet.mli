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

(* This functor takes an integer parameter [n] and provides an implementation
   of sets of integers, where every integer element must lie in the semi-open
   interval [0, n). *)

module Make (N : sig
  val n: int
end) ()
: GSet.S with type element = int
