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

(* We select an implementation based on [n]. If [n] is less than or equal to
   [AtomicBitSet.bound], then we can use a single-word bit set, which requires
   no memory allocation. If [n] is less than or equal to [DWordBitSet.bound],
   then we can use a double-word bit set. And so on, up to quad-word bit sets,
   which are about as far as I am willing to go. Otherwise, we fall back on
   [SparseBitSet], which can represent integers of unbounded magnitude. *)

(* The functor [Make] must take a dummy argument [()] in order to indicate
   that it is not an applicative functor. Otherwise, we get a cryptic type
   error message: "This expression creates fresh types. It is not allowed
   inside applicative functors." *)

module Make (N : sig
  val n: int
end) ()
= struct

  (* An [if] construct in the module language would be welcome. This encoding
     is horrible. *)

  module type S =
    GSet.S with type element = int

  include (val
    if N.n <= AtomicBitSet.bound then
      (module AtomicBitSet : S)
    else if N.n <= DWordBitSet.bound then
      (module DWordBitSet : S)
    else if N.n <= QWordBitSet.bound then
      (module QWordBitSet : S)
    else
      (module SparseBitSet : S)
    : S)

end
