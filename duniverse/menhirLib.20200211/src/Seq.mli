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

(* Sequences with constant time concatenation and linear-time conversion
   to an ordinary list. *)

type 'a seq

val empty: 'a seq
val singleton: 'a -> 'a seq
val append: 'a seq -> 'a seq -> 'a seq
val elements: 'a seq -> 'a list
val concat: 'a seq list -> 'a seq
val first: 'a seq -> 'a (* sequence must be nonempty *)
