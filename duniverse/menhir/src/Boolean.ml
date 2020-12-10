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

(* The Boolean lattice. *)

type property =
    bool

let bottom =
  false

let equal (b1 : bool) (b2 : bool) =
  b1 = b2

let is_maximal b =
  b

let union (b1 : bool) (b2 : bool) =
  b1 || b2

