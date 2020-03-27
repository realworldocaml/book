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

(** This module implements a very simple notion of ``mark''. A mark is
    really a reference cell (without content). Creating a new mark
    requires allocating a new cell, and comparing marks requires
    comparing pointers. *)

type t =
    unit ref

let fresh =
  ref

let same =
  (==)

let none =
  fresh()

