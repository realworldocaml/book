(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* The [Int] module has become standard in OCaml 4.08.0. *)

type t = int

let equal : int -> int -> bool =
  (=)

let compare : int -> int -> int =
  compare
