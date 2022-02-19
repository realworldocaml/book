(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

include Set.Make (String)

(* [map] appears in OCaml 4.04. *)
let map f xs =
  fold (fun x accu -> add (f x) accu) xs empty

let print s =
  Misc.separated_iter_to_string (fun s -> s) ", " (fun f -> iter f s)
