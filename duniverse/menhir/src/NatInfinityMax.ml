(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

type property =
  int

let bottom =
  0

let infinity =
  max_int

let finite n =
  assert (0 <= n && n < infinity);
  n

let to_int p =
  p

let equal : property -> property -> bool =
  (=)

let is_maximal p =
  p = infinity

let max =
  max

let max_lazy p q =
  if p = infinity then infinity else max p (q())

let add p q =
  if p = infinity || q = infinity then infinity else p + q

let add_lazy p q =
  if p = infinity then infinity
  else let q = q() in if q = infinity then infinity else p + q

let print p =
  if p = infinity then "infinity" else string_of_int p
