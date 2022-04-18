(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open StackLangBasics
open Reg.Set

let add_registers accu p =
  match p with
  | PWildcard ->
      accu
  | PReg r ->
      add r accu

let restrict1 rs p =
  match p with
  | PWildcard ->
      PWildcard
  | PReg r ->
      if Reg.Set.mem r rs then p else PWildcard

let occurs1 r p =
  match p with
  | PWildcard ->
      false
  | PReg r' ->
      r = r'

let rec occurs r ps =
  match ps with
  | [] ->
      false
  | p :: ps ->
      occurs1 r p || occurs r ps

let registers ps =
  List.fold_left add_registers empty ps

let restrict rs ps =
  List.map (restrict1 rs) ps
