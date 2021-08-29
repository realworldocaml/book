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

(* This module writes a description of the automaton, before conflict
   resolution, to <basename>.automaton.

   It writes conflict explanations to the file <basename>.conflicts.

   Then, it performs conflict resolution and introduces extra reductions.

   Finally, if any warnings about the grammar have been emitted up to this
   point, and if [--strict] is enabled, then it stops Menhir, before going
   into the back-end.

   No functionality is offered by this module. *)
