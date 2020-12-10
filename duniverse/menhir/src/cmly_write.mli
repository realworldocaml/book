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

(* [write filename] queries the modules [Front] and [Grammar] for information
   about the grammar and queries the modules [Lr0] and [Lr1] for information
   about the automaton. It writes this information to the .cmly file
   [filename]. *)

val write: string -> unit
