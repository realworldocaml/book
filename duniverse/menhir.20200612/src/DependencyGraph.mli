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

(* Build and print the forward reference graph of the grammar. There is an edge
   of a nonterminal symbol [nt1] to every nonterminal symbol [nt2] that occurs
   in the definition of [nt1]. *)

val print_dependency_graph: unit -> unit

