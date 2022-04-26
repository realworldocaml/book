(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open StackLang

val print : program -> unit
(** [print program] dumps the control flow graph of the StackLang program
   [program] to the file [<basename>.dot]. The vertices are the basic blocks
   and the edges are the jumps between them. If there are multiple jumps from
   one block to another, then only one edge is shown. *)
