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

(* A debugging pretty-printer for [IL]. Newlines are used liberally, so as to
   facilitate diffs. *)

module Make (X : sig

  (* This is the channel that is being written to. *)

  val f: out_channel

end) : sig

  val expr: IL.expr -> unit

end

