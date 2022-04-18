(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module offers a pretty-printer for StackLang. *)

open StackLang

module ToChannel : sig

  type 'a printer =
    out_channel -> 'a -> unit

  (**[instruction] prints only the first instruction of the block. *)
  val instruction : block printer

  val block : block printer

  val typed_block : typed_block printer

  val states : states printer

  val program : program printer

end

module ToString : sig

  type 'a printer =
    'a -> string

  val final : final printer

  val values : values printer

  val patterns : patterns printer

end
