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

(* This module implements [--list-errors]. Its purpose is to find, for each
   pair of a state [s] and a terminal symbol [z] such that looking at [z] in
   state [s] causes an error, a minimal path (starting in some initial state)
   that actually triggers this error. *)

(* In this analysis, we explicitly ignore the [error] token. (We display a
   warning if the grammar uses this token.) Thus, we disregard any reductions
   or transitions that take place when the lookahead symbol is [error]. As a
   result, any state whose incoming symbol is [error] is found unreachable. It
   would be too complicated to have to create a first error in order to be
   able to take certain transitions or drop certain parts of the input. *)

module Run (X : sig

  (* If [verbose] is set, produce various messages on [stderr]. *)
  val verbose: bool

  (* If [statistics] is defined, it is interpreted as the name of
     a file to which one line of statistics is appended. *)
  val statistics: string option

end) : sig

  (* The result of this analysis is a [.messages] file. It is written to the
     standard output channel. No result is returned. *)

end
