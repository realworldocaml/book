(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open IL

(* The (code-based) code generator. *)

module Run (T : sig end) : sig

  val program: program

end

(* Some auxiliary functions are exposed for use by the new code back-end. *)

val print_token: string
val printtokendef: valdef
val assertfalsedef: valdef
val call_assertfalse: expr
