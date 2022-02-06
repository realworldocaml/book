(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* The side effects in this module are executed after the LR(1) automaton has
   been constructed and after conflict resolution has taken place. Thus, the
   LR(1) is now frozen and will no longer be modified. This module runs before
   the costly analyses in Invariant are performed. *)
