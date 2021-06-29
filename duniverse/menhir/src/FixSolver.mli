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

open Fix

module Make
(M : IMPERATIVE_MAPS)
(P : MINIMAL_SEMI_LATTICE)
: sig

  type variable =
    M.key

  type property =
    P.property

  (* [record_ConVar x y] records an inequality between a constant and
     a variable. *)

  val record_ConVar: property -> variable -> unit

  (* [record_VarVar x y] records an inequality between two variables. *)

  val record_VarVar: variable -> variable -> unit

  (* The functor [Solve] computes the least solution of the
     constraints. The value [None] represents bottom. *)

  module Solve () :
    SOLUTION
    with type variable = variable
     and type property = property option

end
