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

(* -------------------------------------------------------------------------- *)

(* Constructors. *)

(** [def] is a smart constructor and can perform some simplification of
    consecutive bindings on the fly. *)
val def : bindings -> block -> block

(** [assign ps vs block] is short for [def (Bindings.assign ps vs) block]. *)
val assign : patterns -> values -> block -> block

(* -------------------------------------------------------------------------- *)

(* Partial evaluation. *)

(**[select_branch tag branches] determines which branch is chosen
   in an [ICaseTag] construct when the current state is [tag].
   The case analysis should be exhaustive; yet, if it isn't and
   if [tag] is not found, then [Not_found] is raised. *)
val select_branch : tag -> (tagpat * 'a) list -> 'a

(* -------------------------------------------------------------------------- *)

(* Visitors. *)

(** [map f block] transforms the block [block] by applying the function [f]
    to every immediate child. *)
val map :  (block -> block) -> block -> block

(** [iter f block] applies the function [f] to every immediate child of the
    block [block]. *)
val iter : (block -> unit) -> block -> unit

(** [jumps yield block] applies the function [yield] in turn to every
    label that is the target of a [jump] instruction in the block [block].
    The call takes the form [yield incasetag label], where [label] is the
    destination of the [jump] instruction, and where [incasetag] indicates
    whether this instruction lies inside a [casetag] instruction. *)
val jumps : (bool -> label -> unit) -> block -> unit

(** [successors yield block] applies the function [yield] in turn to every
    label that is the target of a [jump] instruction in the block [block]. *)
val successors : (label -> unit) -> block -> unit
