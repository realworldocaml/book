(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module offers an imperative API for building a StackLang program.
   It is used in [EmitStackLang], where it allows the compiler to be written
   in a very natural style, almost in the same style as an interpreter. *)

open StackLang

(* -------------------------------------------------------------------------- *)

(** A program is built by invoking the functor [Build]. *)
module Build (A : sig

  (** A type of code addresses. *)
  type address

  (** An injection of code addresses into StackLang code labels. *)
  val print : address -> label

  (** A way of iterating over all addresses. *)
  val iter : (address -> unit) -> unit

  (** A mapping of addresses to code. The function call [code addr] is
      expected to use the imperative API below to build the code block
      that corresponds to the address [addr]. *)
  val code : address -> unit

  (** A family of entry addresses. This is a map of names (of start symbols)
      to addresses. *)
  val entry : address StringMap.t

  (** A map of states to type information. Only the states that are
      represented at runtime should appear in this map. *)
  val states : states

end) : sig

  (** A StackLang program. *)
  val program : program

end

(* -------------------------------------------------------------------------- *)

(* The following imperative API can be used by the function [code] above. *)

(* The following functions attach information to the current block. *)

val set_block_type : block_type -> unit
(**[set_block_type] sets the [block_type] field of the current code block.
   It must be called once per code block. *)

val set_hint: hint -> unit
(**[set_hint] sets the [hint] field of the current code block. It must be
   called at most once per code block. *)

val set_spec: spec -> unit
(**[set_spec] sets the [spec] field of the current code block. It must be
   called at most once per code block. *)

(* Each of the functions in the first group extends a code block that is
   currently under construction. Each of the functions in the second group
   ends the construction of the block. Each of the functions in the third
   group generates a case analysis construct, whose branches can then be
   independently constructed. *)

(* Group 1: Instructions with exactly one successor. *)

val push : values -> cell -> unit

val pop : patterns -> cell -> unit

val peek : patterns -> cell -> unit

val def : patterns -> values -> unit

val prim : register -> primitive -> unit
  (* specialized for a pattern of the form [PReg r] *)

val trace : trace -> unit

val comment : string -> unit

(* [move dst src] generates a move instruction from register [src] to
   register [dst]. It is a short-hand for [def (PReg dst) (VReg src)]. *)
val move : register -> register -> unit

(* Group 2: Instructions with zero successor. *)

val stop : int -> unit

val return : start_nonterminal -> value -> unit

val jump : label -> unit

(* Group 3: Case analysis instructions. *)

(* [case_token src cases] generates a case analysis instruction on a token,
   which is held in the register [src]. The user-provided function [cases] is
   provided with two functions, [branch] and [default], which allow generating
   an ordinary branch (guarded by a pattern) and generating a default branch.
   The default branch is implicitly discarded if the ordinary branches alone
   form an exhaustive case analysis. *)

val case_token :
     register
  -> (   ((* branch:  *) tokpat -> (unit -> unit) -> unit)
      -> (* default: *) ((unit -> unit) -> unit)
      -> unit )
  -> unit

(* [case_tag src cases] generates a case analysis instruction on a tag, which is
   held in the register [src]. The user-provided function [cases] is provided
   with one function, [branch], which allows generating a branch.

   The function [branch] expects a list of tags and a function that generates a
   block. If the list [tags] has more than one element, this is interpreted as a
   disjunction pattern. StackLang does not have disjunction patterns, so this
   disjunction is expanded away on the fly.

   The reason why the argument of [branch] is a list of lazy tags is that we do
   not want to call [Tag.make] when there is a single branch. Indeed, [Tag.make]
   cannot be applied to an unrepresented state.

   If there are zero branches, then the [casetag] instruction collapses and is
   replaced with a [DEAD] instruction. If there is one branch, then the [casetag]
   instruction disappears and is replaced with its unique branch. The number of
   branches is counted *before* disjunction patterns are expanded away.

   The Boolean result indicates whether a [casetag] instruction has actually
   been generated. *)

val case_tag :
     register
  -> (((* branch:  *) tag Lazy.t list -> (unit -> unit) -> unit) -> unit)
  -> bool
