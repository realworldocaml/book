(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)

(* A register is a named variable. When one thinks about StackLang as an
   imperative language, a register can be thought of as a mutable global
   variable. In the translation of StackLang through IL to OCaml, a register
   becomes an immutable local variable. *)

module Reg = InternedString.Make()
type register = Reg.t
type registers = Reg.Set.t

(* -------------------------------------------------------------------------- *)

(* A code label is identified by its name. *)

module Label = InternedString.Make()
type label = Label.t

(* -------------------------------------------------------------------------- *)

(* A tag encodes a state of an LR automaton. *)

module Tag : sig

  type tag

  (**[make] converts an LR(1) state to a tag. *)
  val make : Lr1.node -> tag

  (**[lazy_make] converts an LR(1) state to a tag. The assertion
     [represented s] is delayed until the suspension is forced. *)
  val lazy_make : Lr1.node -> tag Lazy.t

  (**[print] converts a tag to a string. *)
  val print : tag -> string

  (**[compare] is a total order on tags. *)
  val compare : tag -> tag -> int

  module Set : Set.S with type elt = tag
  module Map : Map.S with type key = tag

end = struct

  type tag = int

  let make s =
    let tag = Lr1.number s in
    if not (Invariant.represented s) then begin
      Printf.eprintf "Attempt to use an unrepresented state: %d\n" tag;
      assert false
    end
    else
      tag

  let lazy_make s =
    lazy (make s)

  let print tag =
    Misc.padded_index Lr1.n tag

  let compare (tag1 : tag) (tag2 : tag) =
    compare tag1 tag2

  module Int = struct
    type t = int
    let compare (x : t) (y : t) = x - y
  end

  module Set = Set.Make(Int)
  module Map = Map.Make(Int)

end

type tag = Tag.tag

type tags = Tag.Set.t

(* -------------------------------------------------------------------------- *)

(** A terminal symbol. *)
type terminal =
  Grammar.Terminal.t

(** A set of terminal symbols. *)
type terminals =
  Grammar.TerminalSet.t

(** A start nonterminal symbol. *)
type start_nonterminal =
  Grammar.Nonterminal.t

(**A production index. *)
type production =
  Grammar.Production.index

(* -------------------------------------------------------------------------- *)

(**A value is a piece of data that can be pushed onto the stack. Values
   include the unit value, tags, and data loaded from a register. *)
type value =
  | VUnit
  | VTag of tag
  | VReg of register

type values =
  value list

(* -------------------------------------------------------------------------- *)

(**A pattern describes how to decompose and store a piece of data that is
   popped off the stack. Patterns include wildcards and registers. *)
type pattern =
  | PWildcard
  | PReg of register

type patterns =
  pattern list
