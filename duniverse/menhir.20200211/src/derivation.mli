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

open Grammar

(* -------------------------------------------------------------------------- *)

(* This is the type of derivations. Derivations are forests: see inside. *)

type t

(* This is the type of derivations contexts, or derivations with a
   derivation-shaped hole. *)

type context

(* -------------------------------------------------------------------------- *)

(* Construction. *)

(* [empty] is the forest that consists of a single empty tree. *)

val empty: t

(* [tail pos rhs] is the forest: (i) whose first element is the empty tree,
   and (ii) whose remaining elements are the symbols found at positions
   greater than or equal to [pos] in the array [rhs]. *)

val tail: int -> Symbol.t array -> t

(* [build pos rhs forest comment] is the forest: (i) whose first element is
   the tree that has the non-terminal symbol [rhs.(pos)] at its root and the
   forest [forest] below its root, and (ii) whose remaining elements are the
   symbols found at positions greater than [pos] in the array [rhs]. *)

val build: int -> Symbol.t array -> t -> string option -> t

(* [prepend symbol forest] is the forest: (i) whose first element is the
   symbol [symbol], and (ii) whose remaining elements form the forest
   [forest]. *)

val prepend: Symbol.t -> t -> t

(* -------------------------------------------------------------------------- *)

(* Factoring. *)

(* [factor] factors the maximal common derivation context out of a
   nonempty family of derivations. It produces a pair of the context
   and of the residual derivations. *)

val factor: t Item.Map.t -> context * t Item.Map.t

(* -------------------------------------------------------------------------- *)

(* Display. *)

(* [print] prints a derivation. *)

val print: out_channel -> t -> unit

(* [printc] prints a derivation context. *)

val printc: out_channel -> context -> unit

