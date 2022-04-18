(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open StackLangBasics

(**A set of bindings is a finite map of registers to values. It can be thought
   of as a simultaneous assignment of values to registers; it is analogous to
   an OCaml [let/and] construct. *)
type t

(* Constructors. *)

val empty : t
(**[empty] is an empty set of bindings. *)

val assign : patterns -> values -> t
(**[assign_many ps vs] represents the assignment [ps := vs].
   The lists [ps] and [vs] must have the same length.
   No register must appear twice in the patterns [ps]. *)

val seq : t -> t -> t
(**[seq bs1 bs2] returns a set of bindings that is equivalent to the
   sequential composition of the bindings [bs1] and [bs2]. Thus, [IDef (bs1,
   IDef (bs2, block))] is equivalent to [IDef (seq bs1 bs2, block)]. *)

val par : t -> t -> t
(**[par bs1 bs2] returns a set of bindings that is equivalent to the parallel
   composition of the bindings [bs1] and [bs2]. [bs1]'s and [bs2]'s domains
   must be disjoint. *)

val remove : t -> registers -> t
(**[remove bs rs] is the set of bindings [bs], deprived of the bindings that
   concern the registers [rs]. *)

val restrict : registers -> t -> t
(**[restrict rs bs] is the set of bindings [bs], restricted to the registers
   [rs]. *)

(* Accessors. *)

val is_empty : t -> bool
(**[is_empty bs] determines whether [bs] is empty. *)

val mem : register -> t -> bool
(** [mem r bs] determines whether the register [r] appears in the domain
    of [bs]. *)

val domain : t -> registers
(**[domain bs] is the domain of [bs], that is, the set of registers assigned
   by [bs]. *)

val codomain : t -> registers
(**[codomain bs] is the codomain of [bs], that is, the set of registers
   mentioned in the values that appear in the right-hand sides of [bs]. *)

val to_list : t -> (register * value) list
(**[to_list bs] is the set of bindings [bs], viewed as a list of
   register/value pairs, in an unspecified order. *)

val iter : (register -> value -> unit) -> t -> unit
(** [iter] iterates over a set of bindings, in an unspecified order. *)

val fold : (register -> value -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold] iterates over a set of bindings, in an unspecified order. *)

val apply : t -> value -> value
(**[apply bs v] applies the bindings [bs], viewed as a substitution of values
   for registers, to the value [v]. The instruction [IReturn (apply bs v)] is
   equivalent to the instruction [IDef (bs, IReturn v)]. *)
