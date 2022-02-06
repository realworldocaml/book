(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(** This module is an extension of Stdlib.List *)

include module type of List

(** An alias for [List.init], when it exists. *)
val init : int -> (int -> 'a) -> 'a list

(** A list subject to a condition. (Be careful, though: the list is of course
    constructed even if the condition is false.) *)
val ifn : bool -> 'a list -> 'a list

(** A list subject to a condition. (Be careful, though: the list is of course
    constructed even if the condition is false.) *)
val if1 : bool -> 'a -> 'a list

(** A cons subject to a condition. *)
val cons_if : bool -> 'a -> 'a list -> 'a list

(** A lazy version of [ifn], where the list is constructed only if the condition
    is true. *)
val ifnlazy : bool -> (unit -> 'a list) -> 'a list

(** The sum of a list of integers. *)
val sum : int list -> int

(** [at_most k xs] tests whether the list [xs] contains at most [k] elements. *)
val at_most : int -> 'a list -> bool

(** [drop k xs] is the list [xs] deprived of its first [k] elements. *)
val drop : int -> 'a list -> 'a list

(** [take k xs] is the list of the first [k] elements of the list [xs]. *)
val take : int -> 'a list -> 'a list

(**Given a [leq_join] function on elements, [leq_join] constructs a [leq_join]
   function on lists. The two lists must have the same length. The
   specification of a [leq_join] is defined by the signature
   [Fix.MINIMAL_SEMI_LATTICE]. *)
val leq_join : ('a -> 'b -> 'b) -> 'a list -> 'b list -> 'b list

(** Group equivalent elements of a list.
    [group_by ~compare ~group xs] sorts the list [xs] using [compare] and then
    groups runs of equivalent elements using [group].
    The order of the elements in [xs] is not preserved, neither between groups
    nor between the elements of the same group.  *)
val group_by :
  compare:('a -> 'a -> int) -> group:('a -> 'a list -> 'b) ->
  'a list -> 'b list

(** [find_map f xs] applies [f] to elements of [xs] in order and the returns
    the first result of the form [Some y], or [None]. *)
val find_map : ('a -> 'b option) -> 'a list -> 'b option

(** [partition_map f xs] classifies elements of list [xs] in a left and a right
    lists according to the result of [f]. *)
val partition_map :
  ('a -> [< `L of 'l | `R of 'r ]) -> 'a list -> 'l list * 'r list

(** [compare f l1 l2] compares two list according to the lexicographic
    ordering. Elements are compared using the [f] argument. *)
val compare : ('a -> 'b -> int) -> 'a list -> 'b list -> int
