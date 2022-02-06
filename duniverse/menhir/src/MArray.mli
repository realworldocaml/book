(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(** This module is an extension of Stdlib.Array *)

include module type of Array

(**[empty] is the empty array. *)
val empty : 'a array

(**[last a] is the last element of the array [a], which must be nonempty. *)
val last : 'a array -> 'a

(** [pop a] is [a] with its last element removed.
    [pop [|1; 2; 3; 4|] = [|1; 2; 3|]] *)
val pop : 'a array -> 'a array

(** [push a x] is [a] with [x] added at its end.
    [push [|1; 2; 3|] 4 = [|1; 2; 3; 4|]] *)
val push : 'a array -> 'a -> 'a array

(**If the array [a] has length greater than [k],
   then [truncate k a] is the suffix of length [k] of the array [a].
   Otherwise, [truncate k a] is [a].
   [truncate 3 [|1; 2; 3; 4; 5|] = [|3; 4; 5|]] *)
val truncate : int -> 'a array -> 'a array

(** [suffix a k] is [truncate k a]. *)
val suffix : 'a array -> int -> 'a array

(**[is_suffix equal a1 a2] tests whether [a1] is a suffix of [a2]. The
   elements are compared using the function [equal]. *)
val is_suffix : ('a -> 'a -> bool) -> 'a array -> 'a array -> bool

(**[greatest_suffix_forall p a] is the greatest suffix of the array [a]
   whose elements satisfy the predicate [p]. *)
val greatest_suffix_forall : ('a -> bool) -> 'a array -> 'a array

(**[rev a] is a new array whose elements are the elements of the array [a],
   in reverse order.
   [rev [|1; 2; 3; 4|] = [|4; 3; 2; 1|]]*)
val rev : 'a array -> 'a array

(**[rev_of_list] converts a list to an array.
   The list's head becomes the end of the array.
   [rev_of_list [1; 2; 3; 4; 5] = [|5; 4; 3; 2; 1|]] *)
val rev_of_list : 'a list -> 'a array

(**[rev_to_list] converts an array to a list.
   The end of the array becomes the list's head.
   [rev_to_list [|1; 2; 3; 4; 5|] = [5; 4; 3; 2; 1]] *)
val rev_to_list : 'a array -> 'a list

(**[iter_rev f a] is equivalent to [iter f (rev a)]. *)
val iter_rev : ('a -> unit) -> 'a array -> unit

(**The array [filter p a] contains the elements [x] of the array [a]
   such that [p x] is true. *)
val filter : ('a -> bool) -> 'a array -> 'a array

(**[existsi p a] tests whether there exists an index [i] such that
   [p i a.(i)] holds. *)
val existsi : (int -> 'a -> bool) -> 'a array -> bool

(**[count p a] counts how many elements of the array [a] satisfy the
   predicate [p]. *)
val count: ('a -> bool) -> 'a array -> int

(**[for_all] is identical to the function by the same name in the OCaml
   standard library. *)
val for_all : ('a -> bool) -> 'a array -> bool

(**[for_all2] is identical to the function by the same name in the OCaml
   standard library. *)
val for_all2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool

(**[fold_left2] performs left-to-right iteration over two arrays, which
   must have the same length, while carrying an accumulator. *)
val fold_left2 : ('a -> 'b1 -> 'b2 -> 'a) -> 'a -> 'b1 array -> 'b2 array -> 'a

(**Given a [leq_join] function on elements, [leq_join] constructs a [leq_join]
   function on arrays. The two arrays must have the same length. The
   specification of a [leq_join] is defined by the signature
   [Fix.MINIMAL_SEMI_LATTICE]. *)
val leq_join : ('a -> 'b -> 'b) -> 'a array -> 'b array -> 'b array

(** Return the index of the first element that satisfies a predicate, or
    raise [Not_found]. *)
val findi : (int -> 'a -> bool) -> int -> 'a array -> int

(** Unit tests. *)
val test : unit -> unit
