open! Core_kernel
module Deferred = Deferred1

type ('a, 'b, 'c) t = ('a, 'b, 'c) Map.t

val change
  :  ('k, 'v, 'comparator) t
  -> 'k
  -> f:('v option -> 'v option Deferred.t)
  -> ('k, 'v, 'comparator) t Deferred.t

val update
  :  ('k, 'v, 'comparator) t
  -> 'k
  -> f:('v option -> 'v Deferred.t)
  -> ('k, 'v, 'comparator) t Deferred.t

val iter_keys
  :  ?how:Monad_sequence.how
  -> ('k, _, _) t
  -> f:('k -> unit Deferred.t)
  -> unit Deferred.t

val iter
  :  ?how:Monad_sequence.how
  -> (_, 'v, _) t
  -> f:('v -> unit Deferred.t)
  -> unit Deferred.t

val iteri
  :  ?how:Monad_sequence.how
  -> ('k, 'v, _) t
  -> f:(key:'k -> data:'v -> unit Deferred.t)
  -> unit Deferred.t

val map
  :  ?how:Monad_sequence.how
  -> ('k, 'v1, 'comparator) t
  -> f:('v1 -> 'v2 Deferred.t)
  -> ('k, 'v2, 'comparator) t Deferred.t

val mapi
  :  ?how:Monad_sequence.how
  -> ('k, 'v1, 'comparator) t
  -> f:(key:'k -> data:'v1 -> 'v2 Deferred.t)
  -> ('k, 'v2, 'comparator) t Deferred.t

val fold
  :  ('k, 'v, _) t
  -> init:'a
  -> f:(key:'k -> data:'v -> 'a -> 'a Deferred.t)
  -> 'a Deferred.t

val fold_right
  :  ('k, 'v, _) t
  -> init:'a
  -> f:(key:'k -> data:'v -> 'a -> 'a Deferred.t)
  -> 'a Deferred.t

val filter_keys
  :  ?how:Monad_sequence.how
  -> ('k, 'v, 'comparable) t
  -> f:('k -> bool Deferred.t)
  -> ('k, 'v, 'comparable) t Deferred.t

val filter
  :  ?how:Monad_sequence.how
  -> ('k, 'v, 'comparable) t
  -> f:('v -> bool Deferred.t)
  -> ('k, 'v, 'comparable) t Deferred.t

val filteri
  :  ?how:Monad_sequence.how
  -> ('k, 'v, 'comparable) t
  -> f:(key:'k -> data:'v -> bool Deferred.t)
  -> ('k, 'v, 'comparable) t Deferred.t

val filter_map
  :  ?how:Monad_sequence.how
  -> ('k, 'v1, 'comparable) t
  -> f:('v1 -> 'v2 option Deferred.t)
  -> ('k, 'v2, 'comparable) t Deferred.t

val filter_mapi
  :  ?how:Monad_sequence.how
  -> ('k, 'v1, 'comparable) t
  -> f:(key:'k -> data:'v1 -> 'v2 option Deferred.t)
  -> ('k, 'v2, 'comparable) t Deferred.t

(*_ {[
    val compare
      :  ('v -> 'v -> int Deferred.t)
      -> ('k, 'v, 'comparator) t
      -> ('k, 'v, 'comparator) t
      -> int Deferred.t

    val equal
      :  ('v -> 'v -> bool Deferred.t)
      -> ('k, 'v, 'comparator) t
      -> ('k, 'v, 'comparator) t
      -> bool Deferred.t ]} *)

val merge
  :  ?how:Monad_sequence.how
  -> ('k, 'v1, 'comparator) t
  -> ('k, 'v2, 'comparator) t
  -> f:(key:'k
        -> [ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ]
        -> 'v3 option Deferred.t)
  -> ('k, 'v3, 'comparator) t Deferred.t

val all : ('k, 'v Deferred.t, 'comparator) t -> ('k, 'v, 'comparator) t Deferred.t

(*_ {[
    val fold_range_inclusive
      :  ('k, 'v, 'comparator) t
      -> min:'k
      -> max:'k
      -> init:'a
      -> f:(key:'k -> data:'v -> 'a -> 'a Deferred.t)
      -> 'a Deferred.t

    val of_alist_fold
      :  ('k * 'v1) list
      -> init:'v2
      -> f:('v2 -> 'v1 -> 'v2 Deferred.t)
      -> ('k, 'v2, 'comparator) t

    val of_alist_reduce
      :  ('k * 'v) list
      -> f:('v -> 'v -> 'v Deferred.t)
      -> ('k, 'v, 'comparator) t ]} *)
