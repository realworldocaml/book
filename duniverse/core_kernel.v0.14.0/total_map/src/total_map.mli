(** A map that includes an entry for every possible value of the key type.

    This is intended to be used on ['key] types where there is a full enumeration of the
    type. In the common use case, ['key] will be a simple variant type with [[@@deriving
    compare, enumerate]]. For example:

    {[
      module Arrow_key = struct
        module T = struct
          type t =
            | Up
            | Down
            | Left
            | Right
          [@@deriving sexp, bin_io, compare, enumerate]
        end
        include T
        module Total_map = Total_map.Make (T)
      end
    ]}

    In such a case, a [t] is semantically equivalent to a pure function from ['key] to
    ['value]. The differences are that it is serializable and that mapping or changing a
    [t] will produce a [t] using the same amount of space as the original.

    However, in theory you could also modify the comparison function and enumeration, so
    long as the enumeration contains at least one representative of each equivalence class
    determined by the comparison function.
*)

open! Core_kernel
open! Import
module Enumeration = Enumeration

type ('key, 'a, 'cmp, 'enum) t = private ('key, 'a, 'cmp) Map.t

val to_map : ('key, 'a, 'cmp, _) t -> ('key, 'a, 'cmp) Map.t

(** Many of the functions below have types reflecting the fact that the maps are total
    (e.g., [find] does not return an option).  The fact that they won't raise exceptions
    relies on the enumeration passed to [Make] being complete. *)

val map : ('key, 'a, 'c, 'e) t -> f:('a -> 'b) -> ('key, 'b, 'c, 'e) t
val mapi : ('key, 'a, 'c, 'e) t -> f:(key:'key -> data:'a -> 'b) -> ('key, 'b, 'c, 'e) t

val map2
  :  ('key, 'a, 'cmp, 'enum) t
  -> ('key, 'b, 'cmp, 'enum) t
  -> f:('a -> 'b -> 'c)
  -> ('key, 'c, 'cmp, 'enum) t

val iter_keys : ('key, _, _, _) t -> f:('key -> unit) -> unit
val iter : (_, 'a, _, _) t -> f:('a -> unit) -> unit
val iteri : ('key, 'a, _, _) t -> f:(key:'key -> data:'a -> unit) -> unit

val iter2
  :  ('key, 'a, 'cmp, 'enum) t
  -> ('key, 'b, 'cmp, 'enum) t
  -> f:(key:'key -> 'a -> 'b -> unit)
  -> unit

val fold
  :  ('key, 'a, _, _) t
  -> init:'acc
  -> f:(key:'key -> data:'a -> 'acc -> 'acc)
  -> 'acc

val fold_right
  :  ('key, 'a, _, _) t
  -> init:'acc
  -> f:(key:'key -> data:'a -> 'acc -> 'acc)
  -> 'acc

(** Folds over two maps side by side, like [iter2]. *)
val fold2
  :  ('key, 'a, 'cmp, 'enum) t
  -> ('key, 'b, 'cmp, 'enum) t
  -> init:'acc
  -> f:(key:'key -> 'a -> 'b -> 'acc -> 'acc)
  -> 'acc

val set : ('key, 'a, 'cmp, 'enum) t -> 'key -> 'a -> ('key, 'a, 'cmp, 'enum) t

val to_alist
  :  ?key_order:[ `Increasing | `Decreasing ] (** default is [`Increasing] *)
  -> ('key, 'a, _, _) t
  -> ('key * 'a) list

val find : ('key, 'a, _, _) t -> 'key -> 'a
val change : ('key, 'a, 'c, 'e) t -> 'key -> f:('a -> 'a) -> ('key, 'a, 'c, 'e) t
val data : (_, 'a, _, _) t -> 'a list
val for_all : (_, 'a, _, _) t -> f:('a -> bool) -> bool

(** Sequence a total map of computations in order of their keys resulting in computation
    of the total map of results. *)
module Sequence (A : Applicative) : sig
  val sequence : ('key, 'a A.t, 'cmp, 'enum) t -> ('key, 'a, 'cmp, 'enum) t A.t
end

(** The only reason that the Applicative interface isn't included here is that we don't
    have an [Applicative.S4]. *)

module type Key = sig
  type t [@@deriving sexp, bin_io, compare, enumerate]
end

module type Key_with_witnesses = sig
  include Key
  include Comparator.S with type t := t
  include Enumeration.S with type t := t
end

module type S = sig
  module Key : Key

  type comparator_witness
  type enumeration_witness

  type nonrec 'a t = (Key.t, 'a, comparator_witness, enumeration_witness) t
  [@@deriving sexp, bin_io, compare]

  include Applicative with type 'a t := 'a t

  val create : (Key.t -> 'a) -> 'a t
  val create_const : 'a -> 'a t
end

module Make (Key : Key) : S with module Key = Key

module Make_with_witnesses (Key : Key_with_witnesses) :
  S
  with module Key = Key
  with type comparator_witness = Key.comparator_witness
  with type enumeration_witness = Key.enumeration_witness

module Stable : sig
  module V1 : sig
    module type S = sig
      module Key : Key

      type comparator_witness
      type enumeration_witness

      type nonrec 'a t = (Key.t, 'a, comparator_witness, enumeration_witness) t
      [@@deriving bin_io, sexp, compare]
    end

    module Make_with_witnesses (Key : Key_with_witnesses) :
      S
      with module Key = Key
      with type comparator_witness = Key.comparator_witness
      with type enumeration_witness = Key.enumeration_witness
  end
end
