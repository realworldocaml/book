(** For handling diffs abstractly.  A range is a subarray of the two original arrays with
    a constructor defining its relationship to the two original arrays.  A [Same] range
    contains a series of elements which can be found in both arrays.  A [Next] range
    contains elements found only in the second array, while an [Prev] range contains
    elements found only in the first array.

    A [Replace] contains two arrays: elements in the first output array are elements found
    only in the first input array, which have been replaced by elements in the second
    output array, which are elements found only in the second input array. *)

type 'a t =
  | Same of ('a * 'a) array
  | Prev of 'a array
  | Next of 'a array
  | Replace of 'a array * 'a array
  | Unified of 'a array
[@@deriving sexp]

(** [all_same ranges] returns true if all [ranges] are Same *)
val all_same : 'a t list -> bool

(** [prev_only ranges] drops all Next ranges and converts all Replace ranges to Prev
    ranges. *)
val prev_only : 'a t list -> 'a t list

(** [next_only ranges] drops all Prev ranges and converts all Replace ranges to Next
    ranges. *)
val next_only : 'a t list -> 'a t list

(** Counts number of elements. *)
val prev_size : 'a t -> int

val next_size : 'a t -> int

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving sexp, bin_io]
  end
end
