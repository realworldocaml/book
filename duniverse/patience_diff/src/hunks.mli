open! Core

type 'a t = 'a Hunk.t list

(** [unified t] converts all Replace ranges in [t] to an Prev range followed by a Next
    range. *)
val unified : 'a t -> 'a t

(** [ranges t] concatenates all the ranges of all hunks together **)
val ranges : 'a t -> 'a Range.t list

val concat_map_ranges : 'a t -> f:('a Range.t -> 'b Range.t list) -> 'b t

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving sexp, bin_io]
  end
end
