open Base

(** A collection of string frequency counts *)
type t

(** The empty set of frequency counts  *)
val empty : t

(** Bump the frequency count for the given string. *)
val touch : t -> string -> t

(** Converts the set of frequency counts to an association list.
    Every string in the list will show up at most once, and the
    integers will be at least 1. *)
val to_list : t -> (string * int) list
