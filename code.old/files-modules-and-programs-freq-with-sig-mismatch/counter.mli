open Core.Std

(** A collection of string frequency counts *)
type t

(** The empty set of frequency counts  *)
val empty : t

(* part 1 *)
(** Bump the frequency count for the given string. *)
val touch : string -> t -> t
(* part 2 *)

(* Converts the set of frequency counts to an association list.  Every strings
   in the list will show up at most once, and the integers will be at least
   1. *)
val to_list : t -> (string * int) list
