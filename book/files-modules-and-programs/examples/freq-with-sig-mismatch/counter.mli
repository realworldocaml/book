open Base

(** A collection of string frequency counts *)
type t

(** The empty set of frequency counts  *)
val empty : t

(** Converts the set of frequency counts to an association list.  A string shows
    up at most once, and the counts are >= 1. *)
val to_list : t -> (string * int) list

[@@@part "1"] ;;

(** Bump the frequency count for the given string. *)
val touch : string -> t -> t

[@@@part "2"] ;;
