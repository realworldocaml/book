open Base

(** A collection of string frequency counts *)
type t

(** The empty set of frequency counts  *)
val empty : t

[@@@part "1"] ;;

(** Bump the frequency count for the given string. *)
val touch : t -> string -> t

[@@@part "2"] ;;
