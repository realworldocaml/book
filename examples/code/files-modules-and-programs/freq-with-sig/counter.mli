open Core_kernel

(** Bump the frequency count for the given string. *)
val touch : (string * int) list -> string -> (string * int) list
