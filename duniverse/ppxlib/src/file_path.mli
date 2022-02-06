open! Import

(** Return the path used as root in a file *)

val get_default_path : Location.t -> string

val get_default_path_str : structure -> string

val get_default_path_sig : signature -> string
