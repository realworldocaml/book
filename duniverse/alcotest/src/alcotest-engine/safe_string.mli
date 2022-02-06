type t
(** A UTF-8 encoded string that has been made safe for use in filesystems by
    escaping any "unsafe" characters as their [U+XXXX] notational form. *)

val v : string -> t

val to_string : t -> string
(** Get the escaped form of the given {!Safe_string}. *)

val to_unescaped_string : t -> string
(** Get the raw, unescaped string initially passed to [v]. *)

val pp : t Fmt.t
(** Pretty-print the unescaped string. *)

val length : t -> int
(** The approximate number of terminal columns consumed by [pp_name]. *)

val equal : t -> t -> bool
val compare : t -> t -> int
