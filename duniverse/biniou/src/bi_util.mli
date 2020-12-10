(** Error handling etc. *)

exception Error of string
  (** Multipurpose exception normally raised when invalid data
      is found by a read or write operation. *)

val error : string -> 'a
  (** [error msg] is equivalent to [raise (Error msg)]. *)


(**/**)

val string8_of_int : int -> string
val string4_of_int : int -> string
val print_bits : ?pos:int -> ?len:int -> string -> string

val int_size : int
