(** A lazy string, implemented with [Info], but intended specifically for error
    messages. *)

open! Import

include Info_intf.S with type t = private Info.t (** @open *)

(** Note that the exception raised by this function maintains a reference to the [t]
    passed in. *)
val raise : t -> _

val raise_s : Sexp.t -> _

val to_info : t -> Info.t
val of_info : Info.t -> t
