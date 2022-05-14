(** Internal Async debugging functions. *)

open! Core
include module type of Async_kernel_config.Print_debug_messages_for

(** Calls to [Debug.log] should look like [if Debug.??? then Debug.log ...]. *)
val log : string -> 'a -> ('a -> Sexp.t) -> unit

val log_string : string -> unit
