open! Core_kernel

include module type of struct include Protocol.Rpc_error end

val sexp_of_t : t -> get_connection_close_reason:(unit -> Sexp.t) -> Sexp.t

include Stringable.S with type t := t

exception Rpc of t * Info.t

val raise : t -> Info.t -> 'a
