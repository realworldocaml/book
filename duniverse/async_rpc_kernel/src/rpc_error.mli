open! Core
open! Async_kernel

include module type of struct
  include Protocol.Rpc_error
end

val sexp_of_t : t -> get_connection_close_reason:(unit -> Sexp.t) -> Sexp.t

include Stringable.S with type t := t

exception Rpc of t * Info.t

val raise : t -> Info.t -> 'a

val to_error
  :  t
  -> rpc_description:Description.t
  -> connection_description:Info.t
  -> connection_close_started:Info.t Deferred.t
  -> Error.t
