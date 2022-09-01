open Core
open Async_kernel

type t = Implementation_types.On_exception.t =
  { callback : (exn -> unit) option
  ; close_connection_if_no_return_value : bool
  }
[@@deriving sexp_of]

val close_connection : t
val continue : t
val handle_exn : t -> close_connection_monitor:Monitor.t -> Exn.t -> unit
