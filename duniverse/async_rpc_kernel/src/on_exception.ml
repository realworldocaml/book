open Core
open Async_kernel

type t = Implementation_types.On_exception.t =
  { callback : (exn -> unit) option [@sexp.omit_nil]
  ; close_connection_if_no_return_value : bool
  }
[@@deriving sexp_of]

let close_connection = { callback = None; close_connection_if_no_return_value = true }
let continue = { callback = None; close_connection_if_no_return_value = false }

let handle_exn
      { callback; close_connection_if_no_return_value }
      ~close_connection_monitor
      exn
  =
  Option.iter callback ~f:(fun callback -> callback exn);
  if close_connection_if_no_return_value
  then Monitor.send_exn close_connection_monitor exn
;;
