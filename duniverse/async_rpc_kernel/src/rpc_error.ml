open Core_kernel

include Protocol.Rpc_error
include Sexpable.To_stringable (Protocol.Rpc_error)

exception Rpc of t * Info.t [@@deriving sexp]
let raise t connection_description = raise (Rpc (t, connection_description))

let sexp_of_t t ~get_connection_close_reason = match t with
  | Connection_closed ->
    [%sexp
      `Connection_closed (get_connection_close_reason () : Sexp.t)]
  | Bin_io_exn _
  | Write_error _
  | Uncaught_exn _
  | Unimplemented_rpc _
  | Unknown_query_id _
   -> sexp_of_t t
