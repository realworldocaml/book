open Core
open Async_kernel
include Protocol.Rpc_error
include Sexpable.To_stringable (Protocol.Rpc_error)

exception Rpc of t * Info.t [@@deriving sexp]

let raise t connection_description = raise (Rpc (t, connection_description))

let sexp_of_t t ~get_connection_close_reason =
  match t with
  | Connection_closed ->
    [%sexp `Connection_closed (get_connection_close_reason () : Sexp.t)]
  | Bin_io_exn _
  | Write_error _
  | Uncaught_exn _
  | Unimplemented_rpc _
  | Unknown_query_id _ -> sexp_of_t t
;;

(* it would make sense to just take a [Connection.t], but we take its pieces instead to
   avoid a dependency cycle *)
let to_error
      t
      ~rpc_description:{ Description.name = rpc_name; version = rpc_version }
      ~connection_description
      ~connection_close_started
  =
  let rpc_error =
    sexp_of_t t ~get_connection_close_reason:(fun () ->
      let close_reason =
        (* Usually (always?) here we will have the deferred already full
           because Connection_closed error means the connection is already
           closed *)
        Deferred.peek connection_close_started
      in
      [%sexp (close_reason : Info.t option)])
  in
  Error.create_s
    [%sexp
      { rpc_error : Sexp.t
      ; connection_description : Info.t
      ; rpc_name : string
      ; rpc_version : int
      }]
;;
