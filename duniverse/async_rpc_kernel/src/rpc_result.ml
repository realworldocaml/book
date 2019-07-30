open Core_kernel
open Async_kernel

type 'a t = ('a, Rpc_error.t) Result.t [@@deriving bin_io]

type located_error =
  { location : string
  ; exn : Exn.t
  }
[@@deriving sexp_of]

let uncaught_exn ~location exn =
  Error (Rpc_error.Uncaught_exn (sexp_of_located_error { location; exn }))
;;

let bin_io_exn ~location exn =
  Error (Rpc_error.Bin_io_exn (sexp_of_located_error { location; exn }))
;;

let try_with ?run ~location f =
  let x = Monitor.try_with ?run f in
  let join = function
    | Ok x -> x
    | Error exn -> uncaught_exn ~location exn
  in
  match Deferred.peek x with
  | None -> x >>| join
  | Some x -> return (join x)
;;

(* it would make sense to just take a [Connection.t], but we take its pieces instead to
   avoid a dependency cycle *)
let or_error ~rpc_tag ~rpc_version ~connection_description ~connection_close_started =
  function
  | Ok x -> Ok x
  | Error (rpc_error : Rpc_error.t) ->
    let rpc_error =
      Rpc_error.sexp_of_t rpc_error
        ~get_connection_close_reason:(fun () ->
          let close_reason =
            (* Usually (always?) here we will have the deferred already full
               because Connection_closed error means the connection is already
               closed *)
            Deferred.peek connection_close_started
          in
          [%sexp (close_reason : Info.t option)])
    in
    Or_error.error_s
      [%sexp
        { rpc_error = (rpc_error : Sexp.t)
        ; connection_description = (connection_description : Info.t)
        ; rpc_tag = (rpc_tag : Protocol.Rpc_tag.t)
        ; rpc_version = (rpc_version : int)
        }]
;;
