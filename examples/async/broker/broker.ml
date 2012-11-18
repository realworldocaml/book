open Core.Std
open Async.Std
open Protocol

(* CR yminsky: consider adding an RPC that blocks on the server side. *)
let publish_impl dir msg =
  Directory.publish dir msg;
  return ()

let subscribe_impl dir topic ~aborted =
  return (
    match Directory.subscribe dir topic with
    | None -> Error "Unknown topic"
    | Some pipe ->
      don't_wait_for (aborted >>| fun () -> Pipe.close_read pipe);
      Ok pipe
  )
;;

let dump_impl dir () =
  return (Directory.dump dir)

let shutdown_impl _dir () =
  (after (sec 0.1) >>> fun () -> shutdown 0);
  return ()

let implementations =
  [ Rpc.Rpc.     implement publish_rpc   publish_impl
  ; Rpc.Pipe_rpc.implement subscribe_rpc subscribe_impl
  ; Rpc.Rpc.     implement dump_rpc      dump_impl
  ; Rpc.Rpc.     implement shutdown_rpc  shutdown_impl
  ]

let start_server () =
  let server =
    match
      Rpc.Server.create
        ~implementations
        ~on_unknown_rpc:`Ignore
    with
    | Ok x -> x
    | Error (`Duplicate_implementations _) -> assert false
  in
  let directory = Directory.create () in
  Tcp.Server.create  ~on_handler_error:`Ignore
    (Tcp.on_port 8080)
    (fun _addr r w ->
      Rpc.Connection.server_with_close r w
        ~connection_state:directory
        ~on_handshake_error:`Ignore
        ~server
    )
  >>= fun server ->
  Tcp.Server.close_finished server

let () =
  don't_wait_for (start_server ());
  never_returns (Scheduler.go ())

