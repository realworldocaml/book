open Core.Std
open Async.Std
open Protocol

(* Defaults are set here, and can be overridden by command-line arguments. *)
let hostname = ref "127.0.0.1"
let port = ref 8080

let host_and_port () =
  Command.Spec.(
    empty
    +> flag "-hostname" (optional_with_default "127.0.0.1" string)
      ~doc:" Broker's hostname"
    +> flag "-port" (optional_with_default 8080 int)
      ~doc:" Broker's port"
  )

(** [start_async f] runs the function f, shutting down when it's done, and also calls
    [Scheduler.go] to get the async event-loop started.  *)
let run_under_async f =
  don't_wait_for (f () >>| fun () -> shutdown 0);
  never_returns (Scheduler.go ())

let with_rpc_conn f ~host ~port =
  Tcp.with_connection
    (Tcp.to_host_and_port host port)
    ~timeout:(sec 1.)
    (fun r w ->
      Rpc.Connection.create r w ~connection_state:()
      >>= function
      | Error exn -> raise exn
      | Ok conn -> f conn
    )

let shell cmd args =
  In_thread.run (fun () ->
    try Ok (Core_extended.Shell.run_full cmd args)
    with exn -> Error exn)

let shutdown =
  with_rpc_conn (fun conn ->
    Rpc.Rpc.dispatch shutdown_rpc conn ()
    >>= function
    | Ok  () -> return ()
    | Error exn -> eprintf "failed!\n"; return ()
  )

let shutdown_cmd =
  Command.basic (host_and_port ())
    ~summary:"Shut the broker down"
    (fun host port ->
      run_under_async (fun () -> shutdown ~host ~port))


let publish ~topic ~text =
  with_rpc_conn (fun conn ->
    shell "whoami" []
    >>= fun username ->
    let username = Result.ok_exn username in
    let from = Username.of_string (String.strip username) in
    Rpc.Rpc.dispatch_exn publish_rpc conn
      { Message.
        text; topic; from; time = Time.now () }
  )

let pub_cmd = Command.basic
  ~summary:"publish a single value"
  Command.Spec.(
    (host_and_port ())
    +> anon ("<topic>" %: Arg_type.create Topic.of_string)
                          +> anon ("<text>" %: string)
  )
  (fun host port topic text ->
    run_under_async (fun () -> publish ~host ~port ~topic ~text))

let subscribe ~topic =
  with_rpc_conn (fun conn ->
    shell "clear" []
    >>= fun clear_string ->
    let clear_string =
      (* if we're not on a terminal, just use the empty string *)
      match clear_string with
      | Ok s -> s | Error _ -> ""
    in
    Rpc.Pipe_rpc.dispatch subscribe_rpc conn topic
    >>= function
    | Error err -> Error.raise err
    | Ok (Error s) -> eprintf "subscribe failed: %s\n" s; return ()
    | Ok (Ok (pipe,_id)) ->
      Pipe.iter pipe ~f:(fun msg ->
        printf "%s%s\n%!" clear_string msg.Message.text;
        return ()
      ))

let sub_cmd = Command.basic
  ~summary:"subscribe to a topic"
  Command.Spec.(
    host_and_port ()
    +> anon ("<topic>" %: Arg_type.create Topic.of_string)
  )
  (fun host port topic -> run_under_async (fun () -> subscribe ~host ~port ~topic))


let dump =
  with_rpc_conn (fun conn ->
    Rpc.Rpc.dispatch_exn dump_rpc conn ()
    >>= fun dump ->
    printf "%s\n"
      (Dump.sexp_of_t dump |! Sexp.to_string_hum);
    return ()
  )

let dump_cmd = Command.basic
  ~summary:"Get a full dump of the broker's state"
  (host_and_port ())
  (fun host port -> run_under_async (fun () -> dump ~host ~port))

let () =
  Exn.handle_uncaught ~exit:true (fun () ->
    Command.run
      (Command.group ~summary:"Utilities for interacting with message broker"
         [ "publish"  , pub_cmd
         ; "subscribe", sub_cmd
         ; "dump"     , dump_cmd
         ; "shutdown" , shutdown_cmd
         ]))


