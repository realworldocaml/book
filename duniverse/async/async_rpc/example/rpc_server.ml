open Core
open Async

(* The list of implementations supported by the server.  The server state is simply a
   counter used for allocating unique ids. *)
let implementations =
  [ Rpc.Rpc.implement Rpc_intf.get_unique_id (fun ctr () ->
      printf ".%!";
      incr ctr;
      return !ctr)
  ; Rpc.Rpc.implement Rpc_intf.set_id_counter (fun ctr i ->
      printf "!%!";
      if i = 0 then failwith "Can't set counter back to zero";
      return (ctr := i))
  ; Rpc.Pipe_rpc.implement Rpc_intf.counter_values (fun ctr () ->
      let r, w = Pipe.create () in
      let last_value = ref !ctr in
      let send () =
        last_value := !ctr;
        Pipe.write w !ctr
      in
      don't_wait_for (send ());
      Clock.every' ~stop:(Pipe.closed w) (sec 0.1) (fun () ->
        if !last_value <> !ctr then send () else return ());
      return (Ok r))
  ]
;;

let main ~port =
  let counter = ref 0 in
  let implementations =
    Rpc.Implementations.create ~implementations ~on_unknown_rpc:`Close_connection
  in
  match implementations with
  | Error (`Duplicate_implementations _descrs) -> assert false
  | Ok implementations ->
    let server =
      Tcp.Server.create
        (Tcp.Where_to_listen.of_port port)
        ~on_handler_error:`Ignore
        (fun _addr reader writer ->
           Rpc.Connection.server_with_close
             reader
             writer
             ~implementations
             ~connection_state:(fun _ -> counter)
             ~on_handshake_error:`Ignore)
    in
    ignore (server : (_, _) Tcp.Server.t Deferred.t);
    Deferred.never ()
;;

let () =
  Command.async_spec
    ~summary:"A trivial Async-RPC server"
    Command.Spec.(
      empty +> flag "-port" ~doc:" Port to listen on" (optional_with_default 8080 int))
    (fun port () -> main ~port)
  |> Command.run
;;
