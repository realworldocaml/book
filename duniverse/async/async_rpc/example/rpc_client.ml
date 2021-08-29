open Core
open Async

type addr =
  { host : string
  ; port : int
  }

let dispatch rpc { host; port } arg =
  Rpc.Connection.with_client
    (Tcp.Where_to_connect.of_host_and_port { host; port })
    (fun conn -> Rpc.Rpc.dispatch_exn rpc conn arg)
  >>| Result.ok_exn
;;

let pipe_dispatch rpc { host; port } arg f =
  Rpc.Connection.with_client
    (Tcp.Where_to_connect.of_host_and_port { host; port })
    (fun conn -> Rpc.Pipe_rpc.dispatch_exn rpc conn arg >>= fun (pipe, _) -> f pipe)
  >>| Result.ok_exn
;;

let set_id_counter addr new_id = dispatch Rpc_intf.set_id_counter addr new_id

let set_id_counter_v0 addr new_id_pair =
  dispatch Rpc_intf.set_id_counter_v0 addr new_id_pair
;;

let get_unique_id addr =
  dispatch Rpc_intf.get_unique_id addr () >>| fun id -> printf "UNIQUE ID: %d\n" id
;;

let counter_values addr =
  pipe_dispatch Rpc_intf.counter_values addr () (fun reader ->
    Pipe.iter_without_pushback reader ~f:(fun i -> printf "COUNTER: %d\n%!" i))
;;

(* Setting up the command-line interface *)

let host_and_port_spec () =
  let open Command.Spec in
  step (fun k host port -> k { host; port })
  +> flag "-host" ~doc:" server IP" (optional_with_default "127.0.0.1" string)
  +> flag "-port" ~doc:" server port" (optional_with_default 8080 int)
;;

let get_unique_id_cmd =
  Command.async_spec
    ~summary:"get unique id from server"
    (host_and_port_spec ())
    (fun addr () -> get_unique_id addr)
;;

let set_id_counter_cmd =
  Command.async_spec
    ~summary:"forcibly set the unique id counter.  DANGEROUS"
    Command.Spec.(host_and_port_spec () +> anon ("counter" %: int))
    (fun addr i () -> set_id_counter addr i)
;;

(* This one is actually unsupported by the server, so using it will trigger an error. *)
let set_id_counter_cmd_v0 =
  Command.async_spec
    ~summary:"forcibly set the unique id counter.  DANGEROUS"
    Command.Spec.(
      host_and_port_spec () +> anon ("counter1" %: int) +> anon ("counter2" %: int))
    (fun addr id1 id2 () -> set_id_counter_v0 addr (id1, id2))
;;

let counter_values_cmd =
  Command.async_spec
    ~summary:"subscribe to changes to counter id"
    (host_and_port_spec ())
    (fun addr () -> counter_values addr)
;;

let () =
  Command.run
    (Command.group
       ~summary:"Client for trivial Async-RPC server"
       [ "get-unique-id", get_unique_id_cmd
       ; "set-id-counter", set_id_counter_cmd
       ; "set-id-counter-v0", set_id_counter_cmd_v0
       ; "counter-values", counter_values_cmd
       ])
;;
