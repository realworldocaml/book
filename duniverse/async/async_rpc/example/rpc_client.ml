open Core
open Async

let dispatch_exn rpc { Host_and_port.host; port } arg =
  Rpc.Connection.with_client
    (Tcp.Where_to_connect.of_host_and_port { host; port })
    (fun conn -> Rpc.Rpc.dispatch_exn rpc conn arg)
  >>| Result.ok_exn
;;

let pipe_dispatch_exn rpc { Host_and_port.host; port } arg f =
  Rpc.Connection.with_client
    (Tcp.Where_to_connect.of_host_and_port { host; port })
    (fun conn ->
       let%bind pipe, (_ : Rpc.Pipe_rpc.Metadata.t) =
         Rpc.Pipe_rpc.dispatch_exn rpc conn arg
       in
       f pipe)
  >>| Result.ok_exn
;;

let set_id_counter addr new_id = dispatch_exn Rpc_intf.set_id_counter addr new_id

let set_id_counter_v0 addr new_id_pair =
  dispatch_exn Rpc_intf.set_id_counter_v0 addr new_id_pair
;;

let get_unique_id addr =
  let%map id = dispatch_exn Rpc_intf.get_unique_id addr () in
  printf "UNIQUE ID: %d\n" id
;;

let counter_values addr =
  pipe_dispatch_exn Rpc_intf.counter_values addr () (fun reader ->
    Pipe.iter_without_pushback reader ~f:(fun i -> printf "COUNTER: %d\n%!" i))
;;

(* Setting up the command-line interface *)

let host_and_port_param =
  let open Command.Param in
  flag
    "-host-and-port"
    (optional_with_default
       { Host_and_port.host = "127.0.0.1"; port = 8080 }
       host_and_port)
    ~doc:"HOST:PORT server host and port"
;;

let get_unique_id_cmd =
  Command.async
    ~summary:"get unique id from server"
    (let%map.Command host_and_port = host_and_port_param in
     fun () -> get_unique_id host_and_port)
;;

let set_id_counter_cmd =
  Command.async
    ~summary:"forcibly set the unique id counter.  DANGEROUS"
    (let%map_open.Command host_and_port = host_and_port_param
     and i = anon ("counter" %: int) in
     fun () -> set_id_counter host_and_port i)
;;

(* This one is actually unsupported by the server, so using it will trigger an error. *)
let set_id_counter_cmd_v0 =
  Command.async
    ~summary:"forcibly set the unique id counter.  DANGEROUS"
    (let%map_open.Command host_and_port = host_and_port_param
     and id1 = anon ("counter1" %: int)
     and id2 = anon ("counter2" %: int) in
     fun () -> set_id_counter_v0 host_and_port (id1, id2))
;;

let counter_values_cmd =
  Command.async
    ~summary:"subscribe to changes to counter id"
    (let%map.Command host_and_port = host_and_port_param in
     fun () -> counter_values host_and_port)
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Client for trivial Async-RPC server"
       [ "get-unique-id", get_unique_id_cmd
       ; "set-id-counter", set_id_counter_cmd
       ; "set-id-counter-v0", set_id_counter_cmd_v0
       ; "counter-values", counter_values_cmd
       ])
;;
