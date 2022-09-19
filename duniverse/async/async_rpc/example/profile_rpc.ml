open Core
open Async

let measure_words obj =
  let words = ref (Ocaml_value_size.words obj) in
  fun () ->
    let words' = Ocaml_value_size.words obj in
    let diff = words' - !words in
    words := words';
    diff
;;

let query_counter = ref 0

let words () =
  Gc.full_major ();
  (Gc.stat ()).live_words
;;

let direct_impl =
  Rpc.Pipe_rpc.implement_direct Rpc_intf.counter_values (fun () () writer ->
    incr query_counter;
    ignore
      (Rpc.Pipe_rpc.Direct_stream_writer.write_without_pushback writer 0
       : [ `Ok | `Closed ]);
    return (Ok ()))
;;

let server_cmd =
  Command.async
    ~summary:"A simple Async-RPC server with memory usage output"
    (let%map_open.Command port =
       flag "-port" ~doc:"INT Port to listen on" (optional_with_default 8080 int)
     in
     fun () ->
       let implementations =
         Rpc.Implementations.create
           ~on_unknown_rpc:`Close_connection
           ~implementations:[ direct_impl ]
       in
       let words_when_conn_created = ref 0 in
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
                  ~connection_state:(fun _ -> words_when_conn_created := words ())
                  ~on_handshake_error:`Ignore)
         in
         ignore (server : (_, _) Tcp.Server.t Deferred.t);
         Clock.every (Time.Span.of_sec 5.) (fun () ->
           let num_queries = !query_counter in
           if num_queries = 0
           then print_endline "(no queries)"
           else (
             let words = words () - !words_when_conn_created in
             printf "%d queries, %d words/query\n" num_queries (words / num_queries)));
         never ())
;;

let pipe_dispatch_exn rpc { Host_and_port.host; port } arg f =
  let reader, (_ : _ Pipe.Writer.t) = Pipe.create () in
  printf "1 pipe = %d words\n%!" (Ocaml_value_size.words reader);
  printf "1 queue = %d words\n%!" (Ocaml_value_size.words (Queue.create ()));
  printf "1 ivar = %d words\n%!" (Ocaml_value_size.words (Ivar.create ()));
  Rpc.Connection.with_client
    (Tcp.Where_to_connect.of_host_and_port { host; port })
    (fun conn ->
       let pipes = Queue.create () in
       let pipe_words = measure_words pipes in
       let rpc_words = measure_words conn in
       let start () =
         let n = 1_000 in
         for _ = 0 to n - 1 do
           don't_wait_for
             (match%map Rpc.Pipe_rpc.dispatch_iter rpc conn arg ~f >>| ok_exn with
              | Error () -> assert false
              | Ok id -> Queue.enqueue pipes id)
         done;
         let%map () = Clock.after (sec 5.) in
         printf "\n%d words/rpc, %d words/pipe\n%!" (rpc_words () / n) (pipe_words () / n)
       in
       Deferred.forever () start;
       never ())
  >>| Result.ok_exn
;;

let counter_values_exn addr =
  pipe_dispatch_exn Rpc_intf.counter_values addr () (fun _ ->
    printf ".";
    Continue)
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

let client_cmd =
  Command.async
    ~summary:"measure memory usage of client"
    (let%map.Command host_and_port = host_and_port_param in
     fun () -> counter_values_exn host_and_port)
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"profiling async rpc"
       [ "server", server_cmd; "client", client_cmd ])
;;
