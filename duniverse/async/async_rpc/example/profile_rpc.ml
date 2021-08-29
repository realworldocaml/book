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

type addr =
  { host : string
  ; port : int
  }

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
  Command.async_spec
    ~summary:"A simple Async-RPC server with memory usage output"
    Command.Spec.(
      empty +> flag "-port" ~doc:"INT Port to listen on" (optional_with_default 8080 int))
    (fun port () ->
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
         Deferred.never ())
;;

let pipe_dispatch rpc { host; port } arg f =
  let a = Pipe.create () in
  printf "1 pipe = %d words\n%!" (Ocaml_value_size.words (fst a));
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
             (Rpc.Pipe_rpc.dispatch_iter rpc conn arg ~f
              >>| ok_exn
              >>| function
              | Error () -> assert false
              | Ok id -> Queue.enqueue pipes id)
         done;
         Clock.after (sec 5.)
         >>| fun () ->
         printf "\n%d words/rpc, %d words/pipe\n%!" (rpc_words () / n) (pipe_words () / n)
       in
       Deferred.forever () start;
       never ())
  >>| Result.ok_exn
;;

let counter_values addr =
  pipe_dispatch Rpc_intf.counter_values addr () (fun _ ->
    printf ".";
    Continue)
;;

(* Setting up the command-line interface *)

let host_and_port () =
  let open Command.Spec in
  step (fun k host port -> k { host; port })
  +> flag "-host" ~doc:" server IP" (optional_with_default "127.0.0.1" string)
  +> flag "-port" ~doc:" server port" (optional_with_default 8080 int)
;;

let client_cmd =
  Command.async_spec
    ~summary:"measure memory usage of client"
    (host_and_port ())
    (fun addr () -> counter_values addr)
;;

let () =
  Command.run
    (Command.group
       ~summary:"profiling async rpc"
       [ "server", server_cmd; "client", client_cmd ])
;;
