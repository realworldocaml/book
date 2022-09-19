open! Core
open! Async
open! Import

(* [Rpc.Transport.Tcp] is well exercised by the fact that it is used to implement
   [Rpc.Connection.client] and [Rpc.Connection.serve], and they have extensive tests (not
   to mention a large number of users). Nonetheless, it is exposed in the interface, so at
   least deserves a test to check that it works. *)

let string_string_rpc =
  Rpc.Rpc.create
    ~name:"string->string"
    ~version:1
    ~bin_query:[%bin_type_class: string]
    ~bin_response:[%bin_type_class: string]
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ Rpc.Rpc.implement' string_string_rpc (fun state query -> state ^ " " ^ query) ]
;;

let where_to_listen = Tcp.Where_to_listen.bind_to Localhost On_port_chosen_by_os

let%expect_test "basic functionality" =
  let server_listening_on_set_once = Set_once.create () in
  let%bind server =
    Rpc.Transport.Tcp.serve ~where_to_listen (fun ~client_addr ~server_addr transport ->
      (* [server_addr] should be correct... *)
      [%test_eq: Socket.Address.Inet.t]
        server_addr
        (Set_once.get_exn server_listening_on_set_once [%here]);
      (* the [client_addr] should be distinct (we don't have any easy way of checking
         whether or not is correct). *)
      assert (not ([%compare.equal: Socket.Address.Inet.t] server_addr client_addr));
      (* we can create a connection using this transport *)
      let%bind connection =
        Async_rpc_kernel.Rpc.Connection.create
          ~implementations
          ~connection_state:(fun _ -> "server-state")
          transport
      in
      let connection = Result.ok_exn connection in
      Rpc.Connection.close_finished connection)
  in
  let server_listening_on = Tcp.Server.listening_on_address server in
  Set_once.set_exn server_listening_on_set_once [%here] server_listening_on;
  (* we can connect to the server *)
  let%bind client_transport =
    Rpc.Transport.Tcp.connect (Tcp.Where_to_connect.of_inet_address server_listening_on)
  in
  let client_transport, server_addr = Result.ok_exn client_transport in
  [%test_eq: Socket.Address.Inet.t] server_addr server_listening_on;
  (* we can create a connection using that transport. *)
  let%bind client_conn =
    Async_rpc_kernel.Rpc.Connection.create
      ~implementations
      ~connection_state:(fun _ -> "server-state")
      client_transport
  in
  let client_conn = Result.ok_exn client_conn in
  let%bind result = Rpc.Rpc.dispatch_exn string_string_rpc client_conn "query" in
  print_endline result;
  [%expect {| server-state query |}];
  let%bind () = Rpc.Connection.close client_conn in
  let%bind () = Tcp.Server.close server in
  return ()
;;

let with_timeout ~context deferred =
  let generous_timeout_for_hydra = sec 30. in
  match%map Clock.with_timeout generous_timeout_for_hydra deferred with
  | `Timeout -> printf "BUG: timeout while waiting for %s\n" context
  | `Result () -> ()
;;

let%expect_test "transport is closed when handle_transport returns #1" =
  let server_transport = Ivar.create () in
  let%bind server =
    Rpc.Transport.Tcp.serve
      ~where_to_listen
      (fun ~client_addr:_ ~server_addr:_ transport ->
         Ivar.fill server_transport transport;
         (* if we return immediately, the transport is closed. *)
         return ())
  in
  let%bind _sock, reader, writer =
    Tcp.connect
      (Tcp.Where_to_connect.of_inet_address (Tcp.Server.listening_on_address server))
  in
  let%bind server_transport = Ivar.read server_transport in
  (* the transport is closed immediately, so the client reader returns EOF. *)
  let%bind result = Reader.peek reader ~len:1 in
  print_s [%sexp (result : string Reader.Read_result.t)];
  [%expect {| Eof |}];
  let%bind () = Writer.close writer in
  let%bind () = Reader.close reader in
  (* and everything is cleared up server side *)
  let%bind () =
    with_timeout
      ~context:"server-side writer stopped"
      (Rpc.Transport.Writer.stopped server_transport.writer)
  in
  let%bind () = Tcp.Server.close server in
  [%expect {| |}];
  return ()
;;

let%expect_test "transport is closed when handle_transport returns #2" =
  let server_connection = Ivar.create () in
  let server_has_finished_with_connection = Ivar.create () in
  let%bind server =
    Rpc.Transport.Tcp.serve
      ~where_to_listen
      (fun ~client_addr:_ ~server_addr:_ transport ->
         let%bind connection =
           Async_rpc_kernel.Rpc.Connection.create
             ~implementations
             ~connection_state:(fun _ -> "server-state")
             transport
         in
         let connection = Result.ok_exn connection in
         Ivar.fill server_connection connection;
         Ivar.read server_has_finished_with_connection)
  in
  let%bind client_connection =
    Rpc.Connection.client
      (Tcp.Where_to_connect.of_inet_address (Tcp.Server.listening_on_address server))
  in
  let client_connection = Result.ok_exn client_connection in
  let%bind server_connection = Ivar.read server_connection in
  assert (not (Rpc.Connection.is_closed client_connection));
  assert (not (Rpc.Connection.is_closed server_connection));
  (* returning from the handle_transport callback causes the transport to be closed on
     both ends*)
  Ivar.fill server_has_finished_with_connection ();
  let%bind () =
    with_timeout
      ~context:"client connection close finished"
      (Rpc.Connection.close_finished client_connection)
  in
  let%bind () =
    with_timeout
      ~context:"server connection close finished"
      (Rpc.Connection.close_finished server_connection)
  in
  let%bind () = Tcp.Server.close server in
  [%expect {| |}];
  return ()
;;
