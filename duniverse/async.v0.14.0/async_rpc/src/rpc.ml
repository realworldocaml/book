open Core
open Import
module Transport = Rpc_transport
module Low_latency_transport = Rpc_transport_low_latency
module Any = Rpc_kernel.Any
module Description = Rpc_kernel.Description
module Implementation = Rpc_kernel.Implementation
module Implementations = Rpc_kernel.Implementations
module One_way = Rpc_kernel.One_way
module Pipe_rpc = Rpc_kernel.Pipe_rpc
module Rpc = Rpc_kernel.Rpc
module State_rpc = Rpc_kernel.State_rpc
module Pipe_close_reason = Rpc_kernel.Pipe_close_reason

module Connection = struct
  include Rpc_kernel.Connection

  (* unfortunately, copied from reader0.ml *)
  let default_max_message_size = 100 * 1024 * 1024

  let create
        ?implementations
        ~connection_state
        ?(max_message_size = default_max_message_size)
        ?handshake_timeout
        ?heartbeat_config
        ?description
        reader
        writer
    =
    create
      ?implementations
      ~connection_state
      ?handshake_timeout:
        (Option.map handshake_timeout ~f:Time_ns.Span.of_span_float_round_nearest)
      ?heartbeat_config
      ?description
      (Transport.of_reader_writer reader writer ~max_message_size)
  ;;

  let contains_magic_prefix reader =
    Deferred.Or_error.try_with (fun () ->
      Reader.peek_bin_prot reader contains_magic_prefix)
    >>| function
    | Error _ | Ok `Eof -> false
    | Ok (`Ok b) -> b
  ;;

  let with_close
        ?implementations
        ?(max_message_size = default_max_message_size)
        ?handshake_timeout
        ?heartbeat_config
        ~connection_state
        reader
        writer
        ~dispatch_queries
        ~on_handshake_error
    =
    with_close
      ?implementations
      ?handshake_timeout:
        (Option.map handshake_timeout ~f:Time_ns.Span.of_span_float_round_nearest)
      ?heartbeat_config
      ~connection_state
      (Transport.of_reader_writer reader writer ~max_message_size)
      ~dispatch_queries
      ~on_handshake_error
  ;;

  let server_with_close
        ?(max_message_size = default_max_message_size)
        ?handshake_timeout
        ?heartbeat_config
        reader
        writer
        ~implementations
        ~connection_state
        ~on_handshake_error
    =
    server_with_close
      ?handshake_timeout:
        (Option.map handshake_timeout ~f:Time_ns.Span.of_span_float_round_nearest)
      ?heartbeat_config
      (Transport.of_reader_writer reader writer ~max_message_size)
      ~implementations
      ~connection_state
      ~on_handshake_error
  ;;

  let collect_errors (transport : Transport.t) ~f =
    let monitor = Transport.Writer.monitor transport.writer in
    (* don't propagate errors up, we handle them here *)
    ignore (Monitor.detach_and_get_error_stream monitor);
    choose
      [ choice (Monitor.get_next_error monitor) (fun e -> Error e)
      ; choice (try_with ~name:"Rpc.Connection.collect_errors" f) Fn.id
      ]
  ;;

  type transport_maker = Fd.t -> max_message_size:int -> Transport.t

  type on_handshake_error =
    [ `Raise
    | `Ignore
    | `Call of Exn.t -> unit
    ]

  let default_transport_maker fd ~max_message_size = Transport.of_fd fd ~max_message_size

  let serve_with_transport
        ~handshake_timeout
        ~heartbeat_config
        ~implementations
        ~description
        ~connection_state
        ~on_handshake_error
        transport
    =
    collect_errors transport ~f:(fun () ->
      Rpc_kernel.Connection.create
        ?handshake_timeout:
          (Option.map handshake_timeout ~f:Time_ns.Span.of_span_float_round_nearest)
        ?heartbeat_config
        ~implementations
        ~description
        ~connection_state
        transport
      >>= function
      | Ok t -> close_finished t
      | Error handshake_error ->
        (match on_handshake_error with
         | `Call f -> f handshake_error
         | `Raise -> raise handshake_error
         | `Ignore -> ());
        Deferred.unit)
    >>= fun res -> Transport.close transport >>| fun () -> Result.ok_exn res
  ;;

  let make_serve_func
        tcp_creator
        ~implementations
        ~initial_connection_state
        ~where_to_listen
        ?max_connections
        ?backlog
        ?(max_message_size = default_max_message_size)
        ?(make_transport = default_transport_maker)
        ?handshake_timeout
        ?heartbeat_config
        ?(auth = fun _ -> true)
        ?(on_handshake_error = `Ignore)
        ?(on_handler_error = `Ignore)
        ()
    =
    tcp_creator
      ?max_connections
      ?max_accepts_per_batch:None
      ?backlog
      ?socket:None
      ~on_handler_error
      where_to_listen
      (fun inet socket ->
         match (Socket.getpeername socket :> Socket.Address.t) with
         | exception _could_raise_if_the_socket_disconnects_quickly -> Deferred.unit
         | client_addr ->
           if not (auth inet)
           then Deferred.unit
           else (
             let description =
               let server_addr = (Socket.getsockname socket :> Socket.Address.t) in
               Info.create_s
                 [%message
                   "TCP server"
                     (server_addr : Socket.Address.t)
                     (client_addr : Socket.Address.t)]
             in
             let connection_state = initial_connection_state inet in
             serve_with_transport
               ~handshake_timeout
               ~heartbeat_config
               ~implementations
               ~description
               ~connection_state
               ~on_handshake_error
               (make_transport ~max_message_size (Socket.fd socket))))
  ;;

  let serve ~implementations ~initial_connection_state ~where_to_listen =
    make_serve_func
      Tcp.Server.create_sock
      ~implementations
      ~initial_connection_state
      ~where_to_listen
  ;;

  let serve_inet ~implementations ~initial_connection_state ~where_to_listen =
    make_serve_func
      Tcp.Server.create_sock_inet
      ~implementations
      ~initial_connection_state
      ~where_to_listen
  ;;

  let client'
        ?implementations
        ?(max_message_size = default_max_message_size)
        ?(make_transport = default_transport_maker)
        ?(handshake_timeout =
          Time_ns.Span.to_span_float_round_nearest
            Async_rpc_kernel.Async_rpc_kernel_private.default_handshake_timeout)
        ?heartbeat_config
        ?description
        where_to_connect
    =
    let finish_handshake_by =
      Time_ns.add
        (Time_ns.now ())
        (Time_ns.Span.of_span_float_round_nearest handshake_timeout)
    in
    Monitor.try_with (fun () ->
      Tcp.connect_sock ~timeout:handshake_timeout where_to_connect)
    >>=? fun sock ->
    match Socket.getpeername sock with
    | exception exn_could_be_raised_if_the_socket_is_diconnected_now ->
      Socket.shutdown sock `Both;
      Deferred.Result.fail exn_could_be_raised_if_the_socket_is_diconnected_now
    | sock_peername ->
      let description =
        match description with
        | None ->
          Info.create
            "Client connected via TCP"
            where_to_connect
            [%sexp_of: _ Tcp.Where_to_connect.t]
        | Some desc ->
          Info.tag_arg
            desc
            "via TCP"
            where_to_connect
            [%sexp_of: _ Tcp.Where_to_connect.t]
      in
      let handshake_timeout = Time_ns.diff finish_handshake_by (Time_ns.now ()) in
      let transport = make_transport (Socket.fd sock) ~max_message_size in
      (match implementations with
       | None ->
         let { Client_implementations.connection_state; implementations } =
           Client_implementations.null ()
         in
         Rpc_kernel.Connection.create
           transport
           ~handshake_timeout
           ?heartbeat_config
           ~implementations
           ~description
           ~connection_state
       | Some { Client_implementations.connection_state; implementations } ->
         Rpc_kernel.Connection.create
           transport
           ~handshake_timeout
           ?heartbeat_config
           ~implementations
           ~description
           ~connection_state)
      >>= (function
        | Ok t -> return (Ok (sock_peername, t))
        | Error _ as error -> Transport.close transport >>= fun () -> return error)
  ;;

  let client
        ?implementations
        ?max_message_size
        ?make_transport
        ?handshake_timeout
        ?heartbeat_config
        ?description
        where_to_connect
    =
    client'
      ?implementations
      ?max_message_size
      ?make_transport
      ?handshake_timeout
      ?heartbeat_config
      ?description
      where_to_connect
    >>|? snd
  ;;

  let with_client'
        ?implementations
        ?max_message_size
        ?make_transport
        ?handshake_timeout
        ?heartbeat_config
        where_to_connect
        f
    =
    client'
      ?implementations
      ?max_message_size
      ?make_transport
      ?handshake_timeout
      ?heartbeat_config
      where_to_connect
    >>=? fun (remote_server, t) ->
    try_with (fun () -> f ~remote_server t)
    >>= fun result ->
    close t ~reason:(Info.of_string "Rpc.Connection.with_client finished")
    >>| fun () -> result
  ;;

  let with_client
        ?implementations
        ?max_message_size
        ?make_transport
        ?handshake_timeout
        ?heartbeat_config
        where_to_connect
        f
    =
    with_client'
      ?implementations
      ?max_message_size
      ?make_transport
      ?handshake_timeout
      ?heartbeat_config
      where_to_connect
      (fun ~remote_server:_ -> f)
  ;;
end
