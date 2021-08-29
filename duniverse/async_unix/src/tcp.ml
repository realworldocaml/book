open! Core
open! Import
module Unix = Unix_syscalls
module Host = Unix.Host
module Socket = Unix.Socket

module Where_to_connect = struct
  type 'addr t =
    { socket_type : 'addr Socket.Type.t
    ; remote_address : unit -> 'addr Deferred.t
    ; local_address : 'addr option
    ; info : Sexp.t
    }

  let sexp_of_t _ { info; _ } = info

  type inet = Socket.Address.Inet.t t [@@deriving sexp_of]
  type unix = Socket.Address.Unix.t t [@@deriving sexp_of]

  let remote_address t = t.remote_address ()

  let create_local_address ~bind_to_address ~bind_to_port =
    let port = Option.value bind_to_port ~default:0 in
    match bind_to_address with
    | None -> Socket.Address.Inet.create_bind_any ~port
    | Some inet_addr -> Socket.Address.Inet.create ~port inet_addr
  ;;

  let of_host_and_port
        ?bind_to_address
        ?bind_to_port
        ({ Host_and_port.host; port } as hp)
    =
    { socket_type = Socket.Type.tcp
    ; remote_address =
        (fun () ->
           Unix.Inet_addr.of_string_or_getbyname host
           >>| fun inet_addr -> Socket.Address.Inet.create inet_addr ~port)
    ; local_address = Some (create_local_address ~bind_to_address ~bind_to_port)
    ; info = [%sexp (hp : Host_and_port.t)]
    }
  ;;

  let of_file file =
    { socket_type = Socket.Type.unix
    ; remote_address = (fun () -> return (Socket.Address.Unix.create file))
    ; local_address = None
    ; info = [%sexp_of: string] file
    }
  ;;

  let of_inet_address ?bind_to_address ?bind_to_port address =
    { socket_type = Socket.Type.tcp
    ; remote_address = (fun () -> return address)
    ; local_address = Some (create_local_address ~bind_to_address ~bind_to_port)
    ; info = [%sexp_of: Socket.Address.Inet.t] address
    }
  ;;

  let of_unix_address address =
    { socket_type = Socket.Type.unix
    ; remote_address = (fun () -> return address)
    ; local_address = None
    ; info = [%sexp_of: Socket.Address.Unix.t] address
    }
  ;;
end

let close_sock_on_error s f =
  try_with ~name:"Tcp.close_sock_on_error" f
  >>| function
  | Ok v -> v
  | Error e ->
    (* [close] may fail, but we don't really care, since it will fail
       asynchronously.  The error we really care about is [e], and the
       [raise_error] will cause the current monitor to see that. *)
    don't_wait_for (Unix.close (Socket.fd s));
    raise e
;;

let reader_writer_of_sock ?buffer_age_limit ?reader_buffer_size ?writer_buffer_size s =
  let fd = Socket.fd s in
  ( Reader.create ?buf_len:reader_buffer_size fd
  , Writer.create ?buffer_age_limit ?buf_len:writer_buffer_size fd )
;;

let connect_sock
      ?socket
      ?interrupt
      ?(timeout = sec 10.)
      (where_to_connect : _ Where_to_connect.t)
  =
  where_to_connect.remote_address ()
  >>= fun address ->
  let timeout = Clock.after timeout in
  let interrupt =
    match interrupt with
    | None -> timeout
    | Some interrupt -> Deferred.any [ interrupt; timeout ]
  in
  let connect_interruptible s = Socket.connect_interruptible s address ~interrupt in
  Deferred.create (fun result ->
    let s =
      match socket with
      | Some s -> s
      | None -> Socket.create where_to_connect.socket_type
    in
    close_sock_on_error s (fun () ->
      match where_to_connect.local_address with
      | None -> connect_interruptible s
      | Some local_interface ->
        Socket.bind s local_interface >>= fun s -> connect_interruptible s)
    >>> function
    | `Ok s -> Ivar.fill result s
    | `Interrupted ->
      don't_wait_for (Unix.close (Socket.fd s));
      let address = Socket.Address.to_string address in
      if Option.is_some (Deferred.peek timeout)
      then raise_s [%sexp "connection attempt timeout", (address : string)]
      else raise_s [%sexp "connection attempt aborted", (address : string)])
;;

type 'a with_connect_options =
  ?buffer_age_limit:[ `At_most of Time.Span.t | `Unlimited ]
  -> ?interrupt:unit Deferred.t
  -> ?reader_buffer_size:int
  -> ?writer_buffer_size:int
  -> ?timeout:Time.Span.t
  -> 'a

let connect
      ?socket
      ?buffer_age_limit
      ?interrupt
      ?reader_buffer_size
      ?writer_buffer_size
      ?timeout
      where_to_connect
  =
  connect_sock ?socket ?interrupt ?timeout where_to_connect
  >>| fun s ->
  let r, w =
    reader_writer_of_sock ?buffer_age_limit ?reader_buffer_size ?writer_buffer_size s
  in
  s, r, w
;;

let collect_errors writer f =
  let monitor = Writer.monitor writer in
  ignore (Monitor.detach_and_get_error_stream monitor : _ Stream.t);
  (* don't propagate errors up, we handle them here *)
  choose
    [ choice (Monitor.get_next_error monitor) (fun e -> Error e)
    ; choice (try_with ~name:"Tcp.collect_errors" f) Fn.id
    ]
;;

let close_connection_via_reader_and_writer r w =
  Writer.close w ~force_close:(Clock.after (sec 30.)) >>= fun () -> Reader.close r
;;

let with_connection
      ?buffer_age_limit
      ?interrupt
      ?reader_buffer_size
      ?writer_buffer_size
      ?timeout
      where_to_connect
      f
  =
  connect_sock ?interrupt ?timeout where_to_connect
  >>= fun socket ->
  let r, w =
    reader_writer_of_sock
      ?buffer_age_limit
      ?reader_buffer_size
      ?writer_buffer_size
      socket
  in
  let res = collect_errors w (fun () -> f socket r w) in
  Deferred.any
    [ (res >>| fun (_ : ('a, exn) Result.t) -> ())
    ; Reader.close_finished r
    ; Writer.close_finished w
    ]
  >>= fun () ->
  close_connection_via_reader_and_writer r w
  >>= fun () ->
  res
  >>| function
  | Ok v -> v
  | Error e -> raise e
;;

module Bind_to_address = struct
  type t =
    | Address of Unix.Inet_addr.t
    | All_addresses
    | Localhost
  [@@deriving sexp_of]
end

module Bind_to_port = struct
  type t =
    | On_port of int
    | On_port_chosen_by_os
  [@@deriving sexp_of]
end

module Where_to_listen = struct
  type ('address, 'listening_on) t =
    { socket_type : 'address Socket.Type.t
    ; address : 'address
    ; listening_on : ('address -> 'listening_on[@sexp.opaque])
    }
  [@@deriving sexp_of, fields]

  type inet = (Socket.Address.Inet.t, int) t [@@deriving sexp_of]
  type unix = (Socket.Address.Unix.t, string) t [@@deriving sexp_of]

  let create ~socket_type ~address ~listening_on = { socket_type; address; listening_on }

  let bind_to (bind_to_address : Bind_to_address.t) (bind_to_port : Bind_to_port.t) =
    let port =
      match bind_to_port with
      | On_port port -> port
      | On_port_chosen_by_os -> 0
    in
    let address =
      match bind_to_address with
      | All_addresses -> Socket.Address.Inet.create_bind_any ~port
      | Address addr -> Socket.Address.Inet.create addr ~port
      | Localhost -> Socket.Address.Inet.create Unix.Inet_addr.localhost ~port
    in
    { socket_type = Socket.Type.tcp
    ; address
    ; listening_on =
        (function
          | `Inet (_, port) -> port)
    }
  ;;

  let of_port port = bind_to All_addresses (On_port port)
  let of_port_chosen_by_os = bind_to All_addresses On_port_chosen_by_os

  let of_file path =
    { socket_type = Socket.Type.unix
    ; address = Socket.Address.Unix.create path
    ; listening_on = (fun _ -> path)
    }
  ;;
end

module Server = struct
  module Connection = struct
    type 'address t =
      { client_socket : ([ `Active ], 'address) Socket.t
      ; client_address : 'address
      }
    [@@deriving fields, sexp_of]

    let invariant invariant_address t =
      Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
        let check f = Invariant.check_field t f in
        Fields.iter ~client_socket:ignore ~client_address:(check invariant_address))
    ;;

    let create ~client_socket ~client_address = { client_socket; client_address }
    let close t = Fd.close (Socket.fd t.client_socket)
  end

  type ('address, 'listening_on) t =
    { socket : ([ `Passive ], 'address) Socket.t
    ; listening_on : 'listening_on
    ; on_handler_error : [ `Raise | `Ignore | `Call of 'address -> exn -> unit ]
    ; handle_client :
        'address -> ([ `Active ], 'address) Socket.t -> (unit, exn) Result.t Deferred.t
    ; max_connections : int
    ; max_accepts_per_batch : int
    ; connections : 'address Connection.t Bag.t
    ; mutable accept_is_pending : bool
    ; mutable drop_incoming_connections : bool
    ; close_finished_and_handlers_determined : unit Ivar.t
    }
  [@@deriving fields, sexp_of]

  let num_connections t = Bag.length t.connections
  let listening_socket = socket

  type inet = (Socket.Address.Inet.t, int) t [@@deriving sexp_of]
  type unix = (Socket.Address.Unix.t, string) t [@@deriving sexp_of]

  let listening_on_address (t : (_, _) t) = Socket.getsockname t.socket

  let invariant t : unit =
    try
      let check f field = f (Field.get field t) in
      Fields.iter
        ~socket:ignore
        ~listening_on:ignore
        ~on_handler_error:ignore
        ~handle_client:ignore
        ~max_connections:(check (fun max_connections -> assert (max_connections >= 1)))
        ~max_accepts_per_batch:
          (check (fun max_accepts_per_batch -> assert (max_accepts_per_batch >= 1)))
        ~connections:
          (check (fun connections ->
             Bag.invariant (Connection.invariant ignore) connections;
             let num_connections = num_connections t in
             assert (num_connections >= 0);
             assert (num_connections <= t.max_connections)))
        ~accept_is_pending:ignore
        ~drop_incoming_connections:ignore
        ~close_finished_and_handlers_determined:ignore
    with
    | exn ->
      failwiths ~here:[%here] "invariant failed" (exn, t) [%sexp_of: exn * (_, _) t]
  ;;

  let fd t = Socket.fd t.socket
  let is_closed t = Fd.is_closed (fd t)
  let close_finished t = Fd.close_finished (fd t)

  let close_finished_and_handlers_determined t =
    Ivar.read t.close_finished_and_handlers_determined
  ;;

  let close ?(close_existing_connections = false) t =
    let fd_closed = Fd.close (fd t) in
    if not close_existing_connections
    then fd_closed
    else
      (* Connections are removed from the bag by the [maybe_accept] below, as the fds are
         closed. *)
      Deferred.all_unit
        (fd_closed :: List.map (Bag.to_list t.connections) ~f:Connection.close)
  ;;

  (* [maybe_accept] is a bit tricky, but the idea is to avoid calling [accept] until we
     have an available slot (determined by [num_connections < max_connections]). *)
  let rec maybe_accept t =
    let available_slots = t.max_connections - num_connections t in
    if (not (is_closed t)) && available_slots > 0 && not t.accept_is_pending
    then (
      t.accept_is_pending <- true;
      Socket.accept_at_most ~limit:(min t.max_accepts_per_batch available_slots) t.socket
      >>> fun accept_result ->
      t.accept_is_pending <- false;
      match accept_result with
      | `Socket_closed -> ()
      | `Ok conns ->
        (* It is possible that someone called [close t] after the [accept] returned but
           before we got here.  In that case, we just close the clients. *)
        if is_closed t || t.drop_incoming_connections
        then
          List.iter conns ~f:(fun (sock, _) ->
            don't_wait_for (Fd.close (Socket.fd sock)))
        else (
          (* We first [handle_client] on all the connections, which increases
             [num_connections], and then call [maybe_accept] to try to accept more
             clients, which respects the just-increased [num_connections]. *)
          List.iter conns ~f:(fun (sock, addr) -> handle_client t sock addr);
          maybe_accept t))

  and handle_client t client_socket client_address =
    let connection = Connection.create ~client_socket ~client_address in
    let connections_elt = Bag.add t.connections connection in
    t.handle_client client_address client_socket
    >>> fun res ->
    Connection.close connection
    >>> fun () ->
    Bag.remove t.connections connections_elt;
    if Deferred.is_determined (close_finished t) && num_connections t = 0
    then Ivar.fill_if_empty t.close_finished_and_handlers_determined ();
    (match res with
     | Ok () -> ()
     | Error e ->
       (try
          match t.on_handler_error with
          | `Ignore -> ()
          | `Raise -> raise e
          | `Call f -> f client_address e
        with
        | e ->
          don't_wait_for (close t);
          raise e));
    maybe_accept t
  ;;

  let create_from_socket
        ~max_connections
        ?(max_accepts_per_batch = 1)
        ~on_handler_error
        (where_to_listen : _ Where_to_listen.t)
        handle_client
        socket
    =
    let t =
      { socket
      ; listening_on = where_to_listen.listening_on (Socket.getsockname socket)
      ; on_handler_error
      ; handle_client
      ; max_connections
      ; max_accepts_per_batch
      ; connections = Bag.create ()
      ; accept_is_pending = false
      ; drop_incoming_connections = false
      ; close_finished_and_handlers_determined = Ivar.create ()
      }
    in
    (close_finished t
     >>> fun () ->
     if num_connections t = 0
     then Ivar.fill_if_empty t.close_finished_and_handlers_determined ());
    maybe_accept t;
    t
  ;;

  let get_max_connections max_connections =
    match max_connections with
    | None -> 10_000
    | Some max_connections ->
      if max_connections <= 0
      then
        failwiths
          ~here:[%here]
          "Tcp.Server.creater got negative [max_connections]"
          max_connections
          sexp_of_int;
      max_connections
  ;;

  let create_sock_internal
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?socket
        ~on_handler_error
        (where_to_listen : _ Where_to_listen.t)
        handle_client
    =
    let max_connections = get_max_connections max_connections in
    let socket, should_set_reuseaddr =
      match socket with
      | Some socket -> socket, false
      | None -> Socket.create where_to_listen.socket_type, true
    in
    close_sock_on_error socket (fun () ->
      Socket.bind ~reuseaddr:should_set_reuseaddr socket where_to_listen.address
      >>| Socket.listen ?backlog)
    >>| create_from_socket
          ~max_connections
          ?max_accepts_per_batch
          ~on_handler_error
          where_to_listen
          handle_client
  ;;

  let create_sock
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?socket
        ~on_handler_error
        (where_to_listen : ('address, 'listening_on) Where_to_listen.t)
        (handle_client :
           ([< Socket.Address.t ] as 'b) -> ([ `Active ], 'b) Socket.t -> unit Deferred.t)
    =
    create_sock_internal
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ~on_handler_error
      ?socket
      where_to_listen
      (fun client_address client_socket ->
         try_with ~name:"Tcp.Server.create_sock" (fun () ->
           handle_client client_address client_socket))
  ;;

  let create_sock_inet_internal
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?(socket : ([ `Unconnected ], Socket.Address.Inet.t) Socket.t option)
        ~on_handler_error
        (where_to_listen : Where_to_listen.inet)
        handle_client
    =
    let max_connections = get_max_connections max_connections in
    let socket, should_set_reuseaddr =
      match socket with
      | Some socket -> socket, false
      | None -> Socket.create where_to_listen.socket_type, true
    in
    let socket =
      try
        let socket =
          Socket.bind_inet ~reuseaddr:should_set_reuseaddr socket where_to_listen.address
        in
        Socket.listen ?backlog socket
      with
      | exn ->
        don't_wait_for (Unix.close (Socket.fd socket));
        raise exn
    in
    create_from_socket
      ~max_connections
      ?max_accepts_per_batch
      ~on_handler_error
      where_to_listen
      handle_client
      socket
  ;;

  let create_sock_inet
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?socket
        ~on_handler_error
        where_to_listen
        handle_client
    =
    create_sock_inet_internal
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      ~on_handler_error
      where_to_listen
      (fun client_address client_socket ->
         try_with ~name:"Tcp.Server.create_sock_inet" (fun () ->
           handle_client client_address client_socket))
  ;;

  let create_internal
        ~create_sock
        ?buffer_age_limit
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?socket
        ~on_handler_error
        where_to_listen
        handle_client
    =
    create_sock
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      ~on_handler_error
      where_to_listen
      (fun client_address client_socket ->
         let r, w = reader_writer_of_sock ?buffer_age_limit client_socket in
         Writer.set_raise_when_consumer_leaves w false;
         Deferred.any
           [ collect_errors w (fun () -> handle_client client_address r w)
           ; Writer.consumer_left w |> Deferred.ok
           ]
         >>= fun res -> close_connection_via_reader_and_writer r w >>| fun () -> res)
  ;;

  let create_inet
        ?buffer_age_limit
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?socket
        ~on_handler_error
        where_to_listen
        handle_client
    =
    create_internal
      ~create_sock:create_sock_inet_internal
      ?buffer_age_limit
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      ~on_handler_error
      where_to_listen
      handle_client
  ;;

  let create
        ?buffer_age_limit
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?socket
        ~on_handler_error
        where_to_listen
        handle_client
    =
    create_internal
      ~create_sock:create_sock_internal
      ?buffer_age_limit
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      ~on_handler_error
      where_to_listen
      handle_client
  ;;

  module Private = struct
    let fd = fd
  end
end

module Private = struct
  let close_connection_via_reader_and_writer = close_connection_via_reader_and_writer
end
