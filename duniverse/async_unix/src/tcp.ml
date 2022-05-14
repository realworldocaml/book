open! Core
open! Import
module Unix = Unix_syscalls
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

  let of_host_and_port ?bind_to_address ?bind_to_port ({ Host_and_port.host; port } as hp)
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
  Monitor.try_with
    ~run:`Schedule
    ~rest:`Log
    ~name:"Tcp.close_sock_on_error"
    f
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
      ?time_source
      (where_to_connect : _ Where_to_connect.t)
  =
  let time_source =
    match time_source with
    | Some x -> Time_source.read_only x
    | None -> Time_source.wall_clock ()
  in
  where_to_connect.remote_address ()
  >>= fun address ->
  let timeout =
    Time_source.Event.after time_source (Time_ns.Span.of_span_float_round_nearest timeout)
  in
  let interrupt =
    let timeout =
      Time_source.Event.fired timeout
      >>= function
      | Aborted () -> Deferred.never ()
      | Happened () -> Deferred.unit
    in
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
    | `Ok s ->
      Time_source.Event.abort_if_possible timeout ();
      Ivar.fill result s
    | `Interrupted ->
      don't_wait_for (Unix.close (Socket.fd s));
      let address = Socket.Address.to_string address in
      (match Time_source.Event.abort timeout () with
       | Previously_happened () ->
         raise_s [%sexp "connection attempt timeout", (address : string)]
       | Ok | Previously_aborted () ->
         raise_s [%sexp "connection attempt aborted", (address : string)]))
;;

type 'a with_connect_options =
  ?buffer_age_limit:[ `At_most of Time.Span.t | `Unlimited ]
  -> ?interrupt:unit Deferred.t
  -> ?reader_buffer_size:int
  -> ?writer_buffer_size:int
  -> ?timeout:Time.Span.t
  -> ?time_source:Time_source.t
  -> 'a

let connect
      ?socket
      ?buffer_age_limit
      ?interrupt
      ?reader_buffer_size
      ?writer_buffer_size
      ?timeout
      ?time_source
      where_to_connect
  =
  connect_sock ?socket ?interrupt ?timeout ?time_source where_to_connect
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
    ; choice
        (Monitor.try_with
           ~run:
             `Schedule
           ~rest:`Log
           ~name:"Tcp.collect_errors"
           f)
        Fn.id
    ]
;;

let close_connection_via_reader_and_writer r w =
  let force_close_event = Clock.Event.after (sec 30.) in
  let force_close =
    Clock.Event.fired force_close_event
    >>= function
    | Aborted () -> Deferred.never ()
    | Happened () -> Deferred.unit
  in
  Writer.close w ~force_close
  >>= fun () ->
  Clock.Event.abort_if_possible force_close_event ();
  Reader.close r
;;

let with_connection
      ?buffer_age_limit
      ?interrupt
      ?reader_buffer_size
      ?writer_buffer_size
      ?timeout
      ?time_source
      where_to_connect
      f
  =
  connect_sock ?interrupt ?timeout ?time_source where_to_connect
  >>= fun socket ->
  let r, w =
    reader_writer_of_sock ?buffer_age_limit ?reader_buffer_size ?writer_buffer_size socket
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

  let is_inet_witness t = Socket.Family.is_inet_witness (Socket.Type.family t.socket_type)
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

  let binding_on_port_chosen_by_os t =
    match t.address with
    | `Inet _ as inet -> Socket.Address.Inet.port inet = 0
    | `Unix _ -> false
  ;;

  let max_retries_upon_addr_in_use t =
    match binding_on_port_chosen_by_os t with
    | true -> 10
    | false -> 0
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

  module Max_connections = struct
    type t =
      { limit : int
      ; time_source : Time_source.t
      ; listening_on : Info.t
      ; mutable last_logged : Time_ns.t option
      }

    let sexp_of_t t = [%sexp_of: int] t.limit

    let create ~limit ~time_source ~listening_on =
      { limit; time_source; listening_on; last_logged = None }
    ;;

    (* We make sure not to be too spammy with logs. This number was chosen pretty
       arbitrarily. *)
    let log_threshold = Time_ns.Span.of_min 1.

    let log_at_limit t ~now =
      Log.Global.error_s
        [%message
          "At limit of Tcp server [max_connections]. New connections will not be \
           accepted until an existing connection is closed."
            ~limit:(t.limit : int)
            ~listening_on:(t.listening_on : Info.t)];
      t.last_logged <- Some now
    ;;

    let maybe_log_at_limit t =
      let now = Time_source.now t.time_source in
      match t.last_logged with
      | None -> log_at_limit t ~now
      | Some last_logged ->
        if Time_ns.Span.( > ) (Time_ns.diff now last_logged) log_threshold
        then log_at_limit t ~now
    ;;
  end

  type ('address, 'listening_on) t =
    { socket : ([ `Passive ], 'address) Socket.t
    ; listening_on : 'listening_on
    ; on_handler_error : [ `Raise | `Ignore | `Call of 'address -> exn -> unit ]
    ; handle_client :
        'address -> ([ `Active ], 'address) Socket.t -> (unit, exn) Result.t Deferred.t
    ; max_connections : Max_connections.t
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
        ~max_connections:
          (check (fun (max_connections : Max_connections.t) ->
             assert (max_connections.limit >= 1)))
        ~max_accepts_per_batch:
          (check (fun max_accepts_per_batch -> assert (max_accepts_per_batch >= 1)))
        ~connections:
          (check (fun connections ->
             Bag.invariant (Connection.invariant ignore) connections;
             let num_connections = num_connections t in
             assert (num_connections >= 0);
             assert (num_connections <= t.max_connections.limit)))
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
    let available_slots = t.max_connections.limit - num_connections t in
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
          List.iter conns ~f:(fun (sock, _) -> don't_wait_for (Fd.close (Socket.fd sock)))
        else
          (* We first [handle_client] on all the connections, which increases
             [num_connections], and then call [maybe_accept] to try to accept more
             clients, which respects the just-increased [num_connections]. *)
          List.iter conns ~f:(fun (sock, addr) -> handle_client t sock addr);
        maybe_accept t)
    else if (not (is_closed t)) && available_slots = 0
    then Max_connections.maybe_log_at_limit t.max_connections

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
        ?(drop_incoming_connections = false)
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
      ; drop_incoming_connections
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

  let get_max_connections_limit max_connections =
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

  module Socket_creator : sig
    type 'a t constraint 'a = [< Socket.Address.t ]

    val create
      :  ([ `Unconnected ], 'addr) Socket.t option
      -> ('addr, _) Where_to_listen.t
      -> 'addr t

    val bind_and_listen_maybe_retry
      :  'addr t
      -> f:
           (([ `Unconnected ], 'addr) Socket.t
            -> reuseaddr:bool
            -> ([ `Passive ], 'addr) Socket.t Deferred.t)
      -> ([ `Passive ], 'addr) Socket.t Deferred.t

    val bind_and_listen_maybe_retry'
      :  'addr t
      -> f:
           (([ `Unconnected ], 'addr) Socket.t
            -> reuseaddr:bool
            -> ([ `Passive ], 'addr) Socket.t)
      -> ([ `Passive ], 'addr) Socket.t
  end = struct
    type 'addr t =
      { create_socket : unit -> ([ `Unconnected ], 'addr) Socket.t
      ; should_set_reuseaddr : bool
      ; retries_upon_addr_in_use : int
      }

    let create maybe_socket where_to_listen =
      match maybe_socket with
      | Some socket ->
        { create_socket = Fn.const socket
        ; should_set_reuseaddr = false
        ; retries_upon_addr_in_use = 0
        }
      | None ->
        { create_socket =
            (fun () -> Socket.create where_to_listen.Where_to_listen.socket_type)
        ; should_set_reuseaddr = true
        ; retries_upon_addr_in_use =
            Where_to_listen.max_retries_upon_addr_in_use where_to_listen
        }
    ;;

    let handle_exn t socket exn ~retries_attempted_upon_addr_in_use =
      don't_wait_for (Unix.close (Socket.fd socket));
      match t.retries_upon_addr_in_use > retries_attempted_upon_addr_in_use, exn with
      | true, Unix.Unix_error (EADDRINUSE, _, _) -> `Please_retry
      | _, _ ->
        if retries_attempted_upon_addr_in_use > 0
        then
          raise_s
            [%message
              "Failed to bind and listen to socket."
                (exn : Exn.t)
                (retries_attempted_upon_addr_in_use : int)]
        else raise exn
    ;;

    let rec aux_bind_and_listen_maybe_retry t ~retries_attempted_upon_addr_in_use ~f =
      let socket = t.create_socket () in
      match%bind
        Monitor.try_with ~extract_exn:true (fun () ->
          f socket ~reuseaddr:t.should_set_reuseaddr)
      with
      | Ok v -> return v
      | Error exn ->
        let `Please_retry = handle_exn t socket exn ~retries_attempted_upon_addr_in_use in
        aux_bind_and_listen_maybe_retry
          t
          ~retries_attempted_upon_addr_in_use:(retries_attempted_upon_addr_in_use + 1)
          ~f
    ;;

    let rec aux_bind_and_listen_maybe_retry' t ~retries_attempted_upon_addr_in_use ~f =
      let socket = t.create_socket () in
      try f socket ~reuseaddr:t.should_set_reuseaddr with
      | exn ->
        let `Please_retry = handle_exn t socket exn ~retries_attempted_upon_addr_in_use in
        aux_bind_and_listen_maybe_retry'
          t
          ~retries_attempted_upon_addr_in_use:(retries_attempted_upon_addr_in_use + 1)
          ~f
    ;;

    let bind_and_listen_maybe_retry =
      aux_bind_and_listen_maybe_retry ~retries_attempted_upon_addr_in_use:0
    ;;

    let bind_and_listen_maybe_retry' =
      aux_bind_and_listen_maybe_retry' ~retries_attempted_upon_addr_in_use:0
    ;;
  end

  let create_sock_non_inet_internal
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?drop_incoming_connections
        ?socket
        ?time_source
        ~on_handler_error
        (where_to_listen : _ Where_to_listen.t)
        handle_client
    =
    let time_source =
      match time_source with
      | Some x -> Time_source.read_only x
      | None -> Time_source.wall_clock ()
    in
    let%map socket =
      let socket_creator = Socket_creator.create socket where_to_listen in
      Socket_creator.bind_and_listen_maybe_retry
        socket_creator
        ~f:(fun socket ~reuseaddr ->
          Socket.bind ~reuseaddr socket where_to_listen.address >>| Socket.listen ?backlog)
    in
    let max_connections =
      Max_connections.create
        ~limit:(get_max_connections_limit max_connections)
        ~time_source
        (* We must call [Fd.info] on the socket's fd after [Socket.bind] is called,
           otherwise the [Info.t] won't have been set yet. *)
        ~listening_on:(Fd.info (Socket.fd socket))
    in
    create_from_socket
      ~max_connections
      ?max_accepts_per_batch
      ?drop_incoming_connections
      ~on_handler_error
      where_to_listen
      handle_client
      socket
  ;;

  let create_sock_inet_internal
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?drop_incoming_connections
        ?(socket : ([ `Unconnected ], Socket.Address.Inet.t) Socket.t option)
        ?time_source
        ~on_handler_error
        (where_to_listen : (Socket.Address.Inet.t, 'listening_on) Where_to_listen.t)
        handle_client
    =
    let time_source =
      match time_source with
      | Some x -> Time_source.read_only x
      | None -> Time_source.wall_clock ()
    in
    let socket =
      let socket_creator = Socket_creator.create socket where_to_listen in
      Socket_creator.bind_and_listen_maybe_retry'
        socket_creator
        ~f:(fun socket ~reuseaddr ->
          Socket.bind_inet ~reuseaddr socket where_to_listen.address
          |> Socket.listen ?backlog)
    in
    let max_connections =
      Max_connections.create
        ~limit:(get_max_connections_limit max_connections)
        ~time_source
        (* We must call [Fd.info] on the socket's fd after [Socket.bind_inet] is called,
           otherwise the [Info.t] won't have been set yet. *)
        ~listening_on:(Fd.info (Socket.fd socket))
    in
    create_from_socket
      ~max_connections
      ?max_accepts_per_batch
      ?drop_incoming_connections
      ~on_handler_error
      where_to_listen
      handle_client
      socket
  ;;

  type ('address, 'listening_on, 'time_source_access) create_sock_async =
    ?max_connections:int
    -> ?max_accepts_per_batch:int
    -> ?backlog:int
    -> ?drop_incoming_connections:bool
    -> ?socket:([ `Unconnected ], 'address) Socket.t
    -> ?time_source:([> read ] as 'time_source_access) Time_source.T1.t
    -> on_handler_error:[ `Call of 'address -> exn -> unit | `Ignore | `Raise ]
    -> ('address, 'listening_on) Where_to_listen.t
    -> ('address -> ([ `Active ], 'address) Socket.t -> (unit, exn) Result.t Deferred.t)
    -> ('address, 'listening_on) t Deferred.t

  let create_sock_inet_internal_async : ('address, _, _) create_sock_async =
    fun ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?drop_incoming_connections
      ?socket
      ?time_source
      ~on_handler_error
      where_to_listen
      handle_client ->
      return
        (create_sock_inet_internal
           ?max_connections
           ?max_accepts_per_batch
           ?backlog
           ?drop_incoming_connections
           ?socket
           ?time_source
           ~on_handler_error
           where_to_listen
           handle_client)
  ;;

  type ('address, 'listening_on, 't) create_sock_async_no_constraint =
    | T :
        ('address, 'listening_on, 't) create_sock_async
        -> ('address, 'listening_on, 't) create_sock_async_no_constraint

  type 'address is_address_type = T : [< Socket.Address.t ] is_address_type

  let create_sock_internal_type_hackery
    : type address listening_on.
      is_address:address is_address_type
      -> is_inet:(address, [ `Inet of Unix.Inet_addr.t * int ]) Type_equal.t option
      -> (address, listening_on, _) create_sock_async_no_constraint
    =
    fun ~is_address ~is_inet ->
    match is_inet with
    | Some T -> T create_sock_inet_internal_async
    | None ->
      let T = is_address in
      T create_sock_non_inet_internal
  ;;

  let create_sock_internal : (_, _, [> read ]) create_sock_async =
    fun ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?drop_incoming_connections
      ?socket
      ?time_source
      ~on_handler_error
      where_to_listen
      handle_client ->
      let (T f) =
        create_sock_internal_type_hackery
          ~is_inet:(Where_to_listen.is_inet_witness where_to_listen)
          ~is_address:T
      in
      f
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?drop_incoming_connections
        ?socket
        ?time_source
        ~on_handler_error
        where_to_listen
        handle_client
  ;;

  let create_sock
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?drop_incoming_connections
        ?socket
        ?time_source
        ~on_handler_error
        (where_to_listen : ('address, 'listening_on) Where_to_listen.t)
        (handle_client :
           ([< Socket.Address.t ] as 'b) -> ([ `Active ], 'b) Socket.t -> unit Deferred.t)
    =
    create_sock_internal
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?drop_incoming_connections
      ~on_handler_error
      ?socket
      ?time_source
      where_to_listen
      (fun client_address client_socket ->
         Monitor.try_with
           ~run:
             `Schedule
           ~rest:`Log
           ~name:"Tcp.Server.create_sock"
           (fun () -> handle_client client_address client_socket))
  ;;

  let create_sock_inet
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?drop_incoming_connections
        ?socket
        ?time_source
        ~on_handler_error
        where_to_listen
        handle_client
    =
    create_sock_inet_internal
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?drop_incoming_connections
      ?socket
      ?time_source
      ~on_handler_error
      where_to_listen
      (fun client_address client_socket ->
         Monitor.try_with
           ~run:
             `Schedule
           ~rest:`Log
           ~name:"Tcp.Server.create_sock_inet"
           (fun () -> handle_client client_address client_socket))
  ;;

  let create_internal
        ~create_sock
        ?buffer_age_limit
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?drop_incoming_connections
        ?socket
        ?time_source
        ~on_handler_error
        where_to_listen
        handle_client
    =
    create_sock
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?drop_incoming_connections
      ?socket
      ?time_source
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
        ?drop_incoming_connections
        ?socket
        ?time_source
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
      ?drop_incoming_connections
      ?socket
      ?time_source
      ~on_handler_error
      where_to_listen
      handle_client
  ;;

  let create
        ?buffer_age_limit
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?drop_incoming_connections
        ?socket
        ?time_source
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
      ?drop_incoming_connections
      ?socket
      ?time_source
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
