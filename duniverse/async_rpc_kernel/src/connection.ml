open Core_kernel
open Async_kernel

module Time_ns = Core_kernel.Core_kernel_private.Time_ns_alternate_sexp

module P = Protocol
module Reader = Transport.Reader
module Writer = Transport.Writer

module Header : sig
  type t [@@deriving bin_type_class]
  val v1 : t
  val negotiate : us:t -> peer:t -> int Or_error.t
end = struct
  include P.Header

  let negotiate = negotiate ~allow_legacy_peer:true

  let v1 = Protocol_version_header.create ~protocol:Rpc ~supported_versions:[1]
end

module Handshake_error = struct
  module T = struct
    type t =
      | Eof
      | Transport_closed
      | Timeout
      | Reading_header_failed of Error.t
      | Negotiation_failed of Error.t
      | Negotiated_unexpected_version of int
    [@@deriving sexp]
  end
  include T
  include Sexpable.To_stringable (T)

  exception Handshake_error of (t * Info.t) [@@deriving sexp]

  let to_exn ~connection_description t = Handshake_error (t, connection_description)
end

module Heartbeat_config = struct
  type t =
    { timeout : Time_ns.Span.t
    ; send_every : Time_ns.Span.t
    }
  [@@deriving sexp, bin_io]

  let create ~timeout ~send_every =
    { timeout ; send_every }

  let default =
    { timeout = Time_ns.Span.of_sec 30.
    ; send_every = Time_ns.Span.of_sec 10.
    }
end

type response_handler
  =  Nat0.t P.Response.t
  -> read_buffer : Bigstring.t
  -> read_buffer_pos_ref : int ref
  -> [ `keep
     | `wait of unit Deferred.t
     | `remove of unit Rpc_result.t
     | `remove_and_wait of unit Deferred.t
     ]

type t =
  { description         : Info.t
  ; heartbeat_config    : Heartbeat_config.t
  ; heartbeat_callbacks : (unit -> unit) list ref
  ; reader              : Reader.t
  ; writer              : Writer.t
  ; open_queries        : (P.Query_id.t, response_handler sexp_opaque) Hashtbl.t
  ; close_started       : Info.t Ivar.t
  ; close_finished      : unit Ivar.t
  (* There's a circular dependency between connections and their implementation instances
     (the latter depends on the connection state, which is given access to the connection
     when it is created). *)
  ; implementations_instance     : Implementations.Instance.t Set_once.t
  }
[@@deriving sexp_of]

let description t = t.description

let is_closed t = Ivar.is_full t.close_started

let writer t = if is_closed t then Error `Closed else Ok t.writer

let bytes_to_write t = Writer.bytes_to_write t.writer

let flushed t = Writer.flushed t.writer

let handle_send_result : t -> 'a Transport.Send_result.t -> 'a = fun t r ->
  match r with
  | Sent x -> x
  | Closed ->
    (* All of the places we call [handle_send_result] check whether [t] is closed (usually
       via the [writer] function above). This checks whether [t.writer] is closed, which
       should not happen unless [t] is closed. *)
    failwiths "RPC connection got closed writer" t sexp_of_t
  | Message_too_big _ ->
    raise_s [%sexp
      "Message cannot be sent",
      { reason     = (r : _ Transport.Send_result.t)
      ; connection = (t : t)
      }
    ]

let dispatch t ~response_handler ~bin_writer_query ~query =
  match writer t with
  | Error `Closed as r -> r
  | Ok writer ->
    Option.iter response_handler ~f:(fun response_handler ->
      Hashtbl.set t.open_queries ~key:query.P.Query.id ~data:response_handler);
    Writer.send_bin_prot writer
      (P.Message.bin_writer_needs_length (Writer_with_length.of_writer bin_writer_query))
      (Query query)
    |> handle_send_result t;
    Ok ()
;;

let make_dispatch_bigstring do_send t ~tag ~version buf ~pos ~len ~response_handler =
  match writer t with
  | Error `Closed -> Error `Closed
  | Ok writer ->
    let id = P.Query_id.create () in
    let header : Nat0.t P.Message.t =
      Query { tag; version; id; data = Nat0.of_int_exn len }
    in
    Option.iter response_handler ~f:(fun response_handler ->
      Hashtbl.set t.open_queries ~key:id ~data:response_handler);
    let result =
      do_send writer P.Message.bin_writer_nat0_t header ~buf ~pos ~len
      |> handle_send_result t
    in
    Ok result
;;

let dispatch_bigstring =
  make_dispatch_bigstring Writer.send_bin_prot_and_bigstring
;;

let schedule_dispatch_bigstring =
  make_dispatch_bigstring Writer.send_bin_prot_and_bigstring_non_copying
;;

let handle_response t (response : _ P.Response.t) ~read_buffer ~read_buffer_pos_ref
  : _ Transport.Handler_result.t =
  match Hashtbl.find t.open_queries response.id with
  | None -> Stop (Error (Rpc_error.Unknown_query_id response.id))
  | Some response_handler ->
    match response_handler response ~read_buffer ~read_buffer_pos_ref with
    | `keep -> Continue
    | `wait wait -> Wait wait
    | `remove_and_wait wait ->
      Hashtbl.remove t.open_queries response.id;
      Wait wait
    | `remove removal_circumstances ->
      Hashtbl.remove t.open_queries response.id;
      begin match removal_circumstances with
      | Ok () -> Continue
      | Error e ->
        match e with
        | Unimplemented_rpc _ -> Continue
        | Bin_io_exn _
        | Connection_closed
        | Write_error _
        | Uncaught_exn _
        | Unknown_query_id _ -> Stop (Error e)
      end
;;

let handle_msg t (msg : _ P.Message.t) ~read_buffer ~read_buffer_pos_ref
  : _ Transport.Handler_result.t =
  match msg with
  | Heartbeat -> Continue
  | Response response ->
    handle_response t response ~read_buffer ~read_buffer_pos_ref
  | Query query ->
    let instance = Set_once.get_exn t.implementations_instance [%here] in
    Implementations.Instance.handle_query instance
      ~query ~read_buffer ~read_buffer_pos_ref
;;

let close_reason t ~on_close =
  let reason = Ivar.read t.close_started in
  match on_close with
  | `started -> reason
  | `finished ->
    Ivar.read t.close_finished
    >>= fun () ->
    reason

let close_finished t = Ivar.read t.close_finished

let add_heartbeat_callback t f =
  t.heartbeat_callbacks := f :: !(t.heartbeat_callbacks)
;;

let close ?(streaming_responses_flush_timeout = Time_ns.Span.of_int_sec 5) ~reason t =
  if not (is_closed t) then begin
    Ivar.fill t.close_started reason;
    begin
      match Set_once.get t.implementations_instance with
      | None          -> Deferred.unit
      | Some instance ->
        let flushed = Implementations.Instance.flush instance in
        if Deferred.is_determined flushed then begin
          Implementations.Instance.stop instance;
          flushed
        end else begin
          Deferred.any_unit
            [ flushed
            ; Clock_ns.after streaming_responses_flush_timeout
            ; Writer.stopped t.writer
            ]
          >>| fun () ->
          Implementations.Instance.stop instance
        end
    end
    >>> fun () ->
    Writer.close t.writer
    >>> fun () ->
    Reader.close t.reader
    >>> fun () ->
    Ivar.fill t.close_finished ();
  end;
  close_finished t;
;;

let on_message t =
  let f buf ~pos ~len:_ : _ Transport.Handler_result.t =
    let pos_ref = ref pos in
    let nat0_msg = P.Message.bin_read_nat0_t buf ~pos_ref in
    match
      handle_msg t nat0_msg ~read_buffer:buf ~read_buffer_pos_ref:pos_ref
    with
    | Continue -> Continue
    | Wait _ as res -> res
    | Stop result ->
      let reason =
        let msg = "Rpc message handling loop stopped" in
        match result with
        | Ok () -> Info.of_string msg
        | Error e ->
          Info.create msg e
            (Rpc_error.sexp_of_t
               ~get_connection_close_reason:(fun () ->
                 [%sexp
                   "Connection.on_message resulted in Connection_closed error. \
                    This is weird."]
               )
            )
      in
      don't_wait_for (close t ~reason);
      Stop reason
  in
  Staged.stage f
;;

let heartbeat t ~last_heartbeat =
  if not (is_closed t) then begin
    let since_last_heartbeat = Time_ns.diff (Time_ns.now ()) last_heartbeat in
    if Time_ns.Span.(>) since_last_heartbeat t.heartbeat_config.timeout then begin
      let reason () =
        sprintf !"No heartbeats received for %{sexp:Time_ns.Span.t}."
          t.heartbeat_config.timeout
      in
      don't_wait_for (close t ~reason:(Info.of_thunk reason));
    end else begin
      Writer.send_bin_prot t.writer P.Message.bin_writer_nat0_t Heartbeat
      |> handle_send_result t
    end
  end

let default_handshake_timeout = Time_ns.Span.of_sec 30.

let cleanup t ~reason exn =
  don't_wait_for (close ~reason t);
  if not (Hashtbl.is_empty t.open_queries)
  then begin
    let error = match exn with
      | Rpc_error.Rpc (error, (_ : Info.t)) -> error
      | exn -> Uncaught_exn (Exn.sexp_of_t exn)
    in
    (* clean up open streaming responses *)
    (* an unfortunate hack; ok because the response handler will have nothing
       to read following a response where [data] is an error *)
    let dummy_buffer = Bigstring.create 1 in
    let dummy_ref = ref 0 in
    Hashtbl.iteri t.open_queries
      ~f:(fun ~key:query_id ~data:response_handler ->
        ignore (
          response_handler
            ~read_buffer:dummy_buffer
            ~read_buffer_pos_ref:dummy_ref
            { id   = query_id
            ; data = Error error
            }
        ));
    Hashtbl.clear t.open_queries;
    Bigstring.unsafe_destroy dummy_buffer;
  end
;;

let run_after_handshake t ~implementations ~connection_state =
  let instance =
    Implementations.instantiate implementations ~writer:t.writer
      ~connection_description:t.description
      ~connection_close_started:(Ivar.read t.close_started)
      ~connection_state:(connection_state t)
  in
  Set_once.set_exn t.implementations_instance [%here] instance;
  let monitor = Monitor.create ~name:"RPC connection loop" () in
  let reason name exn =
    (exn, Info.tag (Info.of_exn exn) ~tag:("exn raised in RPC connection " ^ name))
  in
  Stream.iter
    (Stream.interleave (Stream.of_list (
       [ Stream.map ~f:(reason "loop" )
           (Monitor.detach_and_get_error_stream monitor)
       ; Stream.map ~f:(reason "Writer.t")
           (Monitor.detach_and_get_error_stream (Writer.monitor t.writer))
       ])))
    ~f:(fun (exn, reason) -> cleanup t exn ~reason);
  within ~monitor (fun () ->
    let last_heartbeat = ref (Time_ns.now ()) in
    every ~stop:(Ivar.read t.close_started >>| fun (_ : Info.t) -> ())
      t.heartbeat_config.send_every
      (fun () ->
         (* Make sure not to do this after calling [close] -- this function could be
            called between when [t.close_started] is determined and when [stop] is, since
            they happen in different Async jobs. *)
         if not (Ivar.is_full t.close_started)
         then heartbeat t ~last_heartbeat:!last_heartbeat);
    Reader.read_forever t.reader
      ~on_message:(Staged.unstage (on_message t))
      ~on_end_of_batch:(fun () ->
        last_heartbeat := Time_ns.now ();
        List.iter !(t.heartbeat_callbacks) ~f:(fun f -> f ())
      )
    >>> function
    | Ok reason ->
      cleanup t ~reason (Rpc_error.Rpc (Connection_closed, t.description))
    (* The protocol is such that right now, the only outcome of the other side closing the
       connection normally is that we get an eof. *)
    | Error (`Eof | `Closed) ->
      cleanup t ~reason:(Info.of_string "EOF or connection closed")
        (Rpc_error.Rpc (Connection_closed, t.description))
  )
;;

let do_handshake t ~handshake_timeout =
  if Writer.is_closed t.writer then
    return (Error Handshake_error.Transport_closed)
  else begin
    Writer.send_bin_prot t.writer Header.bin_t.writer Header.v1 |> handle_send_result t;
    (* If we use [max_connections] in the server, then this read may just hang until the
       server starts accepting new connections (which could be never).  That is why a
       timeout is used *)
    let result =
      Monitor.try_with ~run:`Now (fun () ->
        Reader.read_one_message_bin_prot t.reader Header.bin_t.reader)
    in
    Clock_ns.with_timeout handshake_timeout result
    >>| function
    | `Timeout ->
      (* There's a pending read, the reader is basically useless now, so we clean it
         up. *)
      don't_wait_for (close t ~reason:(Info.of_string "Handshake timeout"));
      Error Handshake_error.Timeout
    | `Result (Error exn) ->
      let reason = Info.of_string "[Reader.read_one_message_bin_prot] raised" in
      don't_wait_for (close t ~reason);
      Error (Reading_header_failed (Error.of_exn exn))
    | `Result (Ok (Error `Eof   )) -> Error Eof
    | `Result (Ok (Error `Closed)) -> Error Transport_closed
    | `Result (Ok (Ok     peer))   ->
      match Header.negotiate ~us:Header.v1 ~peer with
      | Error e -> Error (Negotiation_failed e)
      | Ok 1 -> Ok ()
      | Ok i -> Error (Negotiated_unexpected_version i)
  end
;;

let contains_magic_prefix =
  Protocol_version_header.contains_magic_prefix ~protocol:Rpc
;;

let create
      ?implementations
      ~connection_state
      ?(handshake_timeout = default_handshake_timeout)
      ?(heartbeat_config = Heartbeat_config.default)
      ?(description = Info.of_string "<created-directly>")
      ({ reader; writer } : Transport.t)
  =
  let implementations =
    match implementations with None -> Implementations.null () | Some s -> s
  in
  let t =
    { description
    ; heartbeat_config
    ; heartbeat_callbacks = ref []
    ; reader
    ; writer
    ; open_queries   = Hashtbl.Poly.create ~size:10 ()
    ; close_started  = Ivar.create ()
    ; close_finished = Ivar.create ()
    ; implementations_instance = Set_once.create ()
    }
  in
  upon (Writer.stopped writer) (fun () ->
    don't_wait_for (close t ~reason:(Info.of_string "RPC transport stopped")));
  do_handshake t ~handshake_timeout
  >>| function
  | Ok () ->
    run_after_handshake t ~implementations ~connection_state;
    Ok t
  | Error error ->
    Error (Handshake_error.to_exn ~connection_description:description error)
;;

let with_close
      ?implementations
      ?handshake_timeout
      ?heartbeat_config
      ~connection_state
      transport
      ~dispatch_queries
      ~on_handshake_error =
  let handle_handshake_error =
    match on_handshake_error with
    | `Call f -> f
    | `Raise -> raise
  in
  create ?implementations ?handshake_timeout ?heartbeat_config
    ~connection_state transport
  >>= fun t ->
  match t with
  | Error e ->
    Transport.close transport
    >>= fun () ->
    handle_handshake_error e
  | Ok t ->
    Monitor.protect
      ~finally:(fun () ->
        close t ~reason:(Info.of_string "Rpc.Connection.with_close finished")
      ) (fun () ->
        dispatch_queries t
        >>= fun result ->
        (match implementations with
         | None -> Deferred.unit
         | Some _ -> close_finished t)
        >>| fun () ->
        result
      )
;;

let server_with_close ?handshake_timeout ?heartbeat_config
      transport ~implementations ~connection_state ~on_handshake_error =
  let on_handshake_error =
    match on_handshake_error with
    | `Call f -> `Call f
    | `Raise -> `Raise
    | `Ignore -> `Call (fun _ -> Deferred.unit)
  in
  with_close ?handshake_timeout ?heartbeat_config
    transport ~implementations ~connection_state
    ~on_handshake_error ~dispatch_queries:(fun _ -> Deferred.unit)
;;

let close
      ?streaming_responses_flush_timeout
      ?(reason = Info.of_string "Rpc.Connection.close")
      t
  = close ?streaming_responses_flush_timeout ~reason t

module Client_implementations = struct
  type nonrec 's t =
    { connection_state : t -> 's
    ; implementations  : 's Implementations.t
    }

  let null () =
    { connection_state = (fun _ -> ())
    ; implementations  = Implementations.null ()
    }
end
