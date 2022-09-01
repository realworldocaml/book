open! Core
open! Async_kernel
open! Async_kernel_require_explicit_time_source
include Persistent_connection_kernel_intf

module Make (Conn : Closable) = struct
  module Conn = struct
    include Conn

    let sexp_of_t (_ : t) : Sexp.t = Atom "<Conn.t>"
  end

  type conn = Conn.t

  module Event = struct
    type 'address t =
      | Attempting_to_connect
      | Obtained_address of 'address
      | Failed_to_connect of Error.t
      | Connected of (conn[@sexp.opaque])
      | Disconnected
    [@@deriving sexp_of]

    type 'address event = 'address t

    module Handler = struct
      type 'address t =
        { server_name : string
        ; on_event : 'address event -> unit Deferred.t
        }
      [@@deriving sexp_of]
    end

    let log_level = function
      | Attempting_to_connect | Connected _ | Disconnected | Obtained_address _ -> `Info
      | Failed_to_connect _ -> `Error
    ;;

    let handle t { Handler.server_name = _; on_event } = on_event t
  end

  (* A persistent connection that is polymorphic in the address type.  We hide away this
     type later since it only appears in the type of [create]. *)
  module Poly = struct
    type 'address t =
      { get_address : unit -> 'address Or_error.t Deferred.t
      ; connect : 'address -> Conn.t Or_error.t Deferred.t
      ; retry_delay : unit -> unit Deferred.t
      ; mutable conn : [ `Ok of Conn.t | `Close_started ] Ivar.t
      ; mutable next_connect_result : Conn.t Or_error.t Ivar.t
      ; event_handler : 'address Event.Handler.t
      ; close_started : unit Ivar.t
      ; close_finished : unit Ivar.t
      ; don't_reconnect : unit Ivar.t
      ; address_equal : 'address -> 'address -> bool
      ; sexp_of_address : 'address -> Sexp.t
      }
    [@@deriving fields, sexp_of]

    let server_name t = t.event_handler.server_name
    let handle_event t event = Event.handle event t.event_handler

    (* This function focuses in on the the error itself, discarding information about which
       monitor caught the error, if any.

       If we don't do this, we sometimes end up with noisy logs which report the same error
       again and again, differing only as to what monitor caught them. *)
    let same_error e1 e2 =
      let to_sexp e = Exn.sexp_of_t (Monitor.extract_exn (Error.to_exn e)) in
      Sexp.equal (to_sexp e1) (to_sexp e2)
    ;;

    (* Continue trying to connect until we are able to do so, in which case we return both
       the new connection and a deferred that will become determined once we are ready for
       the next reconnection attempt. *)
    let try_connecting_until_successful t =
      (* We take care not to spam logs with the same message over and over by comparing
         each log message the the previous one of the same type. *)
      let previous_address = ref None in
      let previous_error = ref None in
      let connect () =
        t.get_address ()
        >>= function
        | Error e -> return (Error e)
        | Ok addr ->
          let same_as_previous_address =
            match !previous_address with
            | None -> false
            | Some previous_address -> t.address_equal addr previous_address
          in
          previous_address := Some addr;
          (if same_as_previous_address
           then Deferred.unit
           else handle_event t (Obtained_address addr))
          >>= fun () -> t.connect addr
      in
      let rec loop () =
        if Ivar.is_full t.close_started
        then return `Close_started
        else if Ivar.is_full t.don't_reconnect
        then return `Don't_reconnect
        else (
          let ready_to_retry_connecting = t.retry_delay () in
          let%bind connect_result = connect () in
          Ivar.fill t.next_connect_result connect_result;
          t.next_connect_result <- Ivar.create ();
          match connect_result with
          | Ok conn -> return (`Ok (conn, ready_to_retry_connecting))
          | Error err ->
            let same_as_previous_error =
              match !previous_error with
              | None -> false
              | Some previous_err -> same_error err previous_err
            in
            previous_error := Some err;
            (if same_as_previous_error
             then Deferred.unit
             else handle_event t (Failed_to_connect err))
            >>= fun () ->
            Deferred.any
              [ ready_to_retry_connecting
              ; Ivar.read t.close_started
              ; Ivar.read t.don't_reconnect
              ]
            >>= fun () -> loop ())
      in
      loop ()
    ;;

    let abort_reconnecting_with_no_active_connection t =
      Ivar.fill t.close_started ();
      Ivar.fill t.close_finished ();
      Ivar.fill t.conn `Close_started
    ;;

    let create
          (type address)
          ~server_name
          ?(on_event = fun _ -> Deferred.unit)
          ?retry_delay
          ?(random_state = `State Random.State.default)
          ?(time_source = Time_source.wall_clock ())
          ~connect
          ~address:(module Address : Address with type t = address)
          get_address
      =
      let event_handler = { Event.Handler.server_name; on_event } in
      let default_retry_delay =
        Fn.const (Time_ns.Span.of_sec (if am_running_test then 0.1 else 10.))
      in
      let non_randomized_delay = Option.value retry_delay ~default:default_retry_delay in
      let retry_delay_span =
        match random_state with
        | `Non_random -> non_randomized_delay
        | `State random_state ->
          fun () ->
            let span = non_randomized_delay () in
            let span = Time_ns.Span.to_sec span in
            let distance = Random.State.float random_state (span *. 0.3) in
            let wait =
              if Random.State.bool random_state
              then span +. distance
              else span -. distance
            in
            Time_ns.Span.of_sec wait
      in
      let retry_delay () = Time_source.after time_source (retry_delay_span ()) in
      let t =
        { event_handler
        ; get_address
        ; connect
        ; next_connect_result = Ivar.create ()
        ; retry_delay
        ; conn = Ivar.create ()
        ; close_started = Ivar.create ()
        ; close_finished = Ivar.create ()
        ; don't_reconnect = Ivar.create ()
        ; address_equal = Address.equal
        ; sexp_of_address = Address.sexp_of_t
        }
      in
      (* this loop finishes once [close t] has been called, in which case it makes sure to
         leave [t.conn] filled with [`Close_started]. *)
      don't_wait_for
      @@ Deferred.repeat_until_finished () (fun () ->
        handle_event t Attempting_to_connect
        >>= fun () ->
        try_connecting_until_successful t
        >>= function
        | `Close_started ->
          Ivar.fill t.conn `Close_started;
          return (`Finished ())
        | `Don't_reconnect ->
          abort_reconnecting_with_no_active_connection t;
          return (`Finished ())
        | `Ok (conn, ready_to_retry_connecting) ->
          Ivar.fill t.conn (`Ok conn);
          handle_event t (Connected conn)
          >>= fun () ->
          Conn.close_finished conn
          >>= fun () ->
          t.conn <- Ivar.create ();
          handle_event t Disconnected
          >>= fun () ->
          (* waits until [retry_delay ()] time has passed since the time just before we last
             tried to connect rather than the time we noticed being disconnected, so that if
             a long-lived connection dies, we will attempt to reconnect immediately. *)
          let%map () =
            Deferred.any
              [ ready_to_retry_connecting
              ; Ivar.read t.close_started
              ; Ivar.read t.don't_reconnect
              ]
          in
          if Ivar.is_full t.close_started
          then (
            Ivar.fill t.conn `Close_started;
            `Finished ())
          else if Ivar.is_full t.don't_reconnect
          then (
            abort_reconnecting_with_no_active_connection t;
            `Finished ())
          else `Repeat ());
      t
    ;;

    let connected t =
      (* Take care not to return a connection that is known to be closed at the time
         [connected] was called.  This could happen in client code that behaves like
         {[
           Persistent_connection.Rpc.connected t
           >>= fun c1 ->
           ...
             Rpc.Connection.close_finished c1
           (* at this point we are in a race with the same call inside
              persistent_client.ml *)
           >>= fun () ->
           Persistent_connection.Rpc.connected t
           (* depending on how the race turns out, we don't want to get a closed connection
              here *)
           >>= fun c2 ->
           ...
         ]}
         This doesn't remove the race condition, but it makes it less likely to happen.
      *)
      let rec loop () =
        let d = Ivar.read t.conn in
        match Deferred.peek d with
        | None ->
          d
          >>= (function
            | `Close_started -> Deferred.never ()
            | `Ok conn -> return conn)
        | Some `Close_started -> Deferred.never ()
        | Some (`Ok conn) ->
          if Conn.is_closed conn
          then
            (* give the reconnection loop a chance to overwrite the ivar *)
            Conn.close_finished conn >>= loop
          else return conn
      in
      loop ()
    ;;

    let current_connection t =
      match Deferred.peek (Ivar.read t.conn) with
      | None | Some `Close_started -> None
      | Some (`Ok conn) -> Some conn
    ;;

    let close_finished t = Ivar.read t.close_finished
    let is_closed t = Ivar.is_full t.close_started

    let close t =
      if Ivar.is_full t.close_started
      then
        (* Another call to close is already in progress.  Wait for it to finish. *)
        close_finished t
      else (
        Ivar.fill t.close_started ();
        Ivar.read t.conn
        >>= fun conn_opt ->
        (match conn_opt with
         | `Close_started -> Deferred.unit
         | `Ok conn -> Conn.close conn)
        >>| fun () -> Ivar.fill t.close_finished ())
    ;;

    let connected_or_failed_to_connect_connection_closed =
      Or_error.error_s [%message "Persistent connection closed"]
    ;;

    let connected_or_failed_to_connect t =
      if is_closed t
      then return connected_or_failed_to_connect_connection_closed
      else (
        match current_connection t with
        | Some x when not (Conn.is_closed x) -> return (Ok x)
        | Some (_ : Conn.t) | None ->
          Deferred.choose
            [ choice (Ivar.read t.close_started) (fun () ->
                connected_or_failed_to_connect_connection_closed)
            ; choice (Ivar.read t.next_connect_result) Fn.id
            ])
    ;;

    let close_when_current_connection_is_closed t =
      Ivar.fill_if_empty t.don't_reconnect ()
    ;;
  end

  type t = T : 'address Poly.t -> t [@@unboxed]

  let sexp_of_t (T t) = Poly.sexp_of_t t.sexp_of_address t

  let close_when_current_connection_is_closed (T t) =
    Poly.close_when_current_connection_is_closed t
  ;;

  let close_finished (T t) = Poly.close_finished t
  let is_closed (T t) = Poly.is_closed t
  let close (T t) = Poly.close t
  let server_name (T t) = Poly.server_name t
  let current_connection (T t) = Poly.current_connection t
  let connected_or_failed_to_connect (T t) = Poly.connected_or_failed_to_connect t
  let connected (T t) = Poly.connected t

  let create
        ~server_name
        ?on_event
        ?retry_delay
        ?random_state
        ?time_source
        ~connect
        ~address
        get_address
    =
    T
      (Poly.create
         ~server_name
         ?on_event
         ?retry_delay
         ?random_state
         ?time_source
         ~connect
         ~address
         get_address)
  ;;
end
