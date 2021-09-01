open! Core
open! Async
module Session = Session
module X509_async = X509_async

let try_to_close t =
  match%map Session.close_tls t with
  | Ok () -> ()
  | Error tls_close_error -> Log.Global.error_s [%sexp (tls_close_error : Error.t)]
;;

let pipe t =
  let b_reader = Cstruct.create 0x8000 in
  let rec f_reader writer =
    match%bind Session.read t b_reader with
    | Ok 0 ->
      Pipe.close writer;
      return ()
    | Ok len ->
      let%bind () = Pipe.write writer (Cstruct.to_string (Cstruct.sub b_reader 0 len)) in
      f_reader writer
    | Error read_error ->
      Log.Global.error_s [%sexp (read_error : Error.t)];
      Pipe.close writer;
      return ()
  in
  let rec f_writer reader =
    let%bind pipe_read = Pipe.read reader in
    match pipe_read with
    | `Ok s ->
      (match%bind Session.writev t [ Cstruct.of_string s ] with
       | Ok () -> f_writer reader
       | Error (_ : Error.t) -> try_to_close t)
    | `Eof -> try_to_close t
  in
  Pipe.create_reader ~close_on_exception:false f_reader, Pipe.create_writer f_writer
;;

let upgrade_connection tls_session ((_ : Reader.t), outer_writer) =
  let pipe_r, pipe_w = pipe tls_session in
  let%bind inner_reader = Reader.of_pipe (Info.of_string "tls_reader") pipe_r in
  let%map inner_writer, `Closed_and_flushed_downstream inner_cafd =
    Writer.of_pipe (Info.of_string "tls_writer") pipe_w
  in
  Writer.set_raise_when_consumer_leaves inner_writer false;
  let outer_cafd =
    (* Ordering is important here to ensure no data is lost during the session shutdown *)
    let%bind () = Writer.close_finished inner_writer in
    let%bind () = inner_cafd in
    let%bind () = try_to_close tls_session in
    Writer.flushed outer_writer
  in
  tls_session, inner_reader, inner_writer, `Tls_closed_and_flushed_downstream outer_cafd
;;

let upgrade_server_reader_writer_to_tls config rw =
  let open Deferred.Or_error.Let_syntax in
  let%bind tls_session = Session.server_of_fd config rw in
  upgrade_connection tls_session rw |> Deferred.ok
;;

let upgrade_client_reader_writer_to_tls ?host config rw =
  let open Deferred.Or_error.Let_syntax in
  let%bind tls_session = Session.client_of_fd ?host config rw in
  upgrade_connection tls_session rw |> Deferred.ok
;;

let listen
      ?buffer_age_limit
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      ~on_handler_error
      config
      where_to_listen
      handle_client
  =
  let tls_handler sock outer_reader outer_writer =
    let%bind ( tls_session
             , inner_reader
             , inner_writer
             , `Tls_closed_and_flushed_downstream inner_cafd )
      =
      upgrade_server_reader_writer_to_tls config (outer_reader, outer_writer)
      |> Deferred.Or_error.ok_exn
    in
    Monitor.protect
      (fun () -> handle_client sock tls_session inner_reader inner_writer)
      ~finally:(fun () ->
        Deferred.all_unit
          [ Reader.close inner_reader; Writer.close inner_writer; inner_cafd ])
  in
  Tcp.Server.create
    ?buffer_age_limit
    ?max_connections
    ?max_accepts_per_batch
    ?backlog
    ?socket
    ~on_handler_error
    where_to_listen
    tls_handler
;;

let connect
      ?socket
      ?buffer_age_limit
      ?interrupt
      ?reader_buffer_size
      ?writer_buffer_size
      ?timeout
      config
      where_to_connect
      ~host
  =
  let open Deferred.Or_error.Let_syntax in
  let%bind (_ : ([ `Active ], 'a) Socket.t), outer_reader, outer_writer =
    Tcp.connect
      ?socket
      ?buffer_age_limit
      ?interrupt
      ?reader_buffer_size
      ?writer_buffer_size
      ?timeout
      where_to_connect
    |> Deferred.ok
  in
  let%bind ( tls_session
           , inner_reader
           , inner_writer
           , `Tls_closed_and_flushed_downstream inner_cafd )
    =
    upgrade_client_reader_writer_to_tls ?host config (outer_reader, outer_writer)
  in
  don't_wait_for
    (let%bind.Deferred () = inner_cafd in
     Deferred.all_unit [ Writer.close outer_writer; Reader.close outer_reader ]);
  return (tls_session, inner_reader, inner_writer)
;;

(* initialized RNG early to maximise available entropy. *)
let () = Mirage_crypto_rng_async.initialize (module Mirage_crypto_rng.Fortuna)
