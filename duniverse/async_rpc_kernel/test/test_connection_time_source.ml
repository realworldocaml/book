open! Core
open! Async
open! Async_rpc_kernel
open! Async_rpc_kernel_private
open! Expect_test_helpers_core
open! Expect_test_helpers_async
module Time_ns = Core_kernel.Core_kernel_private.Time_ns_alternate_sexp

let sec = Time_ns.Span.of_sec

let advance_by_span time_source span =
  let to_ = Time_ns.add (Synchronous_time_source.now time_source) span in
  Synchronous_time_source.advance_by_alarms time_source ~to_ |> ok_exn
;;

let heartbeat_every = sec 2.
let heartbeat_timeout = sec 10.
let yield () = Async_kernel_scheduler.yield_until_no_jobs_remain ()

let establish_connection transport time_source description =
  let conn =
    Connection.create
      ~connection_state:(fun _ -> ())
      ~heartbeat_config:
        (Connection.Heartbeat_config.create
           ~timeout:heartbeat_timeout
           ~send_every:heartbeat_every
           ())
      ~description
      ~time_source
      transport
  in
  Deferred.upon conn (fun conn ->
    let conn = Result.ok_exn conn in
    let () =
      Deferred.upon
        (Connection.close_reason conn ~on_close:`started)
        (fun reason ->
           print_s
             [%message
               "connection closed"
                 ~now:(Synchronous_time_source.now time_source : Time_ns.t)
                 (description : Info.t)
                 (reason : Info.t)])
    in
    let () =
      Connection.add_heartbeat_callback conn (fun () ->
        print_s
          [%message
            "received heartbeat"
              ~now:(Synchronous_time_source.now time_source : Time_ns.t)
              (description : Info.t)])
    in
    ());
  conn >>| Result.ok_exn
;;

let%expect_test "test connection with time_source <> wall_clock" =
  let server_time_source = Synchronous_time_source.create ~now:Time_ns.epoch () in
  let client_time_source = Synchronous_time_source.create ~now:Time_ns.epoch () in
  let server_r, server_w = Pipe.create () in
  let client_r, client_w = Pipe.create () in
  let server_transport =
    Pipe_transport.create Pipe_transport.Kind.bigstring client_r server_w
  in
  let client_transport =
    Pipe_transport.create Pipe_transport.Kind.bigstring server_r client_w
  in
  let server_conn =
    establish_connection
      server_transport
      (Synchronous_time_source.read_only server_time_source)
      (Info.of_string "server")
  in
  let client_conn =
    establish_connection
      client_transport
      (Synchronous_time_source.read_only client_time_source)
      (Info.of_string "client")
  in
  let%bind server_conn, client_conn = Deferred.both server_conn client_conn in
  let%bind () =
    [%expect
      {|
    ("received heartbeat"
      (now         "1970-01-01 00:00:00Z")
      (description server))
    ("received heartbeat"
      (now         "1970-01-01 00:00:00Z")
      (description client)) |}]
  in
  advance_by_span server_time_source heartbeat_every;
  advance_by_span client_time_source heartbeat_every;
  let%bind () = yield () in
  let%bind () =
    [%expect
      {|
    ("received heartbeat"
      (now         "1970-01-01 00:00:02Z")
      (description client))
    ("received heartbeat"
      (now         "1970-01-01 00:00:02Z")
      (description server)) |}]
  in
  advance_by_span server_time_source heartbeat_timeout;
  let%bind () = yield () in
  let%bind () =
    [%expect
      {|
    ("received heartbeat"
      (now         "1970-01-01 00:00:02Z")
      (description client)) |}]
  in
  advance_by_span server_time_source heartbeat_every;
  let%bind () = yield () in
  let%bind () =
    [%expect
      {|
    ("connection closed"
      (now         "1970-01-01 00:00:14Z")
      (description server)
      (reason      "No heartbeats received for 10s."))
    ("connection closed"
      (now         "1970-01-01 00:00:02Z")
      (description client)
      (reason      "EOF or connection closed")) |}]
  in
  Deferred.all_unit
    [ Connection.close_finished server_conn; Connection.close_finished client_conn ]
;;
