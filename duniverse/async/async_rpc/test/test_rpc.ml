open! Core
open Poly
open! Async
open! Import
module Debug = Async_kernel_private.Debug

let max_message_size = 1_000_000

let test ~make_transport ~imp1 ~imp2 ~state1 ~state2 ~f () =
  Unix.pipe (Info.of_string "rpc_test 1")
  >>= fun (`Reader r1, `Writer w2) ->
  Unix.pipe (Info.of_string "rpc_test 2")
  >>= fun (`Reader r2, `Writer w1) ->
  let t1 = make_transport (r1, w1) in
  let t2 = make_transport (r2, w2) in
  let s imp =
    if List.length imp > 0
    then
      Some
        (Rpc.Implementations.create_exn
           ~implementations:imp
           ~on_unknown_rpc:`Close_connection)
    else None
  in
  let s1 = s imp1 in
  let s2 = s imp2 in
  let conn1_ivar = Ivar.create () in
  let f2_done =
    Async_rpc_kernel.Rpc.Connection.with_close
      ?implementations:s2
      t2
      ~dispatch_queries:(fun conn2 ->
        Ivar.read conn1_ivar >>= fun conn1 -> f conn1 conn2)
      ~connection_state:(fun _ -> state2)
      ~on_handshake_error:`Raise
  in
  Async_rpc_kernel.Rpc.Connection.with_close
    ?implementations:s1
    t1
    ~dispatch_queries:(fun conn1 ->
      Ivar.fill conn1_ivar conn1;
      f2_done)
    ~connection_state:(fun _ -> state1)
    ~on_handshake_error:`Raise
;;

let test1 ~make_transport ~imp ~state ~f =
  test ~make_transport ~imp1:imp ~state1:state ~imp2:[] ~state2:() ~f
;;

module Pipe_count_error = struct
  type t = [ `Argument_must_be_positive ] [@@deriving bin_io]
end

let pipe_count_rpc =
  Rpc.Pipe_rpc.create
    ~name:"pipe_count"
    ~version:0
    ~bin_query:Int.bin_t
    ~bin_response:Int.bin_t
    ~bin_error:Pipe_count_error.bin_t
    ()
;;

let pipe_wait_rpc =
  Rpc.Pipe_rpc.create
    ~name:"pipe_wait"
    ~version:0
    ~bin_query:Unit.bin_t
    ~bin_response:Unit.bin_t
    ~bin_error:Unit.bin_t
    ()
;;

let pipe_count_imp =
  Rpc.Pipe_rpc.implement pipe_count_rpc (fun () n ->
    if n < 0
    then return (Error `Argument_must_be_positive)
    else (
      let pipe_r, pipe_w = Pipe.create () in
      upon
        (Deferred.List.iter (List.init n ~f:Fn.id) ~how:`Sequential ~f:(fun i ->
           Pipe.write pipe_w i))
        (fun () -> Pipe.close pipe_w);
      return (Ok pipe_r)))
;;

let pipe_wait_imp ivar =
  Rpc.Pipe_rpc.implement pipe_wait_rpc (fun () () ->
    let pipe_r, pipe_w = Pipe.create () in
    (Pipe.write pipe_w ()
     >>> fun () ->
     Ivar.read ivar >>> fun () -> Pipe.write pipe_w () >>> fun () -> Pipe.close pipe_w);
    return (Ok pipe_r))
;;

let make_tests ~make_transport ~transport_name =
  List.mapi
    ~f:(fun i f -> sprintf "rpc-%s-%d" transport_name i, f)
    [ test1 ~make_transport ~imp:[ pipe_count_imp ] ~state:() ~f:(fun _ conn ->
        let n = 3 in
        Rpc.Pipe_rpc.dispatch_exn pipe_count_rpc conn n
        >>= fun (pipe_r, _id) ->
        Pipe.fold_without_pushback pipe_r ~init:0 ~f:(fun x i ->
          assert (x = i);
          i + 1)
        >>= fun x ->
        [%test_result: int] ~expect:n x;
        Deferred.unit)
    ; test1 ~make_transport ~imp:[ pipe_count_imp ] ~state:() ~f:(fun _ conn ->
        Rpc.Pipe_rpc.dispatch pipe_count_rpc conn (-1)
        >>= fun result ->
        match result with
        | Ok (Ok _) | Error _ -> assert false
        | Ok (Error `Argument_must_be_positive) -> Deferred.unit)
    ; (let ivar = Ivar.create () in
       test1 ~make_transport ~imp:[ pipe_wait_imp ivar ] ~state:() ~f:(fun conn1 conn2 ->
         (* Test that the pipe is flushed when the connection is closed. *)
         Rpc.Pipe_rpc.dispatch_exn pipe_wait_rpc conn2 ()
         >>= fun (pipe_r, _id) ->
         Pipe.read pipe_r
         >>= fun res ->
         assert (res = `Ok ());
         don't_wait_for (Rpc.Connection.close conn1);
         Ivar.fill ivar ();
         Pipe.read pipe_r
         >>= fun res ->
         assert (res = `Ok ());
         Deferred.unit))
    ]
;;

let%expect_test _ =
  let make_transport_std (fd_r, fd_w) : Rpc.Transport.t =
    { reader = Reader.create fd_r |> Rpc.Transport.Reader.of_reader ~max_message_size
    ; writer = Writer.create fd_w |> Rpc.Transport.Writer.of_writer ~max_message_size
    }
  in
  let make_transport_low_latency (fd_r, fd_w) : Rpc.Transport.t =
    { reader = Rpc.Low_latency_transport.Reader.create fd_r ~max_message_size
    ; writer = Rpc.Low_latency_transport.Writer.create fd_w ~max_message_size
    }
  in
  let%bind () =
    Deferred.List.iter
      ~f:(fun (name, f) ->
        print_s [%message name];
        f ())
      (make_tests ~make_transport:make_transport_std ~transport_name:"std"
       @ make_tests
           ~make_transport:make_transport_low_latency
           ~transport_name:"low-latency")
  in
  [%expect
    {|
    rpc-std-0
    rpc-std-1
    rpc-std-2
    rpc-low-latency-0
    rpc-low-latency-1
    rpc-low-latency-2 |}]
;;

let%expect_test "[Connection.create] shouldn't raise" =
  let%bind `Reader r1, `Writer w1 = Unix.pipe (Info.of_string "rpc_test 1") in
  let%bind `Reader _, `Writer w2 = Unix.pipe (Info.of_string "rpc_test 2") in
  let result =
    Deferred.create (fun ivar ->
      Monitor.try_with (fun () ->
        Rpc.Connection.create
          ~connection_state:(Fn.const ())
          (Reader.create r1)
          (Writer.create w2))
      >>> function
      | Error exn -> Ivar.fill ivar (`Raised exn)
      | Ok (Ok (_ : Rpc.Connection.t)) -> assert false
      | Ok (Error exn) -> Ivar.fill ivar (`Returned exn))
  in
  let writer1 = Writer.create w1 in
  (* We must write at least [Header.length] (8) bytes. *)
  Writer.write writer1 "failfail";
  let%bind () = Writer.flushed writer1 in
  let%bind result = result in
  print_s [%message "" ~_:(result : [ `Raised of Exn.t | `Returned of Exn.t ])];
  [%expect
    {|
    (Returned
     (connection.ml.Handshake_error.Handshake_error
      ((Reading_header_failed
        (monitor.ml.Error
         (Failure "unsafe_read_int64: value cannot be represented unboxed!")
         ("<backtrace elided in test>")))
       <created-directly>))) |}]
;;

open! Rpc

let%test_unit "Open dispatches see connection closed error" =
  Thread_safe.block_on_async_exn (fun () ->
    let bin_t = Bin_prot.Type_class.bin_unit in
    let rpc =
      Rpc.create
        ~version:1
        ~name:"__TEST_Async_rpc.Rpc"
        ~bin_query:bin_t
        ~bin_response:bin_t
    in
    let serve () =
      let implementation = Rpc.implement rpc (fun () () -> Deferred.never ()) in
      let implementations =
        Implementations.create_exn
          ~implementations:[ implementation ]
          ~on_unknown_rpc:`Raise
      in
      Connection.serve
        ~initial_connection_state:(fun _ _ -> ())
        ~implementations
        ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
        ()
    in
    let client ~port =
      Connection.client
        (Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port })
      >>| Result.ok_exn
      >>= fun connection ->
      let res = Rpc.dispatch rpc connection () in
      don't_wait_for (Connection.close connection);
      res
      >>| function
      | Ok () -> failwith "Dispatch should have failed"
      | Error err ->
        [%test_eq: string]
          (sprintf
             "((rpc_error (Connection_closed (Rpc.Connection.close)))\n\
             \ (connection_description (\"Client connected via TCP\" (localhost %d)))\n\
             \ (rpc_tag __TEST_Async_rpc.Rpc) (rpc_version 1))"
             port)
          (Error.to_string_hum err)
    in
    serve ()
    >>= fun server ->
    let port = Tcp.Server.listening_on server in
    client ~port >>= fun () -> Tcp.Server.close server)
;;
