open! Core
open Poly
open! Async
open! Import

let make_tests ~make_transport ~transport_name =
  List.mapi
    ~f:(fun i f -> sprintf "rpc-%s-%d" transport_name i, f)
    [ test1 ~make_transport ~imp:[ pipe_count_imp ] ~state:() ~f:(fun _ conn ->
        let n = 3 in
        let%bind pipe_r, _id = Rpc.Pipe_rpc.dispatch_exn pipe_count_rpc conn n in
        let%bind x =
          Pipe.fold_without_pushback pipe_r ~init:0 ~f:(fun x i ->
            assert (x = i);
            i + 1)
        in
        [%test_result: int] ~expect:n x;
        Deferred.unit)
    ; test1 ~make_transport ~imp:[ pipe_count_imp ] ~state:() ~f:(fun _ conn ->
        let%bind result = Rpc.Pipe_rpc.dispatch pipe_count_rpc conn (-1) in
        match result with
        | Ok (Ok _) | Error _ -> assert false
        | Ok (Error `Argument_must_be_positive) -> Deferred.unit)
    ; (let ivar = Ivar.create () in
       test1 ~make_transport ~imp:[ pipe_wait_imp ivar ] ~state:() ~f:(fun conn1 conn2 ->
         (* Test that the pipe is flushed when the connection is closed. *)
         let%bind pipe_r, _id = Rpc.Pipe_rpc.dispatch_exn pipe_wait_rpc conn2 () in
         let%bind res = Pipe.read pipe_r in
         assert (res = `Ok ());
         don't_wait_for (Rpc.Connection.close conn1);
         Ivar.fill ivar ();
         let%bind res = Pipe.read pipe_r in
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
    rpc-low-latency-2 |}];
  return ()
;;

let%expect_test "[Connection.create] shouldn't raise" =
  let%bind `Reader r1, `Writer w1 = Unix.pipe (Info.of_string "rpc_test 1") in
  let%bind `Reader _, `Writer w2 = Unix.pipe (Info.of_string "rpc_test 2") in
  let result =
    Deferred.create (fun ivar ->
      Monitor.try_with
        ~run:
          `Schedule
        ~rest:`Log
        (fun () ->
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
       <created-directly>))) |}];
  return ()
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
      let%bind connection =
        Connection.client
          (Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port })
        >>| Result.ok_exn
      in
      let res = Rpc.dispatch rpc connection () in
      don't_wait_for (Connection.close connection);
      match%map res with
      | Ok () -> failwith "Dispatch should have failed"
      | Error err ->
        [%test_eq: string]
          (sprintf
             "((rpc_error (Connection_closed (Rpc.Connection.close)))\n\
             \ (connection_description (\"Client connected via TCP\" (localhost %d)))\n\
             \ (rpc_name __TEST_Async_rpc.Rpc) (rpc_version 1))"
             port)
          (Error.to_string_hum err)
    in
    let%bind server = serve () in
    let port = Tcp.Server.listening_on server in
    let%bind () = client ~port in
    Tcp.Server.close server)
;;

let%test_module "Exception handling" =
  (module struct
    let test_exception_handling
          ?client_received_response
          ?use_on_exception
          ~expect_close_connection
          ~implementation
          ~dispatch
          ~ok_is_expected
          ~check_error
          ()
      =
      let callback_triggered = Ivar.create () in
      let connection_closed_by_server = Ivar.create () in
      let serve
            (on_exception : Async_rpc_kernel.Rpc.On_exception.t option)
            ~implementation
        =
        let implementations =
          Implementations.create_exn
            ~implementations:[ implementation on_exception ]
            ~on_unknown_rpc:`Raise
        in
        Connection.serve
          ~initial_connection_state:(fun _ connection ->
            don't_wait_for
              (let%map () = Connection.close_finished connection in
               Ivar.fill connection_closed_by_server ()))
          ~implementations
          ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
          ()
      in
      let client ~port =
        let%bind connection =
          Connection.client
            (Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port })
          >>| Result.ok_exn
        in
        let res = dispatch connection () in
        let%bind () =
          if Option.is_none use_on_exception
          then Deferred.unit
          else Ivar.read callback_triggered
        in
        let%bind () = Scheduler.yield_until_no_jobs_remain () in
        let%bind res = res in
        Option.iter client_received_response ~f:(Fn.flip Ivar.fill ());
        let%bind () = Scheduler.yield_until_no_jobs_remain () in
        if not expect_close_connection
        then (
          [%test_pred: unit Ivar.t] Ivar.is_empty connection_closed_by_server;
          [%test_pred: Connection.t] (Fn.non Connection.is_closed) connection;
          don't_wait_for (Connection.close connection));
        (match res with
         | Ok _ -> assert ok_is_expected
         | Error err -> check_error err);
        Connection.close_finished connection
      in
      let on_exception =
        Option.map use_on_exception ~f:(fun () ->
          { Async_rpc_kernel.Rpc.On_exception.callback =
              Some (fun (_ : exn) -> Ivar.fill callback_triggered ())
          ; close_connection_if_no_return_value = expect_close_connection
          })
      in
      let%bind server = serve on_exception ~implementation in
      Monitor.protect
        ~finally:(fun () -> Tcp.Server.close server)
        (fun () ->
           let port = Tcp.Server.listening_on server in
           client ~port)
    ;;

    let location_field_in_error_msg err ~expected =
      err
      |> Error.to_string_mach
      |> Sexp.of_string
      |> Sexp_select.select "location"
      |> List.hd
      |> Option.value ~default:(Error.to_string_hum err |> Sexp.of_string)
      |> Sexp.to_string_mach
      |> [%test_eq: string] expected
    ;;

    let bin_t = Bin_prot.Type_class.bin_unit

    let test_exception_handling_using_on_exception
          ?client_received_response
          ~implementation
          ~dispatch
          ~ok_is_expected
          ~check_error
          ()
      =
      let f ~expect_close_connection =
        test_exception_handling
          ?client_received_response
          ~use_on_exception:()
          ~expect_close_connection
          ~implementation
          ~dispatch
          ~ok_is_expected
          ~check_error
          ()
      in
      let%bind () = f ~expect_close_connection:false in
      f ~expect_close_connection:true
    ;;

    let%test_module "RPC" =
      (module struct
        let rpc =
          Rpc.create
            ~version:1
            ~name:"__TEST_Async_rpc.Rpc"
            ~bin_query:bin_t
            ~bin_response:bin_t
        ;;

        let implementation on_exception =
          Rpc.implement ?on_exception rpc (fun () () -> failwith "Exception")
        ;;

        let dispatch connection () = Rpc.dispatch rpc connection ()

        let%expect_test "Regular rpc does not close the connection but returns the error" =
          let%bind () =
            test_exception_handling
              ~expect_close_connection:false
              ~implementation
              ~dispatch
              ~ok_is_expected:false
              ~check_error:
                (location_field_in_error_msg ~expected:{|"server-side rpc computation"|})
              ()
          in
          [%expect {| |}];
          Deferred.unit
        ;;

        let%expect_test "Regular rpc honors On_exception.t" =
          let%bind () =
            test_exception_handling_using_on_exception
              ~implementation
              ~dispatch
              ~ok_is_expected:false
              ~check_error:
                (location_field_in_error_msg ~expected:{|"server-side rpc computation"|})
              ()
          in
          [%expect {| |}];
          Deferred.unit
        ;;
      end)
    ;;

    let%test_module "ONE-WAY" =
      (module struct
        let rpc =
          One_way.create ~version:1 ~name:"__TEST_Async_rpc.One_way" ~bin_msg:bin_t
        ;;

        let implementation on_exception =
          One_way.implement ?on_exception rpc (fun () () -> failwith "Exception")
        ;;

        let dispatch connection () =
          Deferred.Or_error.return (One_way.dispatch rpc connection ())
        ;;

        let%expect_test "One way rpc closes the connection because it can't return the \
                         error"
          =
          let%bind () =
            test_exception_handling
              ~expect_close_connection:true
              ~implementation
              ~dispatch
              ~ok_is_expected:true
              ~check_error:(Fn.const ())
              ()
          in
          [%expect {| |}];
          Deferred.unit
        ;;

        let%expect_test "One way rpc honors On_exception.t" =
          let%bind () =
            test_exception_handling_using_on_exception
              ~implementation
              ~dispatch
              ~ok_is_expected:true
              ~check_error:(Fn.const ())
              ()
          in
          [%expect {| |}];
          Deferred.unit
        ;;
      end)
    ;;

    let%test_module "PIPE/STATE" =
      (module struct
        let rpc =
          Pipe_rpc.create
            ~version:1
            ~name:"__TEST_Async_rpc.Pipe_rpc"
            ~bin_query:bin_t
            ~bin_response:bin_t
            ~bin_error:bin_t
            ()
        ;;

        let implementation on_exception =
          Pipe_rpc.implement ?on_exception rpc (fun () () -> failwith "Exception")
        ;;

        let dispatch connection () = Pipe_rpc.dispatch rpc connection ()

        let%expect_test "Pipe/State rpc does not close the connection, but returns the \
                         error"
          =
          let%bind () =
            test_exception_handling
              ~expect_close_connection:false
              ~implementation
              ~dispatch
              ~ok_is_expected:false
              ~check_error:
                (location_field_in_error_msg
                   ~expected:{|"server-side pipe_rpc computation"|})
              ()
          in
          [%expect {| |}];
          Deferred.unit
        ;;

        let%expect_test "Pipe/State rpc honors On_exception.t when it raises immediately" =
          Thread_safe.block_on_async_exn (fun () ->
            test_exception_handling_using_on_exception
              ~implementation
              ~dispatch
              ~ok_is_expected:false
              ~check_error:
                (location_field_in_error_msg
                   ~expected:{|"server-side pipe_rpc computation"|})
              ());
          [%expect {| |}];
          Deferred.unit
        ;;

        let rpc =
          Pipe_rpc.create
            ~version:1
            ~name:"__TEST_Async_rpc.Pipe_rpc"
            ~bin_query:bin_t
            ~bin_response:bin_t
            ~bin_error:bin_t
            ()
        ;;

        let implementation client_received_response on_exception =
          Pipe_rpc.implement ?on_exception rpc (fun () () ->
            upon (Ivar.read client_received_response) (fun () ->
              failwith "Asynchronous failure");
            failwith "Exception")
        ;;

        let dispatch connection () = Pipe_rpc.dispatch rpc connection ()

        let%expect_test "Pipe/State rpc may swallow asynchronous errors and merely log \
                         them"
          =
          let%bind () =
            let client_received_response = Ivar.create () in
            let%bind () =
              test_exception_handling
                ~client_received_response
                ~expect_close_connection:false
                ~implementation:(implementation client_received_response)
                ~dispatch
                ~ok_is_expected:false
                ~check_error:
                  (location_field_in_error_msg
                     ~expected:{|"server-side pipe_rpc computation"|})
                ()
            in
            Scheduler.yield_until_no_jobs_remain ()
          in
          [%expect
            {| 1969-12-31 19:00:00.000000-05:00 Error ("Exception raised to [Monitor.try_with] that already returned.""This error was captured by a default handler in [Async.Log]."(exn(monitor.ml.Error(Failure"Asynchronous failure")("<backtrace elided in test>")))) |}];
          Deferred.unit
        ;;

        let%expect_test "Pipe/State rpc honors On_exception.t for async errors: don't \
                         close connection"
          =
          let%bind () =
            let client_received_response = Ivar.create () in
            let%bind () =
              test_exception_handling
                ~use_on_exception:()
                ~client_received_response
                ~expect_close_connection:false
                ~implementation:(implementation client_received_response)
                ~dispatch
                ~ok_is_expected:false
                ~check_error:
                  (location_field_in_error_msg
                     ~expected:{|"server-side pipe_rpc computation"|})
                ()
            in
            Scheduler.yield_until_no_jobs_remain ()
          in
          [%expect {| |}];
          Deferred.unit
        ;;

        let%expect_test "Pipe/State rpc honors On_exception.t for async errors: close \
                         connection"
          =
          let%bind () =
            let client_received_response = Ivar.create () in
            let%bind () =
              test_exception_handling
                ~use_on_exception:()
                ~client_received_response
                ~expect_close_connection:true
                ~implementation:(implementation client_received_response)
                ~dispatch
                ~ok_is_expected:false
                ~check_error:
                  (location_field_in_error_msg
                     ~expected:{|"server-side pipe_rpc computation"|})
                ()
            in
            Scheduler.yield_until_no_jobs_remain ()
          in
          [%expect {| |}];
          Deferred.unit
        ;;
      end)
    ;;
  end)
;;
