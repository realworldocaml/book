open Core
open Poly
open Async
open Rpc
module Limiter = Limiter_async.Token_bucket

let run_at_limit ~dispatch msgs_per_sec () =
  let limiter =
    let burst_size = Float.to_int (msgs_per_sec /. 1_000.) in
    Limiter.create_exn
      ~burst_size
      ~sustained_rate_per_sec:msgs_per_sec
      ~continue_on_error:false
      ~initial_burst_size:burst_size
      ()
  in
  let event_precision = Scheduler.event_precision () in
  let rec send_messages () =
    let ran = ref false in
    Limiter.enqueue_exn
      limiter
      ~allow_immediate_run:true
      1000
      (fun () ->
         for _ = 2 to 1000 do
           dispatch ()
         done;
         ran := true)
      ();
    if !ran
    then send_messages ()
    else (
      let rec wait_for_previous_send () =
        if !ran
        then Deferred.unit
        else Clock.after event_precision >>= wait_for_previous_send
      in
      wait_for_previous_send () >>= send_messages)
  in
  send_messages ()
;;

module Pipe_simple_test = struct
  module String_pipe = struct
    module Query = struct
      type t' =
        { msg_size : Byte_units.Stable.V1.t
        ; msgs_per_sec : int
        }
      [@@deriving bin_io, sexp]

      type t = t' option [@@deriving bin_io, sexp]

      let start t =
        let bytes =
          match t with
          | None -> 0
          | Some t -> Byte_units.bytes_int_exn t.msg_size
        in
        let reader, writer = Pipe.create () in
        let total_msgs = ref 0 in
        let start = Time.now () in
        Pipe.set_size_budget reader 1000;
        let string = String.init bytes ~f:(fun _ -> 'A') in
        let stop = Pipe.closed writer in
        Clock.every ~stop (Time.Span.of_sec 1.) (fun () ->
          Log.Global.printf "Queue size: %d" (Pipe.length reader));
        Clock.every ~stop (Time.Span.of_sec 1.) (fun () ->
          Log.Global.printf
            "Messages per sec: %f"
            (Float.of_int !total_msgs
             /. Time.Span.to_sec (Time.diff (Time.now ()) start)));
        let prev = ref (Time.now ()) in
        let () =
          match t with
          | None ->
            let rec loop () =
              Pipe.pushback writer
              >>> fun () ->
              Pipe.write_without_pushback writer string;
              loop ()
            in
            loop ()
          | Some t ->
            Clock.every' ~stop (Time.Span.of_sec 1.) (fun () ->
              let msgs =
                let new_time = Time.now () in
                let diff = Time.Span.to_sec (Time.diff new_time !prev) in
                Log.Global.printf "The diff is %f\n" diff;
                prev := new_time;
                Int.of_float (diff *. Int.to_float t.msgs_per_sec)
              in
              if not (Pipe.is_closed writer)
              then
                for i = 1 to msgs do
                  let _ = i in
                  incr total_msgs;
                  Pipe.write_without_pushback writer string
                done;
              Pipe.downstream_flushed writer
              >>| function
              | `Reader_closed | `Ok -> ())
        in
        reader
      ;;

      let create msg_size msgs_per_sec = { msg_size; msgs_per_sec }
    end

    let rpc =
      Pipe_rpc.create
        ~client_pushes_back:()
        ~name:"test-pipe-rpc"
        ~version:1
        ~bin_query:Query.bin_t
        ~bin_response:String.bin_t
        ~bin_error:Nothing.bin_t
        ()
    ;;
  end

  module Memory_consumption = struct
    let init () =
      let major_cycles = ref 0 in
      ignore (Gc.Alarm.create (fun () -> incr major_cycles));
      Clock.every (Time.Span.of_sec 5.) (fun () ->
        Log.Global.printf "%d major cycles" !major_cycles)
    ;;
  end

  module Client = struct
    let _is_the_right_string msg_size string =
      String.length string = msg_size && String.for_all string ~f:(( = ) 'A')
    ;;

    let main bytes msgs_per_sec host port ~rpc_impl () =
      Memory_consumption.init ();
      let query = Option.map msgs_per_sec ~f:(String_pipe.Query.create bytes) in
      Rpc_impl.make_client rpc_impl host port
      >>| Result.ok_exn
      >>= fun connection ->
      Pipe_rpc.dispatch String_pipe.rpc connection query
      >>| Or_error.ok_exn
      >>= function
      | Error t -> Nothing.unreachable_code t
      | Ok (pipe, _) ->
        let msgs = ref 0 in
        let start = Time.now () in
        let _msg_size = Byte_units.bytes_int_exn bytes in
        Clock.every (Time.Span.of_sec 1.) (fun () ->
          let now = Time.now () in
          let secs = Time.Span.to_sec (Time.diff now start) in
          Log.Global.printf "%f msgs per sec" (Float.of_int !msgs /. secs));
        Pipe.iter_without_pushback pipe ~f:(fun _string -> incr msgs)
    ;;

    let command =
      Command.async_spec
        ~summary:"test client"
        Command.Spec.(
          empty
          +> flag "msg-size" (required Byte_units.arg_type) ~doc:""
          +> flag "msgs-per-sec" (optional int) ~doc:""
          +> flag "hostname" (required string) ~doc:""
          +> flag "port" (required int) ~doc:""
          ++ Rpc_impl.spec ())
        main
    ;;
  end

  module Server = struct
    let implementation =
      Pipe_rpc.implement String_pipe.rpc (fun () query ->
        return (Ok (String_pipe.Query.start query)))
    ;;

    let main port ~rpc_impl () =
      Memory_consumption.init ();
      let implementations =
        Implementations.create_exn
          ~implementations:[ implementation ]
          ~on_unknown_rpc:`Raise
      in
      Rpc_impl.make_server
        ~initial_connection_state:(fun _ -> ())
        ~implementations
        ~port
        rpc_impl
      >>= fun (_ : Rpc_impl.Server.t) -> Deferred.never ()
    ;;

    let command =
      Command.async_spec
        ~summary:"test server"
        Command.Spec.(empty +> flag "port" (required int) ~doc:"" ++ Rpc_impl.spec ())
        main
    ;;
  end

  let command =
    Command.group
      ~summary:
        "Simple client and server to quickly check manually that pipe-rpc is working ok "
      [ "server", Server.command; "client", Client.command ]
  ;;
end

module Heartbeat_pipe_test = struct
  let main () =
    let rpc =
      Rpc.create
        ~name:"Heartbeat_pipe_test"
        ~version:1
        ~bin_query:[%bin_type_class: unit]
        ~bin_response:[%bin_type_class: unit]
    in
    let implementations =
      let implementations = [ Rpc.implement rpc (fun () () -> Deferred.unit) ] in
      Implementations.create_exn ~implementations ~on_unknown_rpc:`Raise
    in
    let heartbeat_config =
      Connection.Heartbeat_config.create
        ~timeout:(Time_ns.Span.of_day 1.)
        ~send_every:(Time_ns.Span.of_day 1.)
        ()
    in
    Connection.serve
      ~implementations
      ~heartbeat_config
      ~initial_connection_state:(fun _ _ -> ())
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      ()
    >>= fun server ->
    let port = Tcp.Server.listening_on server in
    Connection.with_client
      (Tcp.Where_to_connect.of_host_and_port { host = "127.0.0.1"; port })
      ~heartbeat_config
      (fun conn ->
         let c1 = ref 0 in
         Clock.after (sec 1.)
         >>= fun () ->
         Connection.add_heartbeat_callback conn (fun () -> incr c1);
         [%test_eq: int] 0 !c1;
         Clock_ns.after (Time_ns.Span.of_sec 1.)
         >>= fun () ->
         Rpc.dispatch_exn rpc conn ()
         >>= fun () ->
         [%test_eq: int] 1 !c1;
         Tcp.Server.close server
         >>= fun () ->
         (* No more heartbeats now that the server is closed *)
         Connection.add_heartbeat_callback conn (fun () -> assert false);
         Clock_ns.after (Time_ns.Span.of_ms 10.) >>= fun () -> Deferred.unit)
    >>| Result.ok_exn
  ;;

  let command =
    Command.async_spec
      ~summary:"test that heartbeat handlers are installed correctly"
      Command.Spec.empty
      main
  ;;
end

module Pipe_closing_test = struct
  type query =
    [ `Do_close
    | `Dont_close
    ]
  [@@deriving bin_io]

  let rpc =
    Pipe_rpc.create
      ()
      ~name:"pipe-closing-test"
      ~version:1
      ~bin_query
      ~bin_response:bin_unit
      ~bin_error:Nothing.bin_t
  ;;

  let main () =
    let implementations =
      Implementations.create_exn
        ~on_unknown_rpc:`Raise
        ~implementations:
          [ Pipe_rpc.implement rpc (fun () query ->
              let pipe = fst (Pipe.create ()) in
              (match query with
               | `Dont_close -> ()
               | `Do_close -> Pipe.close_read pipe);
              return (Ok pipe))
          ]
    in
    Connection.serve
      ()
      ~implementations
      ~initial_connection_state:(fun _ _ -> ())
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
    >>= fun server ->
    let port = Tcp.Server.listening_on server in
    Connection.with_client
      (Tcp.Where_to_connect.of_host_and_port { host = "127.0.0.1"; port })
      (fun conn ->
         Pipe_rpc.dispatch_exn rpc conn `Dont_close
         >>= fun (pipe, id) ->
         Pipe.close_read pipe;
         Pipe_rpc.close_reason id
         >>= fun reason ->
         assert (reason = Closed_locally);
         Pipe_rpc.dispatch_exn rpc conn `Do_close
         >>= fun (pipe, id) ->
         Pipe_rpc.close_reason id
         >>= fun reason ->
         assert (Pipe.is_closed pipe);
         assert (reason = Closed_remotely);
         Pipe_rpc.dispatch_exn rpc conn `Dont_close
         >>= fun (pipe, id) ->
         Connection.close conn
         >>= fun () ->
         Pipe_rpc.close_reason id
         >>= fun reason ->
         assert (Pipe.is_closed pipe);
         assert (
           match reason with
           | Error _ -> true
           | Closed_locally | Closed_remotely -> false);
         Deferred.unit)
    >>| Result.ok_exn
  ;;

  let command =
    Command.async_spec ~summary:"test behavior of closing pipes" Command.Spec.empty main
  ;;
end

module Pipe_iter_test = struct
  let rpc =
    Pipe_rpc.create
      ()
      ~name:"dispatch-iter-test"
      ~version:1
      ~bin_query:Time.Span.bin_t
      ~bin_response:Int.bin_t
      ~bin_error:Nothing.bin_t
  ;;

  let main () =
    let implementations =
      Implementations.create_exn
        ~on_unknown_rpc:`Raise
        ~implementations:
          [ Pipe_rpc.implement rpc (fun () query ->
              let r, w = Pipe.create () in
              don't_wait_for
                (Deferred.repeat_until_finished 0 (fun counter ->
                   if counter > 10
                   then (
                     Pipe.close w;
                     return (`Finished ()))
                   else
                     Clock.after query
                     >>| fun () ->
                     Pipe.write_without_pushback_if_open w counter;
                     `Repeat (counter + 1)));
              return (Ok r))
          ]
    in
    Connection.serve
      ()
      ~implementations
      ~initial_connection_state:(fun _ _ -> ())
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
    >>= fun server ->
    let port = Tcp.Server.listening_on server in
    Connection.with_client
      (Tcp.Where_to_connect.of_host_and_port { host = "127.0.0.1"; port })
      (fun conn ->
         let dispatch_exn query f =
           Pipe_rpc.dispatch_iter rpc conn query ~f
           >>| function
           | Error e -> Error.raise e
           | Ok (Error nothing) -> Nothing.unreachable_code nothing
           | Ok (Ok id) -> id
         in
         let next_expected : [ `Update of int | `Closed_remotely ] ref =
           ref (`Update 0)
         in
         let finished = Ivar.create () in
         dispatch_exn Time.Span.millisecond (function
           | Update n ->
             (match !next_expected with
              | `Update n' ->
                assert (n = n');
                next_expected := if n = 10 then `Closed_remotely else `Update (n + 1);
                Continue
              | `Closed_remotely -> assert false)
           | Closed `By_remote_side ->
             (match !next_expected with
              | `Update _ -> assert false
              | `Closed_remotely ->
                Ivar.fill finished ();
                Continue)
           | Closed (`Error e) -> Error.raise e)
         >>= fun (_ : Pipe_rpc.Id.t) ->
         Ivar.read finished
         >>= fun () ->
         let finished = Ivar.create () in
         dispatch_exn Time.Span.second (function
           | Update _ -> assert false
           | Closed `By_remote_side ->
             Ivar.fill finished ();
             Continue
           | Closed (`Error e) -> Error.raise e)
         >>= fun id ->
         Pipe_rpc.abort rpc conn id;
         Ivar.read finished
         >>= fun () ->
         let finished = Ivar.create () in
         dispatch_exn Time.Span.second (function
           | Update _ | Closed `By_remote_side -> assert false
           | Closed (`Error _) ->
             Ivar.fill finished ();
             Continue)
         >>= fun (_ : Pipe_rpc.Id.t) ->
         Connection.close conn >>= fun () -> Ivar.read finished)
    >>| Result.ok_exn
  ;;

  let command =
    Command.async_spec ~summary:"test behavior of dispatch_iter" Command.Spec.empty main
  ;;
end

module Pipe_direct_test = struct
  let rpc =
    Pipe_rpc.create
      ~name:"test-pipe-direct"
      ~version:1
      ~bin_query:[%bin_type_class: [ `Close | `Expect_auto_close of int ]]
      ~bin_response:Int.bin_t
      ~bin_error:Nothing.bin_t
      ()
  ;;

  let main () =
    let auto_close_was_ok : bool Ivar.t array = [| Ivar.create (); Ivar.create () |] in
    let output = List.init 10 ~f:Fn.id in
    let impl =
      Pipe_rpc.implement_direct rpc (fun () action writer ->
        List.iter output ~f:(fun i ->
          match Pipe_rpc.Direct_stream_writer.write_without_pushback writer i with
          | `Ok -> ()
          | `Closed -> assert false);
        (match action with
         | `Close -> Pipe_rpc.Direct_stream_writer.close writer
         | `Expect_auto_close n ->
           let ivar = auto_close_was_ok.(n) in
           upon (Clock.after Time.Span.second) (fun () -> Ivar.fill_if_empty ivar false);
           upon (Pipe_rpc.Direct_stream_writer.closed writer) (fun () ->
             Ivar.fill_if_empty ivar true));
        return (Ok ()))
    in
    let implementations =
      Implementations.create_exn ~implementations:[ impl ] ~on_unknown_rpc:`Raise
    in
    Connection.serve
      ~implementations
      ~initial_connection_state:(fun _ _ -> ())
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      ()
    >>= fun server ->
    let port = Tcp.Server.listening_on server in
    Connection.with_client
      (Tcp.Where_to_connect.of_host_and_port { host = "127.0.0.1"; port })
      (fun conn ->
         Pipe_rpc.dispatch_exn rpc conn `Close
         >>= fun (pipe, md) ->
         Pipe.to_list pipe
         >>= fun l ->
         [%test_result: int list] l ~expect:output;
         Pipe_rpc.close_reason md
         >>= fun reason ->
         assert (reason = Closed_remotely);
         Pipe_rpc.dispatch_exn rpc conn (`Expect_auto_close 0)
         >>= fun (pipe, md) ->
         Pipe.read_exactly pipe ~num_values:10
         >>= fun result ->
         let l =
           match result with
           | `Eof | `Fewer _ -> assert false
           | `Exactly q -> Queue.to_list q
         in
         Pipe.close_read pipe;
         [%test_result: int list] l ~expect:output;
         Pipe_rpc.close_reason md
         >>= fun reason ->
         assert (reason = Closed_locally);
         Ivar.read auto_close_was_ok.(0)
         >>= fun was_ok ->
         assert was_ok;
         Pipe_rpc.dispatch_exn rpc conn (`Expect_auto_close 1)
         >>= fun (pipe, md) ->
         Connection.close conn
         >>= fun () ->
         Pipe.to_list pipe
         >>= fun l ->
         [%test_result: int list] l ~expect:output;
         Ivar.read auto_close_was_ok.(1)
         >>= fun was_ok ->
         assert was_ok;
         Pipe_rpc.close_reason md
         >>| function
         | Error _ -> ()
         | Closed_locally | Closed_remotely -> assert false)
    >>| Result.ok_exn
  ;;

  let command =
    Command.async_spec
      ~summary:"test behavior of implement_direct"
      Command.Spec.empty
      main
  ;;
end

module Pipe_rpc_performance_measurements = struct
  module Protocol = struct
    type query = float [@@deriving bin_io]
    type response = unit [@@deriving bin_io]

    let rpc =
      Pipe_rpc.create
        ~client_pushes_back:()
        ~name:"test-rpc-performance"
        ~version:1
        ~bin_query
        ~bin_response
        ~bin_error:Nothing.bin_t
        ()
    ;;
  end

  module Client = struct
    let main msgs_per_sec host port ~rpc_impl () =
      Rpc_impl.make_client rpc_impl host port
      >>| Result.ok_exn
      >>= fun connection ->
      Pipe_rpc.dispatch Protocol.rpc connection msgs_per_sec
      >>| Or_error.ok_exn
      >>= function
      | Error t -> Nothing.unreachable_code t
      | Ok (pipe, _) ->
        let cnt = ref 0 in
        let total_cnt = ref 0 in
        let ratio_acc = ref 0. in
        let percentage_acc = ref 0. in
        let sample_to_collect_and_exit = ref (-5) in
        don't_wait_for
          (Pipe.iter_without_pushback (Cpu_usage.samples ()) ~f:(fun percent ->
             let percentage = Percent.to_percentage percent in
             incr sample_to_collect_and_exit;
             if percentage > 100.
             then (
               Print.printf "CPU pegged (%f). This test is not good.\n" percentage;
               Shutdown.shutdown 1);
             if !sample_to_collect_and_exit = 10
             then (
               Print.printf
                 "%f (cpu: %f)\n"
                 (!ratio_acc /. 10.)
                 (!percentage_acc /. 10.);
               Shutdown.shutdown 0)
             else if !sample_to_collect_and_exit >= 0
             then
               if !cnt > 0
               then (
                 let ratio = percentage *. 1_000_000. /. Int.to_float !cnt in
                 ratio_acc := !ratio_acc +. ratio;
                 percentage_acc := !percentage_acc +. percentage);
             cnt := 0));
        Pipe.iter' pipe ~f:(fun queue ->
          let len = Queue.length queue in
          cnt := !cnt + len;
          total_cnt := !total_cnt + len;
          Deferred.unit)
    ;;
  end

  module Server = struct
    let start_test ~msgs_per_sec =
      let reader, writer = Pipe.create () in
      don't_wait_for
        (run_at_limit ~dispatch:(Pipe.write_without_pushback writer) msgs_per_sec ());
      reader
    ;;

    let implementation =
      Pipe_rpc.implement Protocol.rpc (fun () msgs_per_sec ->
        return (Ok (start_test ~msgs_per_sec)))
    ;;
  end
end

module Rpc_performance_measurements = struct
  type msg = unit [@@deriving bin_io]

  let one_way = One_way.create ~name:"one-way-rpc" ~version:1 ~bin_msg

  type query = unit [@@deriving bin_io]
  type response = unit [@@deriving bin_io]

  let rpc = Rpc.create ~name:"regular-rpc" ~version:1 ~bin_query ~bin_response

  let run_client ~dispatch msgs_per_sec host port ~rpc_impl () =
    Rpc_impl.make_client rpc_impl host port
    >>| Result.ok_exn
    >>= fun connection -> run_at_limit ~dispatch:(dispatch connection) msgs_per_sec ()
  ;;

  let one_way_client msgs_per_sec host port ~rpc_impl () =
    run_client
      ~dispatch:(One_way.dispatch_exn one_way)
      msgs_per_sec
      host
      port
      ~rpc_impl
      ()
  ;;

  let rpc_client msgs_per_sec host port ~rpc_impl () =
    let dispatch connection () = don't_wait_for (Rpc.dispatch_exn rpc connection ()) in
    run_client ~dispatch msgs_per_sec host port ~rpc_impl ()
  ;;

  module Server = struct
    let cnt = ref 0
    let one_way_implementation = One_way.implement one_way (fun () () -> incr cnt)
    let rpc_implementation = Rpc.implement' rpc (fun () () -> incr cnt)

    let main port ~rpc_impl () =
      let implementations =
        Implementations.create_exn
          ~implementations:
            [ one_way_implementation
            ; rpc_implementation
            ; Pipe_rpc_performance_measurements.Server.implementation
            ]
          ~on_unknown_rpc:`Raise
      in
      Rpc_impl.make_server
        ~initial_connection_state:(fun _ -> ())
        ~implementations
        ~port
        rpc_impl
      >>= fun _server ->
      let ratio_acc = ref 0. in
      let percentage_acc = ref 0. in
      let sample = ref 0 in
      don't_wait_for
        (Pipe.iter_without_pushback (Cpu_usage.samples ()) ~f:(fun percent ->
           if 0 = !cnt
           then ()
           else (
             let percentage = Percent.to_percentage percent in
             Print.printf
               "%d %d %f (cpu: %f)\n"
               !cnt
               !sample
               (!ratio_acc /. Float.of_int !sample)
               (!percentage_acc /. Float.of_int !sample);
             if percentage > 100.
             then Print.printf "CPU pegged (%f). This test may not good.\n" percentage
             else (
               if !sample >= 0
               then
                 if !cnt > 0
                 then (
                   let ratio = percentage *. 1_000_000. /. Int.to_float !cnt in
                   ratio_acc := !ratio_acc +. ratio;
                   percentage_acc := !percentage_acc +. percentage);
               cnt := 0);
             incr sample)));
      Deferred.never ()
    ;;
  end

  let server_command =
    Command.async_spec
      ~summary:"test server for one-way and regular rpcs"
      Command.Spec.(empty +> flag "port" (required int) ~doc:"" ++ Rpc_impl.spec ())
      Server.main
  ;;

  let client_flags =
    let open Command.Spec in
    empty
    +> flag "msg-per-sec" (required float) ~doc:""
    +> flag "hostname" (required string) ~doc:""
    +> flag "port" (required int) ~doc:""
    ++ Rpc_impl.spec ()
  ;;

  let client_command =
    Command.group
      ~summary:"Clients"
      [ ( "one-way"
        , Command.async_spec
            ~summary:"client for one-way rpc"
            client_flags
            one_way_client )
      ; ( "rpc"
        , Command.async_spec ~summary:"client for regular rpc" client_flags rpc_client )
      ; ( "pipe"
        , Command.async_spec
            ~summary:"client for pipe rpc"
            client_flags
            Pipe_rpc_performance_measurements.Client.main )
      ]
  ;;
end

module Rpc_expert_test = struct
  let rpc ~name =
    Rpc.create ~name ~version:0 ~bin_query:bin_string ~bin_response:bin_string
  ;;

  (* names refer to how they're implemented *)
  let unknown_raw_rpc = rpc ~name:"unknown-raw"
  let raw_rpc = rpc ~name:"raw"
  let normal_rpc = rpc ~name:"normal"
  let custom_io_rpc_tag = "custom-io-rpc"
  let custom_io_rpc_version = 0

  let raw_one_way_rpc =
    One_way.create ~name:"raw-one-way" ~version:0 ~bin_msg:String.bin_t
  ;;

  let normal_one_way_rpc =
    One_way.create ~name:"normal-one-way" ~version:0 ~bin_msg:String.bin_t
  ;;

  let the_query = "flimflam"
  let the_response = String.rev the_query

  let main debug ~rpc_impl () =
    let level = if debug then `Debug else `Error in
    let log = Log.create ~level ~output:[ Log.Output.stdout () ] ~on_error:`Raise () in
    let one_way_reader, one_way_writer = Pipe.create () in
    let assert_one_way_rpc_received () =
      Pipe.read one_way_reader
      >>| function
      | `Eof -> assert false
      | `Ok () -> assert (Pipe.is_empty one_way_reader)
    in
    let implementations =
      let handle_raw responder buf ~pos:init_pos ~len =
        let pos_ref = ref init_pos in
        let query = String.bin_read_t buf ~pos_ref in
        [%test_result: string] query ~expect:the_query;
        Log.debug log "query value = %S" query;
        assert (!pos_ref - init_pos = len);
        let new_buf = Bin_prot.Utils.bin_dump String.bin_writer_t the_response in
        ignore
          (Rpc.Expert.Responder.schedule
             responder
             new_buf
             ~pos:0
             ~len:(Bigstring.length new_buf)
           : [ `Connection_closed | `Flushed of unit Deferred.t ])
      in
      let handle_unknown_raw () ~rpc_tag ~version responder buf ~pos ~len =
        Log.debug log "query: %s v%d" rpc_tag version;
        assert (
          rpc_tag = Rpc.name unknown_raw_rpc && version = Rpc.version unknown_raw_rpc);
        try
          handle_raw responder buf ~pos ~len;
          Deferred.unit
        with
        | e ->
          Log.debug log !"got exception: %{Exn#mach}" e;
          Rpc.Expert.Responder.write_error responder (Error.of_exn e);
          Deferred.unit
      in
      Implementations.Expert.create_exn
        ~implementations:
          [ Rpc.implement normal_rpc (fun () query ->
              [%test_result: string] query ~expect:the_query;
              return the_response)
          ; Rpc.Expert.implement' raw_rpc (fun () responder buf ~pos ~len ->
              handle_raw responder buf ~pos ~len;
              Replied)
          ; Rpc.Expert.implement_for_tag_and_version'
              ~rpc_tag:custom_io_rpc_tag
              ~version:custom_io_rpc_version
              (fun () responder buf ~pos ~len ->
                 handle_raw responder buf ~pos ~len;
                 Replied)
          ; One_way.implement normal_one_way_rpc (fun () query ->
              Log.debug log "received one-way RPC message (normal implementation)";
              Log.debug log "message value = %S" query;
              [%test_result: string] query ~expect:the_query;
              Pipe.write_without_pushback one_way_writer ())
          ; One_way.Expert.implement raw_one_way_rpc (fun () buf ~pos ~len ->
              Log.debug log "received one-way RPC message (expert implementation)";
              let pos_ref = ref pos in
              let query = String.bin_read_t buf ~pos_ref in
              Log.debug log "message value = %S" query;
              assert (!pos_ref - pos = len);
              [%test_result: string] query ~expect:the_query;
              Pipe.write_without_pushback one_way_writer ())
          ]
        ~on_unknown_rpc:(`Expert handle_unknown_raw)
    in
    Rpc_impl.make_server
      ~implementations
      ~initial_connection_state:(fun _ -> ())
      rpc_impl
    >>= fun server ->
    let port = Rpc_impl.Server.bound_on server in
    Rpc_impl.with_client rpc_impl "127.0.0.1" port (fun conn ->
      Deferred.List.iter [ unknown_raw_rpc; raw_rpc; normal_rpc ] ~f:(fun rpc ->
        Log.debug log "sending %s query normally" (Rpc.name rpc);
        Rpc.dispatch_exn rpc conn the_query
        >>= fun response ->
        Log.debug log "got response";
        [%test_result: string] response ~expect:the_response;
        let buf = Bin_prot.Utils.bin_dump String.bin_writer_t the_query in
        Log.debug log "sending %s query via Expert interface" (Rpc.name rpc);
        Deferred.create (fun i ->
          ignore
            (Rpc.Expert.schedule_dispatch
               conn
               ~rpc_tag:(Rpc.name rpc)
               ~version:(Rpc.version rpc)
               buf
               ~pos:0
               ~len:(Bigstring.length buf)
               ~handle_error:(fun e -> Ivar.fill i (Error e))
               ~handle_response:(fun buf ~pos ~len ->
                 let pos_ref = ref pos in
                 let response = String.bin_read_t buf ~pos_ref in
                 assert (!pos_ref - pos = len);
                 Ivar.fill i (Ok response);
                 Deferred.unit)
             : [ `Connection_closed | `Flushed of unit Deferred.t ]))
        >>| fun response ->
        Log.debug log "got response";
        [%test_result: string Or_error.t] response ~expect:(Ok the_response))
      >>= fun () ->
      (let buf = Bin_prot.Utils.bin_dump String.bin_writer_t the_query in
       Log.debug log "sending %s query via Expert interface" custom_io_rpc_tag;
       Deferred.create (fun i ->
         ignore
           (Rpc.Expert.schedule_dispatch
              conn
              ~rpc_tag:custom_io_rpc_tag
              ~version:custom_io_rpc_version
              buf
              ~pos:0
              ~len:(Bigstring.length buf)
              ~handle_error:(fun e -> Ivar.fill i (Error e))
              ~handle_response:(fun buf ~pos ~len ->
                let pos_ref = ref pos in
                let response = String.bin_read_t buf ~pos_ref in
                assert (!pos_ref - pos = len);
                Ivar.fill i (Ok response);
                Deferred.unit)
            : [ `Connection_closed | `Flushed of unit Deferred.t ]))
       >>| fun response ->
       Log.debug log "got response";
       [%test_result: string Or_error.t] response ~expect:(Ok the_response))
      >>= fun () ->
      Deferred.List.iter [ raw_one_way_rpc; normal_one_way_rpc ] ~f:(fun rpc ->
        Log.debug log "sending %s query normally" (One_way.name rpc);
        One_way.dispatch_exn rpc conn the_query;
        assert_one_way_rpc_received ()
        >>= fun () ->
        Log.debug log "sending %s query via Expert.dispatch" (One_way.name rpc);
        let buf = Bin_prot.Utils.bin_dump String.bin_writer_t the_query in
        let pos = 0 in
        let len = Bigstring.length buf in
        (match One_way.Expert.dispatch rpc conn buf ~pos ~len with
         | `Ok -> ()
         | `Connection_closed -> assert false);
        assert_one_way_rpc_received ()
        >>= fun () ->
        Log.debug
          log
          "sending %s query via Expert.schedule_dispatch"
          (One_way.name rpc);
        (match One_way.Expert.schedule_dispatch rpc conn buf ~pos ~len with
         | `Flushed f -> f
         | `Connection_closed -> assert false)
        >>= fun () -> assert_one_way_rpc_received ()))
    >>= fun result ->
    Result.ok_exn result;
    Rpc_impl.Server.close server
  ;;

  let command =
    Command.async_spec
      ~summary:"connect basic and low-level clients"
      Command.Spec.(empty +> flag "debug" no_arg ~doc:"" ++ Rpc_impl.spec ())
      main
  ;;
end

module Connection_closing_test = struct
  let one_way_unimplemented =
    One_way.create ~name:"unimplemented" ~version:1 ~bin_msg:bin_unit
  ;;

  let never_returns =
    Rpc.create
      ~name:"never-returns"
      ~version:1
      ~bin_query:bin_unit
      ~bin_response:bin_unit
  ;;

  let never_returns_impl = Rpc.implement never_returns (fun () () -> Deferred.never ())

  let implementations =
    Implementations.create_exn
      ~implementations:[ never_returns_impl ]
      ~on_unknown_rpc:`Continue
  ;;

  let main () =
    let most_recent_server_conn = ref None in
    Connection.serve
      ~implementations
      ~initial_connection_state:(fun _ conn -> most_recent_server_conn := Some conn)
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      ()
    >>= fun server ->
    let port = Tcp.Server.listening_on server in
    let connect () =
      Connection.client
        (Tcp.Where_to_connect.of_host_and_port { host = "127.0.0.1"; port })
      >>| Result.ok_exn
    in
    let dispatch_never_returns conn =
      let response = Rpc.dispatch never_returns conn () in
      Clock.after Time.Span.second
      >>= fun () ->
      assert (not (Deferred.is_determined response));
      return response
    in
    let check_response_is_error here conn response_deferred =
      Clock.with_timeout Time.Span.second (Connection.close_finished conn)
      >>= function
      | `Timeout ->
        failwithf
          !"%{Source_code_position} timed out waiting for connection to close"
          here
          ()
      | `Result () ->
        Clock.with_timeout Time.Span.second response_deferred
        >>| (function
          | `Timeout ->
            failwithf
              !"%{Source_code_position} timed out waiting for response to be determined"
              here
              ()
          | `Result (Ok ()) ->
            failwithf !"%{Source_code_position} somehow got an ok response for RPC" here ()
          | `Result (Error _) -> ())
    in
    (* Kill the connection after dispatching the RPC. *)
    connect ()
    >>= fun conn ->
    dispatch_never_returns conn
    >>= fun response_deferred ->
    let server_conn = Option.value_exn ~here:[%here] !most_recent_server_conn in
    Connection.close server_conn
    >>= fun () ->
    check_response_is_error [%here] conn response_deferred
    >>= fun () ->
    (* Call an unknown one-way RPC while the connection is open. This causes somewhat
       strange but not problematic behavior -- the server sends back an "unknown RPC"
       message, but the client doesn't have a response handler installed, so it closes the
       connection. *)
    connect ()
    >>= fun conn ->
    dispatch_never_returns conn
    >>= fun response_deferred ->
    One_way.dispatch_exn one_way_unimplemented conn ();
    check_response_is_error [%here] conn response_deferred
  ;;

  let command =
    Command.async
      ~summary:"test that responses are determined when connections are closed"
      (Command.Param.return main)
  ;;
end

let all_regression_tests =
  Command.async_spec
    ~summary:"run all regression tests"
    Command.Spec.(empty +> flag "debug" no_arg ~doc:"" ++ Rpc_impl.spec ())
    (fun debug ~rpc_impl () ->
       Heartbeat_pipe_test.main ()
       >>= fun () ->
       Pipe_closing_test.main ()
       >>= fun () ->
       Pipe_iter_test.main ()
       >>= fun () ->
       Pipe_direct_test.main ()
       >>= fun () ->
       Rpc_expert_test.main debug ~rpc_impl ()
       >>= fun () -> Connection_closing_test.main ())
;;

let () =
  Command.run
    (Command.group
       ~summary:"Various tests for rpcs"
       [ ( "performance"
         , Command.group
             ~summary:"Plain rpc performance test"
             [ "server", Rpc_performance_measurements.server_command
             ; "client", Rpc_performance_measurements.client_command
             ] )
       ; ( "pipe"
         , Command.group
             ~summary:"Pipe rpc"
             [ "simple", Pipe_simple_test.command
             ; "closing", Pipe_closing_test.command
             ; "iter", Pipe_iter_test.command
             ; "direct", Pipe_direct_test.command
             ] )
       ; ( "expert"
         , Command.group
             ~summary:"Testing Expert interfaces"
             [ "test", Rpc_expert_test.command ] )
       ; ( "heartbeat"
         , Command.group
             ~summary:"Testing heartbeats"
             [ "test-heartbeat-callback", Heartbeat_pipe_test.command ] )
       ; "regression", all_regression_tests
       ; "connection-inspector", Rpc_connection_inspector.command
       ; "connection-close", Connection_closing_test.command
       ])
;;
