open! Core
open Jane
open! Async
open Rpc

let () =
  if Time.Span.( > ) (Scheduler.event_precision ()) (Time.Span.of_us 1.)
  then (
    Core.eprintf
      {|%s: you need to run this program with:

  ASYNC_CONFIG='((timing_wheel_config ((alarm_precision 1us))))'

Otherwise the async timing wheel won't be precise enough.
|}
      (Filename.basename Sys.executable_name);
    Caml.exit 2)
;;

module Spec = struct
  open Command.Spec

  let port () =
    step (fun main port -> main ~port)
    +> flag "-port" (optional_with_default 12345 int) ~doc:" RPC server port"
  ;;

  let host () =
    step (fun main host -> main ~host)
    +> flag "-host" (required string) ~doc:" RPC server hostname"
  ;;
end

module Stats = struct
  type t =
    { mutable count : int
    ; mutable total : Time_ns.Span.t
    ; mutable max : Time_ns.Span.t
    ; mutable min : Time_ns.Span.t
    ; mutable count_by_us : int array
    }

  let create () =
    { count = 0
    ; total = Time_ns.Span.zero
    ; max = Time_ns.Span.zero
    ; min = Time_ns.Span.max_value_for_1us_rounding
    ; count_by_us = Array.create 0 ~len:1000
    }
  ;;

  let add t ~span =
    t.count <- t.count + 1;
    t.total <- Time_ns.Span.( + ) t.total span;
    if Time_ns.Span.( < ) span t.min then t.min <- span;
    if Time_ns.Span.( > ) span t.max then t.max <- span;
    let us = Time_ns.Span.to_int_ns span / 1000 in
    if us < Array.length t.count_by_us then t.count_by_us.(us) <- t.count_by_us.(us) + 1
  ;;

  let print t =
    let open Core in
    let rows, cols =
      try ok_exn Linux_ext.get_terminal_size `Controlling with
      | _ -> 80, 25
    in
    let histogram_height = rows - 8 in
    let start =
      let rec search i =
        if i >= Array.length t.count_by_us - cols || t.count_by_us.(i) > 0
        then i / 10 * 10
        else search (i + 1)
      in
      search 0
    in
    let cols = Int.min (Array.length t.count_by_us - start) cols in
    let count_by_us = Array.sub t.count_by_us ~pos:start ~len:cols in
    let count_displayed = Array.fold count_by_us ~init:0 ~f:( + ) in
    if histogram_height > 0 && count_displayed > 0
    then (
      let displayed_max = Array.fold count_by_us ~init:0 ~f:Int.max in
      let histo =
        Array.map count_by_us ~f:(fun n -> n * histogram_height / displayed_max)
      in
      let lines =
        Array.init histogram_height ~f:(fun row ->
          String.init cols ~f:(fun col -> if row < histo.(col) then '#' else ' '))
      in
      for i = histogram_height - 1 downto 0 do
        print_string lines.(i);
        Out_channel.output_char Out_channel.stdout '\n'
      done;
      let col = ref 0 in
      while !col < cols do
        let next = !col + 10 in
        let s = Int.to_string (start + !col) in
        let len = String.length s in
        if !col + len <= cols
        then (
          print_string s;
          print_string (String.make (10 - len) ' '));
        col := next
      done;
      print_string "\n\n");
    printf
      "Displayed: %d%% (%d/%d)\n"
      (count_displayed * 100 / t.count)
      count_displayed
      t.count;
    printf !"Min      : %{Time_ns.Span}\n" t.min;
    printf !"Max      : %{Time_ns.Span}\n" t.max;
    printf !"Mean     : %{Time_ns.Span}\n" (Time_ns.Span.( / ) t.total (float t.count))
  ;;
end

module Protocol = struct
  type query = string [@@deriving bin_io]
  type response = string [@@deriving bin_io]

  let rpc = Rpc.create ~name:"test-rpc-latency" ~version:1 ~bin_query ~bin_response

  module Quit = struct
    type query = unit [@@deriving bin_io]
    type response = unit [@@deriving bin_io]

    let rpc =
      Rpc.create ~name:"test-rpc-latency-quit" ~version:1 ~bin_query ~bin_response
    ;;
  end
end

module Client = struct
  let one_call ~connection ~stats ~record ~start ~payload =
    Rpc.dispatch_exn Protocol.rpc connection payload
    >>> fun _ ->
    let stop = Time_ns.now () in
    let span = Time_ns.diff stop start in
    if !record then Stats.add stats ~span
  ;;

  let rec loop ~connection ~stats ~record ~span_between_call ~payload =
    let start = Time_ns.now () in
    one_call ~connection ~stats ~record ~start ~payload;
    let now = Time_ns.now () in
    let stop = Time_ns.add start span_between_call in
    let span = Time_ns.diff stop now in
    (if Time_ns.Span.( <= ) span Time_ns.Span.zero
     then Scheduler.yield ()
     else Clock_ns.after span)
    >>> fun () -> loop ~connection ~stats ~record ~span_between_call ~payload
  ;;

  let main msgs_per_sec msg_size ~host ~port ~rpc_impl () =
    let payload = String.make msg_size '\000' in
    Rpc_impl.make_client rpc_impl host port
    >>| Result.ok_exn
    >>= fun connection ->
    let stats = Stats.create () in
    let record = ref false in
    loop
      ~connection
      ~stats
      ~record
      ~span_between_call:(Time_ns.Span.of_int_ns (1_000_000_000 / msgs_per_sec))
      ~payload;
    Clock.after (sec 1.)
    >>= fun () ->
    record := true;
    Clock.after (sec 5.) >>| fun () -> Stats.print stats
  ;;
end

module Client_long = struct
  let pending = ref 0
  let running = ref false
  let record = ref false

  let one_call ~connection ~stats ~start ~payload =
    incr pending;
    Rpc.dispatch_exn Protocol.rpc connection payload
    >>> fun _ ->
    decr pending;
    let stop = Time_ns.now () in
    let span = Time_ns.diff stop start in
    let span = float (Time_ns.Span.to_int_ns span / 1000) in
    if !record then Rstats.update_in_place stats span
  ;;

  let rec loop ~connection ~stats ~record ~span_between_call ~payload =
    if !running
    then (
      let start = Time_ns.now () in
      one_call ~connection ~stats ~start ~payload;
      let now = Time_ns.now () in
      let stop = Time_ns.add start span_between_call in
      let span = Time_ns.diff stop now in
      (if Time_ns.Span.( <= ) span Time_ns.Span.zero
       then Scheduler.yield ()
       else Clock_ns.after span)
      >>> fun () -> loop ~connection ~stats ~record ~span_between_call ~payload)
  ;;

  let rec wait_for_pending () =
    if !pending > 0 then Clock.after (sec 0.01) >>= wait_for_pending else Deferred.unit
  ;;

  let try_one csv_oc ~connection ~msgs_per_sec ~payload =
    let stats = Rstats.create () in
    record := false;
    running := true;
    assert (!pending = 0);
    loop
      ~connection
      ~stats
      ~record
      ~span_between_call:(Time_ns.Span.of_int_ns (1_000_000_000 / msgs_per_sec))
      ~payload;
    Clock.after (sec 5.)
    >>= fun () ->
    record := true;
    Clock.after (sec 10.)
    >>= fun () ->
    running := false;
    record := false;
    wait_for_pending ()
    >>| fun () ->
    let real_msgs_per_sec = float (Rstats.samples stats) /. 10. in
    Core.printf
      {|msgs/s : %f
mean   : %f us
stdev  : %f us
min    : %f us
max    : %f us

%!|}
      real_msgs_per_sec
      (Rstats.mean stats)
      (Rstats.stdev stats)
      (Rstats.min stats)
      (Rstats.max stats);
    Core.fprintf
      csv_oc
      "%f,%f,%f\n"
      real_msgs_per_sec
      (Rstats.mean stats)
      (Rstats.stdev stats)
  ;;

  let main csv_prefix msg_size ~host ~port ~rpc_impl () =
    let payload = String.make msg_size '\000' in
    Rpc_impl.make_client rpc_impl host port
    >>| Result.ok_exn
    >>= fun connection ->
    let csv_oc = ksprintf Core.Out_channel.create "out-%s-%d.csv" csv_prefix msg_size in
    Core.fprintf csv_oc "msgs/s,mean,stdev\n";
    Deferred.List.iter
      [ 100
      ; 200
      ; 300
      ; 400
      ; 500
      ; 600
      ; 700
      ; 800
      ; 900
      ; 1000
      ; 10_000
      ; 20_000
      ; 30_000
      ; 40_000
      ; 50_000
      ; 60_000
      ; 70_000
      ; 80_000
      ; 90_000
      ; 100_000
      ; 110_000
      ; 120_000
      ; 130_000
      ; 140_000
      ; 150_000
      ; 200_000
      ; 300_000
      ; 400_000
      ; 500_000
      ; 600_000
      ; 700_000
      ; 800_000
      ; 900_000
      ; 1_000_000
      ; 1_500_000
      ; 2_000_000
      ]
      ~f:(fun msgs_per_sec -> try_one csv_oc ~connection ~msgs_per_sec ~payload)
    >>| fun () -> Core.Out_channel.close csv_oc
  ;;
end

module Server = struct
  let implementations =
    [ Rpc.implement' Protocol.rpc (fun () s -> s)
    ; Rpc.implement' Protocol.Quit.rpc (fun () () ->
        Clock.after (sec 1.0) >>> fun () -> shutdown 0)
    ]
  ;;

  let main ~port ~rpc_impl () =
    let implementations =
      Implementations.create_exn ~implementations ~on_unknown_rpc:`Raise
    in
    Rpc_impl.make_server
      ~initial_connection_state:(fun _ -> ())
      ~implementations
      ~port
      rpc_impl
    >>= fun _server -> Deferred.never ()
  ;;
end

module Quit = struct
  let main ~host ~port ~rpc_impl () =
    Rpc_impl.make_client rpc_impl host port
    >>| Result.ok_exn
    >>= fun connection ->
    Rpc.dispatch_exn Protocol.Quit.rpc connection () >>| fun () -> shutdown 0
  ;;
end

let server_command =
  Command.async_spec
    ~summary:"test server"
    Command.Spec.(empty ++ Spec.port () ++ Rpc_impl.spec ())
    Server.main
;;

let client_command =
  Command.async_spec
    ~summary:"test client"
    Command.Spec.(
      empty
      +> flag "msg-per-sec" (required int) ~doc:""
      +> flag "msg-size" (optional_with_default 0 int) ~doc:" message size"
      ++ Spec.host ()
      ++ Spec.port ()
      ++ Rpc_impl.spec ())
    Client.main
;;

let client_long_command =
  Command.async_spec
    ~summary:"long test client"
    Command.Spec.(
      empty
      +> flag "csv-prefix" (required string) ~doc:""
      +> flag "msg-size" (optional_with_default 0 int) ~doc:" message size"
      ++ Spec.host ()
      ++ Spec.port ()
      ++ Rpc_impl.spec ())
    Client_long.main
;;

let quit_command =
  Command.async_spec
    ~summary:"test quit"
    Command.Spec.(empty ++ Spec.host () ++ Spec.port () ++ Rpc_impl.spec ())
    Quit.main
;;

let () =
  Command.run
    (Command.group
       ~summary:"Async RPC latency test"
       [ "server", server_command
       ; "client", client_command
       ; "client-long", client_long_command
       ; "quit-server", quit_command
       ])
;;
