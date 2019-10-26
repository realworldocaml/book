open Core
open Async

let printf = Print.printf

let () =
  let m2 = Monitor.create ~name:"test monitor 2" () in
  Stream.iter (Monitor.detach_and_get_error_stream m2) ~f:(fun _ ->
    printf "caught error\n");
  schedule ~monitor:m2 (fun () ->
    let m1 = Monitor.create ~name:"test monitor 1" () in
    Stream.iter
      (Clock.at_intervals (sec 1.0))
      ~f:(fun _ ->
        try failwith "error!" with
        | _ -> Monitor.send_exn m1 (Failure "error!") ~backtrace:`Get));
  never_returns (Scheduler.go ())
;;
