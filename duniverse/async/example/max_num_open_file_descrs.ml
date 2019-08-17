open! Core
open! Async

let () =
  print_s
    [%message "" ~max_num_open_file_descrs:(Scheduler.max_num_open_file_descrs () : int)];
  shutdown 0;
  never_returns (Scheduler.go ())
;;
