open Core
open Async

let printf = Print.printf
let () = printf "my pid is %s\n" (Pid.to_string (Unix.getpid ()))

let () =
  Signal.handle Signal.terminating ~f:(fun signal ->
    printf "%s\n" (Signal.to_string signal))
;;

let () = never_returns (Scheduler.go ())
