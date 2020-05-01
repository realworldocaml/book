open Core
open Async

(* Strictly a memory test. *)
(* Equals zero: printf "job_queue_length = %d\n" (Scheduler.job_queue_length ()) *)

let rec loop = function
  | 0 ->
    Gc.print_stat stdout;
    Shutdown.shutdown 0;
    Deferred.unit
  | n -> Deferred.unit >>= fun () -> loop (n - 1)
;;

let () = ignore (loop 2000000)
let () = never_returns (Scheduler.go ())
