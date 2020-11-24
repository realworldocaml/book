open! Core
open Async

let () =
  don't_wait_for
    (after (sec 60.)
     >>= fun () ->
     (* At this point, we enqueue enough jobs to fully utilize the thread pool.  However,
        it will only be stuck for a small amount of time, since it was idle for the prior
        60s. *)
     Deferred.List.init ~how:`Parallel 50 ~f:(fun _ ->
       In_thread.run (fun () -> Core.Unix.sleep 2))
     >>= fun _ ->
     shutdown 0;
     return ())
;;

let () = never_returns (Scheduler.go ())
