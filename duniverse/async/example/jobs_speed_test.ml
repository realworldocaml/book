open Core
open Async

let num_jobs = 4096
let num_iters = 4096

let run_test () =
  Deferred.create (fun i ->
    let rec loop n =
      if n = 0
      then Ivar.fill i ()
      else
        Deferred.create (fun i ->
          let finished = ref num_jobs in
          let rec loop n =
            if n > 0
            then (
              upon Deferred.unit (fun () ->
                decr finished;
                if !finished = 0 then Ivar.fill i ());
              loop (n - 1))
          in
          loop num_jobs)
        >>> fun () -> loop (n - 1)
    in
    loop num_iters)
;;

let () =
  let start = Time.now () in
  upon (run_test ()) (fun () ->
    let stop = Time.now () in
    printf "elapsed time: %s\n" (Time.Span.to_string (Time.diff stop start));
    Shutdown.shutdown 0);
  never_returns (Scheduler.go ())
;;

(* jobs_per_cycle
   = 2000, time = 9.3s
   = 1200, time = 10.24s
   = 1100, time = 10.25s
   = 1000, time = 10.30s
   = 900, time  = 10.3s
   = 800, time = 9.7s
   = 700, time = 9.7s
   = 600, time = 10.3s
   = 500, time = 10.5s
   = 400, time = 10.61
   = 100, time = 11.1s
   = 10, time = 16.5s *)
