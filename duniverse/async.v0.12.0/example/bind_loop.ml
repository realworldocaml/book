open Core
open Async

let bind_loop () =
  let rec loop n =
    if n = 0 then return 123 else Deferred.unit >>= fun () -> loop (n - 1)
  in
  loop 15_000_000
;;

let main () =
  bind_loop ()
  >>> fun x ->
  Print.printf "done %d\n" x;
  Shutdown.shutdown 0
;;

let () =
  main ();
  Exn.handle_uncaught ~exit:true (never_returns (Scheduler.go ()))
;;
