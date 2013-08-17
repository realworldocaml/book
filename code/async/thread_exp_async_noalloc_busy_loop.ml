open Core.Std
open Async.Std
open Thread_exp_common


(* part 1 *)
let busy_loop () =
  for i = 1 to 200_000_000 do () done

let () =
  don't_wait_for
    (log_delays (In_thread.run busy_loop))

(* part 2 *)
let () = finish ()



