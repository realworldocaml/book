open Core.Std
open Async.Std
open Thread_exp_common


(* part 1 *)
let busy_loop n =
  let x = ref None in
  for i = 1 to 100_000_000 do x := Some i done

let () =
  don't_wait_for
    (log_delays (Deferred.unit >>= busy_loop))

(* part 2 *)
let () = finish ()



