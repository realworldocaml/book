open Core.Std
open Async.Std
open Thread_exp_common

let () =
  Command.async_basic
    ~summary:"run logger without busy loop"
    Command.Spec.(empty)
    (fun () ->
       log_delays (after (sec 1.))
    )
  |> Command.run
