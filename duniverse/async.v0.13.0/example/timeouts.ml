open Core
open Async

let () =
  (* Memory usage of this program stays low, demonstrating that [with_timeout span d]
     doesn't continue to use memory for the timeout after [d] becomes determined. *)
  Command.async_spec ~summary:"Run a whole buncha timeouts" Command.Spec.empty (fun () ->
    Deferred.repeat_until_finished 100_000_000 (fun n ->
      Clock.with_timeout (sec 60.) (Deferred.return ())
      >>| function
      | `Result () -> if n = 0 then `Finished () else `Repeat (n - 1)
      | `Timeout -> failwith "Timeout"))
  |> Command.run
;;
