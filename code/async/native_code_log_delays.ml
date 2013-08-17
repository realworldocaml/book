open Core.Std
open Async.Std

let log_delays thunk =
  let start = Time.now () in
  let print_time () =
    let diff = Time.diff (Time.now ()) start in
    printf "%s, " (Time.Span.to_string diff)
  in
  let d = thunk () in
  Clock.every (sec 0.1) ~stop:d print_time;
  d >>| fun () -> print_time (); printf "\n"

let noalloc_busyloop () =
  for _i = 1 to 25_000_000_000 do () done;
  Deferred.unit

let () =
  Command.async_basic
    ~summary:"run logger without busy loop"
    Command.Spec.(empty)
    (fun () -> log_delays noalloc_busyloop)
  |> Command.run
