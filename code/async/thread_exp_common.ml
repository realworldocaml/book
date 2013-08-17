open Core.Std
open Async.Std

let log_delays d =
    let start = Time.now () in
    let rec loop stamps =
      let delay = Time.diff (Time.now ()) start in
      match Deferred.peek d with
      | Some () -> return (delay :: stamps)
      | None ->
        after (sec 0.1)
        >>= fun () ->
        loop (delay :: stamps)
    in
    loop []
    >>| fun delays ->
    let sexp = <:sexp_of<Time.Span.t list>> (List.rev delays) in
    printf "%s\n" (Sexp.to_string sexp)

let finish () =
  shutdown 0;
  never_returns (Scheduler.go ())
