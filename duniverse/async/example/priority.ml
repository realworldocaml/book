open Core
open Async

let printf = Print.printf

module Priority = Async.Priority

(* to avoid omake confusion *)

let normal = Priority.normal
let low = Priority.low

let one name priority =
  Scheduler.schedule ~priority (fun () ->
    upon Deferred.unit (fun () ->
      let rec loop i =
        if i > 0
        then (
          printf "%s %d\n" name i;
          upon Deferred.unit (fun () -> loop (i - 1)))
      in
      loop 10))
;;

let () = one "low" low
let () = one "normal" normal
let () = upon (Clock.after (sec 1.)) (fun () -> shutdown 0)
let () = never_returns (Scheduler.go ())
