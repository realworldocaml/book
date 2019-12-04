open Core
open Async
module Fd = Unix.Fd

let cat ~input ~output =
  let reader = Reader.create input in
  let writer = Writer.create ~raise_when_consumer_leaves:false output in
  let buf = Bytes.create 4096 in
  let rec loop () =
    choose
      [ choice (Reader.read reader buf) (fun r -> `Reader r)
      ; choice (Writer.consumer_left writer) (fun () -> `Epipe)
      ]
    >>> function
    | `Epipe -> shutdown 0
    | `Reader r ->
      (match r with
       | `Eof -> Writer.flushed writer >>> fun _ -> shutdown 0
       | `Ok len ->
         Writer.write_substring writer (Substring.create buf ~pos:0 ~len);
         loop ())
  in
  loop ()
;;

let () = cat ~input:(Fd.stdin ()) ~output:(Fd.stdout ())
let () = never_returns (Scheduler.go ())
