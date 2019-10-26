open Core
open Async

let () =
  upon
    (Unix.openfile "/tmp/z.foo" ~mode:[ `Creat; `Wronly ] ~perm:0o0666)
    (fun fd ->
       let writer = Writer.create fd in
       let buf = String.make 1_048_576 ' ' in
       let rec loop i =
         eprintf "about to write %d\n" i;
         Writer.write writer buf;
         upon (Clock.after (sec 1.)) (fun () -> loop (i + 1))
       in
       loop 0)
;;

let () = never_returns (Scheduler.go ())
