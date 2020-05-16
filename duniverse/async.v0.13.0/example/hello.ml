open Core
open Async

let handler _ reader writer =
  Deferred.create (fun i ->
    let write () =
      Writer.write
        writer
        "HTTP/1.1 200 OK\nContent-length: 12\nContent-type: text/plain\n\nHello World!"
    in
    let rec read () =
      Reader.read_line reader
      >>> function
        (* Blows up horribly if it's a POST (or anything with Content-length /= 0). *)
      | `Ok "" ->
        write ();
        read ()
      | `Eof ->
        write ();
        Ivar.fill i ()
      | _ -> read ()
    in
    read ())
;;

let () =
  ignore
    (Tcp.Server.create
       (Tcp.Where_to_listen.of_port 55_555)
       ~on_handler_error:`Ignore
       handler
     : Tcp.Server.inet Deferred.t)
;;

let () = never_returns (Scheduler.go ())

let _dont_run_this () =
  (* To be happy that abstracting [never_returns] into module [Common0] is ok, make a type
     check that the [never_returns] type matches between [Exn.handle_uncaught_and_exit]
     and [never_returns]. *)
  never_returns (Exn.handle_uncaught_and_exit Scheduler.go)
;;
