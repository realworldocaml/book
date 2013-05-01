open Core.Std
open Async.Std

(** Reads line-by-line from the provided reader, writing each line to the
    writer as it goes*)
let rec copy_lines reader writer =
  Reader.read_line reader
  >>= function
  | `Eof -> return ()
  | `Ok line ->
    Writer.write writer line;
    Writer.write writer "\n";
    Writer.flushed writer
    >>= fun () ->
    copy_lines reader writer

(** Starts a TCP server, which listens on the specified port, invoking
    copy_lines every time a client connects. *)
let run () =
  let server =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 8765)
      (fun _addr reader writer -> copy_lines reader writer)
  in
  ignore (server : (_,_) Tcp.Server.t Deferred.t)

(* Call [run], and then start the scheduler *)
let () =
  run ();
  never_returns (Scheduler.go ())
