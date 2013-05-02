open Core.Std
open Async.Std

(* Copy data from the reader to the writer, using the provided buffer
   as scratch space *)
let rec copy_blocks buffer r w =
  Reader.read r buffer
  >>= function
  | `Eof -> return ()
  | `Ok bytes_read ->
    Writer.write w buffer ~len:bytes_read;
    Writer.flushed w
    >>= fun () ->
    copy_blocks buffer r w

(** Starts a TCP server, which listens on the specified port, invoking
    copy_lines every time a client connects. *)
let run () =
  let buffer = String.create (16 * 1024) in
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 8765)
      (fun _addr r w -> copy_blocks buffer r w)
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

(* Call [run], and then start the scheduler *)
let () =
  run ();
  never_returns (Scheduler.go ())
