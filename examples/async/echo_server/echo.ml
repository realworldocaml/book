open Core.Std
open Async.Std

let run () =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 8765)
      (fun _addr reader writer ->
        let rec read_loop () =
          Reader.read_line reader
          >>= function
          | `Eof -> return ()
          | `Ok line ->
            Writer.write writer line;
            Writer.write writer "\n";
            Writer.flushed writer
            >>= fun () ->
            read_loop ()
        in
        read_loop ()
      )
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()

let () =
  don't_wait_for (run ());
  never_returns (Scheduler.go ())
