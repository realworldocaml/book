open Core.Std
open Async.Std


let run () =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 8765)
      (fun _addr reader writer ->
        Pipe.iter' (Reader.pipe reader) ~f:(fun q ->
          Queue.iter q ~f:(fun s -> Writer.write writer s);
          Writer.flushed writer)
      )
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()

let () =
  don't_wait_for (run ());
  never_returns (Scheduler.go ())
