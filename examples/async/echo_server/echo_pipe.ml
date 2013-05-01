open Core.Std
open Async.Std


let run () =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 8765)
      (fun _addr r w ->
        Pipe.transfer_id (Reader.pipe r) (Writer.pipe w)
      )
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()

let () =
  don't_wait_for (run ());
  never_returns (Scheduler.go ())
