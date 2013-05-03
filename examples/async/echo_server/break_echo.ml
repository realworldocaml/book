open Core.Std
open Async.Std

let run ~host ~port =
  Tcp.with_connection
    (Tcp.to_host_and_port host port)
    (fun _socket _r w ->
      Writer.write w (String.create (2000 * 1024));
      Clock.after (sec 0.1)
      >>= fun () ->
      exit ~force:(return ()) (-1)
    )

let () =
  don't_wait_for (run ~host:"localhost" ~port:8765);
  never_returns (Scheduler.go ())
