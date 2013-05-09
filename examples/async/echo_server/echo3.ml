open Core.Std
open Async.Std


let run () =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 8765)
      (fun _addr reader writer ->
        let buf = Bigstring.create (16 * 1024) in
        let sub = Bigsubstring.create buf in
        let rec read_loop () =
          Reader.read_bigsubstring reader sub
          >>= function
          | `Eof -> return ()
          | `Ok chars_read ->
            Writer.schedule_bigstring writer buf ~len:chars_read;
            Writer.flushed writer
            >>= fun () -> read_loop ()
        in
        read_loop ()
      )
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()

let () =
  don't_wait_for (run ());
  never_returns (Scheduler.go ())
