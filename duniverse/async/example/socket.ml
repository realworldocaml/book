open Core
open Async
module Socket = Unix.Socket

let printf = Print.printf
let port = 10_000

let doit () =
  let s =
    Socket.bind_inet
      (Socket.create Socket.Type.tcp)
      (Socket.Address.Inet.create_bind_any ~port)
  in
  don't_wait_for
    (match%map Socket.accept (Socket.listen s) with
     | `Socket_closed -> ()
     | `Ok (s, _) ->
       let buf = Bytes.create 1000 in
       let reader = Reader.create (Socket.fd s) in
       let rec loop bytes_read =
         upon (Reader.read reader buf) (function
           | `Eof -> printf "EOF\n"
           | `Ok n ->
             let bytes_read = bytes_read + n in
             printf "read %d bytes in total.\n" bytes_read;
             loop bytes_read)
       in
       loop 0);
  upon
    (Clock.after (sec 2.))
    (fun () ->
       let s = Socket.create Socket.Type.tcp in
       upon
         (Socket.connect
            s
            (Socket.Address.Inet.create (Unix.Inet_addr.of_string "127.0.0.1") ~port))
         (fun s ->
            let w = Writer.create (Socket.fd s) in
            let buf = String.make 4096 ' ' in
            let rec loop bytes_written =
              Writer.write w buf;
              upon (Writer.flushed w) (fun _ ->
                let bytes_written = bytes_written + String.length buf in
                printf "wrote %d bytes in total.\n" bytes_written;
                loop bytes_written)
            in
            loop 0))
;;

let () = doit ()
let () = never_returns (Scheduler.go ())
