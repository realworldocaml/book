open Lwt.Infix
open Printf

let conduit = Conduit_mirage.empty
let vchan = Conduit_mirage.vchan (module Vchan_xen)
let xs = Conduit_mirage.xs (module OS.Xs)

module Server(Time : Mirage_types_lwt.TIME) = struct

  let server_src = Logs.Src.create "server" ~doc:"vchan server"
  module Log = (val Logs.src_log server_src : Logs.LOG)

  let start _ =
    Conduit_mirage.with_vchan conduit xs vchan "foo_server" >>= fun t ->
    Log.info (fun f -> f "Server initialising");
    let callback flow =
      Log.info (fun f -> f "Got a new flow!");
      let rec loop () =
        Conduit_mirage.Flow.read flow
        >>= fun res ->
        match res with
        | `Ok buf ->
          Log.info (fun f -> f "Received: %s" @@ Cstruct.to_string buf); loop ()
        | `Eof ->
          Log.info (fun f -> f "End of transmission!"); Lwt.return_unit
        | `Error e ->
          Log.warn (fun f -> f "Error reading the vchan flow!");
          Lwt.return_unit
      in loop ()
    in
    Conduit_mirage.listen t (`Vchan `Domain_socket) callback

end

module Client (Time : Mirage_types_lwt.TIME) = struct

  let client_src = Logs.Src.create "client" ~doc:"vchan client"
  module Log = (val Logs.src_log client_src : Logs.LOG)

  let conduit = Conduit_mirage.empty

  let start _t =
    Time.sleep 2.0 >>= fun () ->
    Conduit_mirage.with_vchan conduit xs vchan "foo_client" >>= fun t ->
    Log.info (fun f -> f "Connecting...");
    let client = match Vchan.Port.of_string "flibble" with
      | `Ok port -> `Vchan (`Domain_socket ("foo_server", port))
      | `Error e -> failwith e
    in
    Conduit_mirage.connect t client >>= fun flow ->
    Conduit_mirage.sexp_of_client client
    |> Sexplib.Sexp.to_string_hum
    |> sprintf "Endpoint: %s"
    |> (fun s -> Log.info (fun f -> f "%s" s));

    Log.info (fun f -> f "Client connected");
    let rec write num =
      let buf = Io_page.(to_cstruct (get 1)) in
      let s = sprintf "num is %d" num in
      let len = String.length s in
      Cstruct.blit_from_string s 0 buf 0 len;
      let buf = Cstruct.sub buf 0 len in
      Conduit_mirage.Flow.write flow buf
      >>= function
      |`Eof -> Log.info (fun f -> f "EOF"); Time.sleep 5.
      |`Error _ -> Log.warn (fun f -> f "ERR"); Time.sleep 5.
      |`Ok () -> Time.sleep 0.1 >>= fun () -> write (num+1)
    in
    write 0

end
