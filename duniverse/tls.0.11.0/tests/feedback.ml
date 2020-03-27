
module Flow = struct

  let rewrap_st = function (`S _, st) -> `S st | (`C _, st) -> `C st

  let unwrap_st = function `S st -> st | `C st -> st

  let can_handle_appdata st =
    Tls.Engine.can_handle_appdata (unwrap_st st)

  let send_application_data state data =
    match Tls.Engine.send_application_data (unwrap_st state) data with
    | None           -> None
    | Some (st', cs) -> Some (rewrap_st (state, st'), cs)

  let handle_tls ~tag state msg =
    let (st, descr) = match state with
      | `S st -> (st, "server")
      | `C st -> (st, "client") in
    match Tls.Engine.handle_tls st msg with
    | `Ok (`Ok st', `Response (Some ans), `Data appdata) ->
        (rewrap_st (state, st'), ans, appdata)
    | `Fail (a, _) ->
        failwith @@ Printf.sprintf "[%s] %s error: %s"
          tag descr (Sexplib.Sexp.to_string_hum (Tls.Engine.sexp_of_failure a))
    | `Ok _ -> failwith "decoded alert"
end

let loop_chatter ~certificate ~loops ~size =

  Printf.eprintf "Looping %d times, %d bytes.\n%!" loops size;

  let message  = Mirage_crypto_rng.generate size
  and server   = Tls.(Engine.server (Config.server ~certificates:(`Single certificate) ()))
  and (client, init) =
    let authenticator ~host:_ _ = Ok None in
    Tls.(Engine.client @@ Config.client ~authenticator ())
  in
  Testlib.time @@ fun () ->

    let rec handshake srv cli cli_msg =
      let tag = "handshake" in
      let (srv, ans, _) = Flow.handle_tls ~tag srv cli_msg in
      let (cli, ans, _) = Flow.handle_tls ~tag cli ans in
      if Flow.can_handle_appdata cli then (srv, cli)
      else handshake srv cli ans

    and chat srv cli data = function
      | 0 -> data
      | n ->
          let tag = "chat" in
          let simplex sender recv data =
            match Flow.send_application_data sender [data] with
            | None                -> failwith @@ "can't send"
            | Some (sender', msg) ->
                match Flow.handle_tls ~tag recv msg with
                | (recv', _, Some data') -> (sender', recv', data')
                | (_, _, None)           -> failwith "expected data"
          in
          let (cli, srv, data1) = simplex cli srv data in
          let (srv, cli, data2) = simplex srv cli data1 in
          chat srv cli data2 (pred n)
    in
    let (srv, cli) = handshake (`S server) (`C client) init in
    let message' = chat srv cli message loops in
    if Tls.Utils.Cs.equal message message' then ()
    else failwith @@ "the message got corrupted :("


let load_priv () =
  let cs1 = Testlib.cs_mmap "./certificates/server.pem"
  and cs2 = Testlib.cs_mmap "./certificates/server.key" in
  match
    X509.Certificate.decode_pem_multiple cs1, X509.Private_key.decode_pem cs2
  with
  | Ok certs, Ok (`RSA key) -> certs, key
  | Error (`Msg m), _ -> failwith ("can't parse certificates " ^ m)
  | _, Error (`Msg m) -> failwith ("can't parse private key " ^ m)

let _ =
  let loops =
    try int_of_string Sys.argv.(1) with _ -> 10
  and size  =
    try int_of_string Sys.argv.(2) with _ -> 1024
  and certificate = load_priv ()
  in
  loop_chatter ~certificate ~loops ~size

