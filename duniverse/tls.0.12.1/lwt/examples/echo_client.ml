
open Ex_common
open Lwt

let cached_session : Tls.Core.epoch_data =
  let hex = Cstruct.of_hex in
  {
    Tls.Core.protocol_version = `TLS_1_3 ;
    ciphersuite = `DHE_RSA_WITH_AES_128_GCM_SHA256 ;
    peer_random = hex "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f" ;
    peer_certificate = None ;
    peer_certificate_chain = [] ;
    peer_name = None ;
    trust_anchor = None ;
    received_certificates = [] ;
    own_random = hex "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f" ;
    own_certificate = [] ;
    own_private_key = None ;
    own_name = None ;
    master_secret = hex "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f" ;
    session_id = Cstruct.create 0 ;
    extended_ms = true ;
    alpn_protocol = None ;
    state = `Established ;
  }

let echo_client ?ca hostname port =
  let open Lwt_io in
  Mirage_crypto_rng_lwt.initialize () >>= fun () ->
  auth ~hostname ?ca () >>= fun authenticator ->
  X509_lwt.private_of_pems
    ~cert:server_cert
    ~priv_key:server_key >>= fun certificate ->
  Tls_lwt.connect_ext
    Tls.Config.(client ~authenticator ~cached_session ~certificates:(`Single certificate) ~ciphers:Ciphers.supported ())
    (hostname, port) >>= fun (ic, oc) ->
  Lwt.join [
    lines ic    |> Lwt_stream.iter_s (printf "+ %s\n%!") ;
    lines stdin |> Lwt_stream.iter_s (write_line oc)
  ]

let jump _ port host ca =
  try
    Lwt_main.run (echo_client ?ca host port)
  with
  | Tls_lwt.Tls_alert alert as exn ->
    print_alert "remote end" alert ; raise exn
  | Tls_lwt.Tls_failure alert as exn ->
    print_fail "our end" alert ; raise exn

open Cmdliner

let port =
  let doc = "Port to connect to" in
  Arg.(value & opt int 443 & info [ "port" ] ~doc)

let host =
  let doc = "Host to connect to" in
  Arg.(value & opt string "" & info [ "host" ] ~doc)

let trust =
  let doc = "Trust anchor" in
  Arg.(value & opt (some string) None & info [ "trust" ] ~doc)

let cmd =
  Term.(const jump $ setup_log $ port $ host $ trust),
  Term.info "server" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
