
open Lwt

let o f g x = f (g x)

let ca_cert_dir = "./certificates"
let server_cert = "./certificates/server.pem"
let server_key  = "./certificates/server.key"

let yap ~tag msg = Lwt_io.printf "[%s] %s\n%!" tag msg

let lines ic =
  Lwt_stream.from @@ fun () ->
    Lwt_io.read_line_opt ic >>= function
      | None -> Lwt_io.close ic >>= fun () -> return_none
      | line -> return line

let eprint_sexp sexp =
  output_string stderr Sexplib.Sexp.(to_string_hum sexp) ;
  output_string stderr "\n\n" ;
  flush stderr

let print_alert where alert =
    Printf.eprintf "TLS ALERT (%s): %s\n%!"
      where (Tls.Packet.alert_type_to_string alert)

let print_fail where fail =
  Printf.eprintf "TLS FAIL (%s): %s\n%!"
    where (Tls.Engine.string_of_failure fail)

let null_auth ~host:_ _ = Ok None

let auth ~hostname ?ca ?fp () =
  match ca with
  | Some "NONE" when fp = None -> Lwt.return null_auth
  | _ ->
    let a = match ca, fp with
      | None, Some fp  -> `Hex_key_fingerprints (`SHA256, [ Domain_name.(host_exn (of_string_exn hostname)), fp ])
      | None, _ -> `Ca_dir ca_cert_dir
      | Some f, _ -> `Ca_file f
    in
    X509_lwt.authenticator a
