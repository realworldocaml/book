open Lwt.Infix

module Main (S  : Mirage_stack.V4)
            (KV : Mirage_kv.RO)
            (CL : Mirage_clock.PCLOCK) =
struct

  module TLS  = Tls_mirage.Make (S.TCPV4)
  module X509 = Tls_mirage.X509 (KV) (CL)
  module Http = Cohttp_mirage.Server (TLS)

  module Body = Cohttp_lwt.Body

  let callback _conn req body =
    let resp = Cohttp.Response.make ~status:`OK () in
    (match Cohttp.Request.meth req with
     | `POST ->
       Body.to_string body >|= fun contents ->
       "<pre>" ^ contents ^ "</pre>"
     | _     -> Lwt.return "") >|= fun inlet ->
    let body = Body.of_string @@
    "<html><head><title>ohai</title></head> \
     <body><h3>Secure CoHTTP on-line.</h3>"
    ^ inlet ^ "</body></html>\r\n"
    in
    (resp, body)

  let upgrade conf tcp =
    TLS.server_of_flow conf tcp >>= function
    | Error _  -> Lwt.fail (Failure "tls init")
    | Ok tls  ->
      let t = Http.make ~callback () in
      Http.listen t tls

  let start stack kv _ _ =
    X509.certificate kv `Default >>= fun cert ->
    let conf = Tls.Config.server ~certificates:(`Single cert) () in
    S.listen_tcpv4 stack ~port:4433 (upgrade conf) ;
    S.listen stack

end
