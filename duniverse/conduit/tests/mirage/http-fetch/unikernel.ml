open Lwt.Infix
open Mirage_types_lwt
open Printf

let red fmt    = sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = sprintf ("\027[36m"^^fmt^^"\027[m")

let domain = "anil.recoil.org"
let uri = Uri.of_string "http://anil.recoil.org"
let ns = "8.8.8.8"

module Client (C:CONSOLE) (S:STACKV4) = struct

  module DNS = Dns_resolver_mirage.Make(OS.Time)(S)
  module RES = Resolver_mirage.Make(DNS)

  let mk_conduit s =
    let stackv4 = Conduit_mirage.stackv4 (module S) in
    Conduit_mirage.with_tcp Conduit_mirage.empty stackv4 s

  let start c stack _ =
    C.log_s c (sprintf "Resolving in 3s using DNS server %s" ns) >>= fun () ->
    OS.Time.sleep 3.0 >>= fun () ->
    let res = Resolver_lwt.init () in
    RES.register ~ns:(Ipaddr.V4.of_string_exn ns) ~stack res;
    Resolver_lwt.resolve_uri ~uri res >>= fun endp ->
    mk_conduit stack >>= fun conduit ->
    Conduit_mirage.client endp >>= fun client ->
    let endp = Sexplib.Sexp.to_string_hum (Conduit.sexp_of_endp endp) in
    C.log_s c endp >>= fun () ->
    Conduit_mirage.connect conduit client >>= fun flow ->
    let page = Io_page.(to_cstruct (get 1)) in
    let http_get = "GET / HTTP/1.1\nHost: anil.recoil.org\n\n" in
    Cstruct.blit_from_string http_get 0 page 0 (String.length http_get);
    let buf = Cstruct.sub page 0 (String.length http_get) in
    Conduit_mirage.Flow.write flow buf >>= function
    | `Eof    -> C.log_s c "EOF on write"
    | `Error _ -> C.log_s c "ERR on write"
    | `Ok buf ->  Conduit_mirage.Flow.read flow >>= function
      | `Eof -> C.log_s c "EOF"
      | `Error _ -> C.log_s c "ERR"
      | `Ok buf -> C.log_s c (sprintf "OK\n%s\n" (Cstruct.to_string buf))

end
