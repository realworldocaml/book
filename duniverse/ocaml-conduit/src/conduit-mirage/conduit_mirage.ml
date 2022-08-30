(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c)      2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

let src = Logs.Src.create "conduit_mirage" ~doc:"Conduit Mirage"

module Log = (val Logs.src_log src : Logs.LOG)
open Sexplib.Conv

let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= )
let fail fmt = Fmt.kstr (fun s -> Lwt.fail (Failure s)) fmt
let err_tcp_not_supported = fail "%s: TCP is not supported"
let err_tls_not_supported = fail "%s: TLS is not supported"

let err_domain_sockets_not_supported =
  fail "%s: Unix domain sockets are not supported inside Unikernels"

let err_vchan_not_supported = fail "%s: VCHAN is not supported"
let err_unknown = fail "%s: unknown endpoint type"

let err_not_supported = function
  | `TLS _ -> err_tls_not_supported
  | `TCP _ -> err_tcp_not_supported
  | `Vchan _ -> err_vchan_not_supported

type client =
  [ `TCP of Ipaddr_sexp.t * int
  | `TLS of Tls.Config.client * client
  | `Vchan of
    [ `Direct of int * Vchan.Port.t | `Domain_socket of string * Vchan.Port.t ]
  ]
[@@deriving sexp]

type server =
  [ `TCP of int
  | `TLS of Tls.Config.server * server
  | `Vchan of [ `Direct of int * Vchan.Port.t | `Domain_socket ] ]
[@@deriving sexp]

module type S = sig
  type t
  type flow

  module Flow : Mirage_flow.S with type flow = flow

  val connect : t -> client -> flow Lwt.t
  val listen : t -> server -> (flow -> unit Lwt.t) -> unit Lwt.t
end

(* TCP *)
let tcp_client i p = Lwt.return (`TCP (i, p))
let tcp_server _ p = Lwt.return (`TCP p)

module TCP (S : Tcpip.Stack.V4V6) = struct
  module Flow = S.TCP

  type flow = Flow.flow
  type t = S.t

  let err_tcp e =
    Lwt.fail
    @@ Failure (Format.asprintf "TCP connection failed: %a" S.TCP.pp_error e)

  let connect (t : t) (c : client) =
    match c with
    | `TCP (ip, port) -> (
        S.TCP.create_connection (S.tcp t) (ip, port) >>= function
        | Error e -> err_tcp e
        | Ok flow -> Lwt.return flow)
    | _ -> err_not_supported c "connect"

  let listen (t : t) (s : server) fn =
    match s with
    | `TCP port ->
        let s, _u = Lwt.task () in
        S.TCP.listen (S.tcp t) ~port (fun flow -> fn flow);
        s
    | _ -> err_not_supported s "listen"
end

(* VCHAN *)

let err_vchan_port = fail "%s: invalid Vchan port"

let port p =
  match Vchan.Port.of_string p with
  | Error (`Msg s) -> err_vchan_port s
  | Ok p -> Lwt.return p

let vchan_client = function
  | `Vchan_direct (i, p) -> port p >|= fun p -> `Vchan (`Direct (i, p))
  | `Vchan_domain_socket (i, p) ->
      port p >|= fun p -> `Vchan (`Domain_socket (i, p))

let vchan_server = function
  | `Vchan_direct (i, p) -> port p >|= fun p -> `Vchan (`Direct (i, p))
  | `Vchan_domain_socket _ -> Lwt.return (`Vchan `Domain_socket)

module Vchan
    (Xs : Xs_client_lwt.S)
    (V : Vchan.S.ENDPOINT with type port = Vchan.Port.t) =
struct
  module Flow = V
  module XS = Conduit_xenstore.Make (Xs)

  type flow = Flow.flow
  type t = XS.t

  let register = XS.register

  let rec connect (t : t) (c : client) =
    match c with
    | `Vchan (`Domain_socket (uid, port)) ->
        XS.connect t ~remote_name:uid ~port >>= fun endp ->
        connect t (`Vchan endp :> client)
    | `Vchan (`Direct (domid, port)) -> V.client ~domid ~port ()
    | _ -> err_not_supported c "connect"

  let listen (t : t) (s : server) fn =
    match s with
    | `Vchan (`Direct (domid, port)) -> V.server ~domid ~port () >>= fn
    | `Vchan `Domain_socket ->
        XS.listen t >>= fun conns ->
        Lwt_stream.iter_p
          (function `Direct (domid, port) -> V.server ~domid ~port () >>= fn)
          conns
    | _ -> err_not_supported s "listen"
end

(* TLS *)

let tls_client ~host ~authenticator x =
  let peer_name =
    Result.to_option (Result.bind (Domain_name.of_string host) Domain_name.host)
  in
  `TLS (Tls.Config.client ?peer_name ~authenticator (), x)

let tls_server ?authenticator x = `TLS (Tls.Config.server ?authenticator (), x)

module TLS (S : S) = struct
  module TLS = Tls_mirage.Make (S.Flow)

  type flow = TLS of TLS.flow | Clear of S.flow
  type t = S.t

  module Flow = struct
    type nonrec flow = flow
    type error = [ `Flow of S.Flow.error | `TLS of TLS.error ]

    type write_error =
      [ Mirage_flow.write_error
      | `Flow of S.Flow.write_error
      | `TLS of TLS.write_error ]

    let pp_error ppf = function
      | `Flow e -> S.Flow.pp_error ppf e
      | `TLS e -> TLS.pp_error ppf e

    let pp_write_error ppf = function
      | #Mirage_flow.write_error as e -> Mirage_flow.pp_write_error ppf e
      | `Flow e -> S.Flow.pp_write_error ppf e
      | `TLS e -> TLS.pp_write_error ppf e

    let tls_err = function Ok _ as x -> x | Error e -> Error (`TLS e)
    let flow_err = function Ok _ as x -> x | Error e -> Error (`Flow e)

    let tls_write_err = function
      | Ok _ as x -> x
      | Error `Closed as x -> x
      | Error e -> Error (`TLS e)

    let flow_write_err = function
      | Ok _ as x -> x
      | Error `Closed as x -> x
      | Error e -> Error (`Flow e)

    let read = function
      | TLS f -> TLS.read f >|= tls_err
      | Clear f -> S.Flow.read f >|= flow_err

    let write t x =
      match t with
      | TLS f -> TLS.write f x >|= tls_write_err
      | Clear f -> S.Flow.write f x >|= flow_write_err

    let writev t x =
      match t with
      | TLS f -> TLS.writev f x >|= tls_err
      | Clear f -> S.Flow.writev f x >|= flow_err

    let close = function TLS f -> TLS.close f | Clear f -> S.Flow.close f
  end

  let connect (t : t) (c : client) =
    match c with
    | `TLS (c, x) -> (
        S.connect t x >>= fun flow ->
        TLS.client_of_flow c flow >>= function
        | Error e -> fail "connect: %a" TLS.pp_write_error e
        | Ok flow -> Lwt.return (TLS flow))
    | _ -> S.connect t c >|= fun t -> Clear t

  let listen (t : t) (s : server) fn =
    match s with
    | `TLS (c, x) ->
        S.listen t x (fun flow ->
            TLS.server_of_flow c flow >>= function
            | Error e ->
                Log.info (fun m -> m "listen: %a" TLS.pp_write_error e);
                Lwt.return_unit
            | Ok flow -> fn (TLS flow))
    | _ -> S.listen t s (fun f -> fn (Clear f))
end

module Endpoint (P : Mirage_clock.PCLOCK) = struct
  module Ca_certs = Ca_certs_nss.Make (P)

  let nss_authenticator =
    match Ca_certs.authenticator () with
    | Ok a -> a
    | Error (`Msg msg) -> failwith msg

  let rec client ?(tls_authenticator = nss_authenticator) e =
    match e with
    | `TCP (x, y) -> tcp_client x y
    | `Unix_domain_socket _ -> err_domain_sockets_not_supported "client"
    | (`Vchan_direct _ | `Vchan_domain_socket _) as x -> vchan_client x
    | `TLS (host, y) ->
        client ~tls_authenticator y
        >|= tls_client ~host ~authenticator:tls_authenticator
    | `Unknown s -> err_unknown s

  let rec server ?tls_authenticator e =
    match e with
    | `TCP (x, y) -> tcp_server x y
    | `Unix_domain_socket _ -> err_domain_sockets_not_supported "server"
    | (`Vchan_direct _ | `Vchan_domain_socket _) as x -> vchan_server x
    | `TLS (_host, y) ->
        server y >|= tls_server ?authenticator:tls_authenticator
    | `Unknown s -> err_unknown s
end
