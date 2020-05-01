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

open Sexplib.Conv

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let fail fmt = Fmt.kstrf (fun s -> Lwt.fail (Failure s)) fmt
let err_tcp_not_supported = fail "%s: TCP is not supported"
let err_tls_not_supported = fail "%s: TLS is not supported"
let err_domain_sockets_not_supported =
  fail "%s: Unix domain sockets are not supported inside Unikernels"
let err_vchan_not_supported = fail "%s: VCHAN is not supported"
let err_unknown = fail "%s: unknown endpoint type"
let err_ipv6 = fail "%s: IPv6 is not supported"

module Flow = struct
  type error = [`Msg of string]
  type write_error = [ Mirage_flow.write_error | error ]

  let pp_error ppf (`Msg s) = Fmt.string ppf s

  let pp_write_error ppf = function
    | #Mirage_flow.write_error as e -> Mirage_flow.pp_write_error ppf e
    | #error as e                   -> pp_error ppf e

  open Mirage_flow_combinators

  type flow = Flow: (module CONCRETE with type flow = 'a) * 'a -> flow

  let create (type a) (module M: Mirage_flow.S with type flow = a) t =
    let m = (module Concrete(M): CONCRETE with type flow = a) in
    Flow (m , t)

  let read (Flow ((module F), flow)) = F.read flow
  let write (Flow ((module F), flow)) b = F.write flow b
  let writev (Flow ((module F), flow)) b = F.writev flow b
  let close (Flow ((module F), flow)) = F.close flow
end

type callback = Flow.flow -> unit Lwt.t

module type Handler = sig
  (** Runtime handler *)
  type t
  type client [@@deriving sexp]
  type server [@@deriving sexp]
  val connect: t -> client -> Flow.flow Lwt.t
  val listen: t -> server -> callback -> unit Lwt.t
end

type tcp_client = [ `TCP of Ipaddr_sexp.t * int ] [@@deriving sexp]
type tcp_server = [ `TCP of int ] [@@deriving sexp]

type 'a stackv4 = (module Mirage_stack.V4 with type t = 'a)
let stackv4 x = x

module type VCHAN = Vchan.S.ENDPOINT with type port = Vchan.Port.t
module type XS = Xs_client_lwt.S

type vchan_client = [
  | `Vchan of [
      | `Direct of int * Vchan.Port.t                   (** domain id, port *)
      | `Domain_socket of string * Vchan.Port.t (** Vchan Xen domain socket *)
    ]
] [@@deriving sexp]

type vchan_server = [
  | `Vchan of [
      | `Direct of int * Vchan.Port.t                   (** domain id, port *)
      | `Domain_socket                          (** Vchan Xen domain socket *)
    ]
] [@@deriving sexp]

type vchan = (module VCHAN)
type xs = (module XS)

let vchan x = x
let xs x = x

type 'a tls_client = [ `TLS of Tls.Config.client * 'a ] [@@deriving sexp]
type 'a tls_server = [ `TLS of Tls.Config.server * 'a ] [@@deriving sexp]

type client = [ tcp_client | vchan_client | client tls_client ] [@@deriving sexp]
type server = [ tcp_server | vchan_server | server tls_server ] [@@deriving sexp]

type tls_client' = client tls_client [@@deriving sexp]
type tls_server' = server tls_server [@@deriving sexp]

type ('c, 's) handler =
  S: (module Handler with type t = 'a and type client = 'c and type server = 's)
  * 'a -> ('c, 's) handler

let tcp_client i p = Lwt.return (`TCP (i, p))
let tcp_server _ p = Lwt.return (`TCP p)

type t = {
  tcp  : (tcp_client  , tcp_server  ) handler option;
  tls  : (tls_client' , tls_server' ) handler option;
  vchan: (vchan_client, vchan_server) handler option;
}

let empty = { tcp = None; tls = None; vchan = None }

let connect t (c:client) = match c with
  | `TCP _ as x ->
    begin match t.tcp with
      | None -> err_tcp_not_supported "connect"
      | Some (S ((module S), t)) -> S.connect t x
    end
  | `Vchan _ as x ->
    begin match t.vchan with
      | None -> err_vchan_not_supported "connect"
      | Some (S ((module S), t)) -> S.connect t x
    end
  | `TLS _ as x ->
    begin match t.tls with
      | None -> err_tls_not_supported "connect"
      | Some (S ((module S), t)) -> S.connect t x
    end

let listen t (s:server) f = match s with
  | `TCP _ as x ->
    begin match t.tcp with
      | None -> err_tcp_not_supported "listen"
      | Some (S ((module S), t)) -> S.listen t x f
    end
  | `Vchan _ as x ->
    begin match t.vchan with
      | None  -> err_vchan_not_supported "listen";
      | Some (S ((module S), t)) -> S.listen t x f
    end
  | `TLS _ as x ->
    begin match t.tls with
      | None -> err_tls_not_supported "listen"
      | Some (S ((module S), t)) -> S.listen t x f
    end

(******************************************************************************)
(*                         Implementation of handlers                         *)
(******************************************************************************)

(* TCP *)

module TCP (S: Mirage_stack.V4) = struct

  type t = S.t
  type client = tcp_client [@@deriving sexp]
  type server = tcp_server [@@deriving sexp]
  let err_tcp e = Lwt.fail @@ Failure
    (Format.asprintf "TCP connection failed: %a" S.TCPV4.pp_error e)

  let connect t (`TCP (ip, port): client) =
    match Ipaddr.to_v4 ip with
    | None    -> err_ipv6 "connect"
    | Some ip ->
      S.TCPV4.create_connection (S.tcpv4 t) (ip, port) >>= function
      | Error e -> err_tcp e
      | Ok flow ->
      let flow = Flow.create (module S.TCPV4) flow in
      Lwt.return flow

  let listen t (`TCP port: server) fn =
    let s, _u = Lwt.task () in
    S.listen_tcpv4 t ~port (fun flow ->
        let f = Flow.create (module S.TCPV4) flow in
        fn f
      );
    s

end

module With_tcp(S : Mirage_stack.V4) = struct
  module M = TCP(S)
  let handler stack = Lwt.return (S ((module M),stack))
  let connect stack t = handler stack >|= fun x -> { t with tcp = Some x }
end

let with_tcp (type t) t (module S: Mirage_stack.V4 with type t = t) stack =
  let module M = With_tcp(S) in
  M.connect stack t

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
  | `Vchan_direct (i, p)  -> port p >|= fun p -> `Vchan (`Direct (i, p))
  | `Vchan_domain_socket _-> Lwt.return (`Vchan `Domain_socket)

module Vchan (Xs: Xs_client_lwt.S) (V: VCHAN) = struct

  module XS = Conduit_xenstore.Make(Xs)

  type t = XS.t
  type client = vchan_client [@@deriving sexp]
  type server = vchan_server [@@deriving sexp]

  let register = XS.register

  let rec connect t (c:vchan_client) = match c with
    | `Vchan (`Domain_socket (uid, port)) ->
      XS.connect t ~remote_name:uid ~port >>= fun endp ->
      connect t (`Vchan endp :> vchan_client)
    | `Vchan (`Direct (domid, port)) ->
      V.client ~domid ~port () >>= fun flow ->
      Lwt.return (Flow.create (module V) flow)

  let listen (t:t) (server:vchan_server) fn = match server with
    | `Vchan (`Direct (domid, port)) ->
      V.server ~domid ~port () >>= fun t ->
      fn (Flow.create (module V) t)
    | `Vchan `Domain_socket ->
      XS.listen t >>= fun conns ->
      Lwt_stream.iter_p (function
          | `Direct (domid, port) ->
            V.server ~domid ~port () >>= fun t ->
            fn (Flow.create (module V) t)
        ) conns

end

let mk_vchan (module X: XS) (module V: VCHAN) t =
  let module V = Vchan(X)(V) in
  V.register t >|= fun t ->
  S ((module V), t)

let with_vchan t x y z = mk_vchan x y z >|= fun x -> { t with vchan = Some x }

(* TLS *)

let client_of_bytes _ =
  (* an https:// request doesn't need client-side authentication *)
  let authenticator ~host:_ _ = Ok None in
  Tls.Config.client ~authenticator ()

let server_of_bytes str = Tls.Config.server_of_sexp (Sexplib.Sexp.of_string str)

let tls_client c x = Lwt.return (`TLS (client_of_bytes c, x))
let tls_server s x = Lwt.return (`TLS (server_of_bytes s, x))

module TLS = struct

  module TLS = Tls_mirage.Make(Flow)
  let err_flow_write m e = fail "%s: %a" m TLS.pp_write_error e

  type x = t
  type t = x

  type client = tls_client' [@@deriving sexp]
  type server = tls_server' [@@deriving sexp]

  let connect (t:t) (`TLS (c, x): client) =
    connect t x >>= fun flow ->
    TLS.client_of_flow c flow >>= function
    | Error e -> err_flow_write "connect" e
    | Ok flow -> Lwt.return (Flow.create (module TLS) flow)

  let listen (t:t) (`TLS (c, x): server) fn =
    listen t x (fun flow ->
        TLS.server_of_flow c flow >>= function
        | Error e -> err_flow_write "listen" e
        | Ok flow -> fn (Flow.create (module TLS) flow)
      )

end

let tls t = Lwt.return (S ( (module TLS), t))

let with_tls t = tls t >|= fun x -> { t with tls = Some x }

type conduit = t

module type S = sig
  type t = conduit
  val empty: t
  module With_tcp (S:Mirage_stack.V4) : sig
    val connect : S.t -> t -> t Lwt.t
  end
  val with_tcp: t -> 'a stackv4 -> 'a -> t Lwt.t
  val with_tls: t -> t Lwt.t
  val with_vchan: t -> xs -> vchan -> string -> t Lwt.t
  val connect: t -> client -> Flow.flow Lwt.t
  val listen: t -> server -> callback -> unit Lwt.t
end

let rec client (e:Conduit.endp): client Lwt.t = match e with
  | `TCP (x, y) -> tcp_client x y
  | `Unix_domain_socket _ -> err_domain_sockets_not_supported "client"
  | `Vchan_direct _
  | `Vchan_domain_socket _ as x -> vchan_client x
  | `TLS (x, y) -> client y >>= fun c -> tls_client x c
  | `Unknown s -> err_unknown s

let rec server (e:Conduit.endp): server Lwt.t = match e with
  | `TCP (x, y) -> tcp_server x y
  | `Unix_domain_socket _ -> err_domain_sockets_not_supported "server"
  | `Vchan_direct _
  | `Vchan_domain_socket _ as x -> vchan_server x
  | `TLS (x, y) -> server y >>= fun s -> tls_server x s
  | `Unknown s -> err_unknown s

module Context (R: Mirage_random.S) (C: Mirage_clock.MCLOCK) (S: Mirage_stack.V4) = struct

  type t = Resolver_lwt.t * conduit

  module RES = Resolver_mirage.Make_with_stack(R)(C)(S)

  let conduit = empty
  let stackv4 = stackv4 (module S: Mirage_stack.V4 with type t = S.t)

  let create ?(tls=false) stack =
    let res = Resolver_lwt.init () in
    RES.R.register ~stack res;
    with_tcp conduit stackv4 stack >>= fun conduit ->
    if tls then
      with_tls conduit >|= fun conduit ->
      res, conduit
    else
      Lwt.return (res, conduit)

end
