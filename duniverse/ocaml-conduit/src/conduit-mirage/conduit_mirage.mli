(*
 * Copyright (c) 2012-2015 Anil Madhavapeddy <anil@recoil.org>
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

(** Functorial connection establishment interface that is compatible with the
    Mirage libraries. *)

type client =
  [ `TCP of Ipaddr.t * int  (** address and destination port *)
  | `TLS of Tls.Config.client * client
  | `Vchan of
    [ `Direct of int * Vchan.Port.t  (** domain id, port *)
    | `Domain_socket of string * Vchan.Port.t  (** Vchan Xen domain socket *)
    ] ]
[@@deriving sexp]
(** The type for client configuration values. *)

type server =
  [ `TCP of int  (** listening port *)
  | `TLS of Tls.Config.server * server
  | `Vchan of
    [ `Direct of int * Vchan.Port.t  (** domain id, port *)
    | `Domain_socket  (** Vchan Xen domain socket *) ] ]
[@@deriving sexp]
(** The type for server configuration values. *)

module Endpoint (P : Mirage_clock.PCLOCK) : sig
  val nss_authenticator : X509.Authenticator.t
  (** [nss_authenticator] is the validator using the
      {{:https://github.com/mirage/ca-certs-nss} trust anchors extracted from
      Mozilla's NSS}. *)

  val client :
    ?tls_authenticator:X509.Authenticator.t -> Conduit.endp -> client Lwt.t
  (** [client] resolves a conduit endpoint into a client configuration.

      The certificate is validated using [tls_authenticator]. By default, it is
      [nss_authenticator] *)

  val server :
    ?tls_authenticator:X509.Authenticator.t -> Conduit.endp -> server Lwt.t
  (** [server] resolves a confuit endpoint into a server configuration.

      Clent certificates are validated using [tls_authenticator]. *)
end

module type S = sig
  (** The signature for conduits *)

  type flow
  (** The type for networking flows. *)

  type t
  (** The type for handlers. *)

  module Flow : Mirage_flow.S with type flow = flow
  (** The type for flows. *)

  val connect : t -> client -> flow Lwt.t
  (** Connect a conduit using client configuration. *)

  val listen : t -> server -> (flow -> unit Lwt.t) -> unit Lwt.t
  (** Listen to a conduit using a server configuration. *)
end

(** {2 TCP} *)

module TCP (S : Tcpip.Stack.V4V6) :
  S with type t = S.t and type flow = S.TCP.flow

(** {2 VCHAN} *)

module Vchan
    (X : Xs_client_lwt.S)
    (V : Vchan.S.ENDPOINT with type port = Vchan.Port.t) : sig
  include S

  val register : string -> t Lwt.t
end

(** {2 TLS} *)

module TLS (S : S) : sig
  type flow = TLS of Tls_mirage.Make(S.Flow).flow | Clear of S.flow

  include S with type t = S.t and type flow := flow
end
