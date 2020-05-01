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

(** Functorial connection establishment interface that is compatible with
    the Mirage libraries.
  *)

module Flow: Mirage_flow.S
(** Dynamic flows. *)

type callback = Flow.flow -> unit Lwt.t
(** The type for callback values. *)

module type Handler = sig
  (** The signature for runtime handlers *)

  type t
  (** The type for runtime handlers. *)

  type client [@@deriving sexp]
  (** The type for client configuration values. *)

  type server [@@deriving sexp]
  (** The type for server configuration values. *)

  val connect: t -> client -> Flow.flow Lwt.t
  (** Connect a conduit using client configuration. *)

  val listen: t -> server -> callback -> unit Lwt.t
  (** Listen to a conduit using a server configuration. *)

end

(** {2 TCP} *)

(** The type for client connections. *)

type tcp_client = [ `TCP of Ipaddr.t * int ] (** address and destination port *)
and tcp_server  = [ `TCP of int ]                          (** listening port *)

type 'a stackv4
val stackv4: (module Mirage_stack.V4 with type t = 'a) -> 'a stackv4

(** {2 VCHAN} *)

type vchan_client = [
  | `Vchan of [
      | `Direct of int * Vchan.Port.t                   (** domain id, port *)
      | `Domain_socket of string * Vchan.Port.t (** Vchan Xen domain socket *)
    ]]

type vchan_server = [
  | `Vchan of [
      | `Direct of int * Vchan.Port.t                   (** domain id, port *)
      | `Domain_socket                          (** Vchan Xen domain socket *)
    ]]

module type VCHAN = Vchan.S.ENDPOINT with type port = Vchan.Port.t
module type XS = Xs_client_lwt.S

type vchan
type xs

val vchan: (module VCHAN) -> vchan
val xs: (module XS) -> xs

(** {2 TLS} *)

type 'a tls_client = [ `TLS of Tls.Config.client * 'a ]
type 'a tls_server = [ `TLS of Tls.Config.server * 'a ]

type client = [ tcp_client | vchan_client | client tls_client ] [@@deriving sexp]
(** The type for client configuration values. *)

type server = [ tcp_server | vchan_server | server tls_server ] [@@deriving sexp]
(** The type for server configuration values. *)

val client: Conduit.endp -> client Lwt.t
(** Resolve a conduit endpoint into a client configuration. *)

val server: Conduit.endp -> server Lwt.t
(** Resolve a confuit endpoint into a server configuration. *)

type conduit
(** The type for conduit values. *)

module type S = sig
  (** The signature for Conduit implementations. *)

  type t = conduit

  val empty: t
  (** The empty conduit. *)

  module With_tcp (S:Mirage_stack.V4) : sig
    val connect : S.t -> t -> t Lwt.t
  end

  val with_tcp: t -> 'a stackv4 -> 'a -> t Lwt.t
  (** Extend a conduit with an implementation for TCP. *)

  val with_tls: t -> t Lwt.t
  (** Extend a conduit with an implementation for TLS. *)

  val with_vchan: t -> xs -> vchan -> string -> t Lwt.t
  (** Extend a conduit with an implementation for VCHAN. *)

  val connect: t -> client -> Flow.flow Lwt.t
  (** Connect a conduit using a client configuration value. *)

  val listen: t -> server -> callback -> unit Lwt.t
  (** Configure a server using a conduit configuration value. *)

end

include S

(** {2 Context for MirageOS conduit resolvers} *)
module Context (R: Mirage_random.S) (C: Mirage_clock.MCLOCK) (S: Mirage_stack.V4): sig

  type t = Resolver_lwt.t * conduit
  (** The type for contexts of conduit resolvers. *)

  val create: ?tls:bool -> S.t -> t Lwt.t
  (** Create a new context. If [tls] is specified (by defaut, it is not),
      set-up the conduit to accept TLS connections. *)

end
