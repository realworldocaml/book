(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

(** Resolve URIs to endpoints *)

(** Description of a single service.
    Can be populated from [/etc/services] with the exception of the
    [tls] field, which indicates if the connection is intended to be
    TLS/SSL-encrypted or not (e.g. for [https]).  *)
type service = {
  name: string;
  port: int;
  tls: bool
} [@@deriving sexp]

(** Module type for a {{!resolution}resolver} that can map URIs to
    concrete {{!endp}endpoints} that stream connections can be
    established with. *)
module type S = sig

  (** Abstract type of the cooperative threading library used, normally
      defined via the {!IO} module type *)
  type +'a io

  (** State handle for a running resolver *)
  type t [@@deriving sexp]

  (** Abstract type for a service entry, which maps a URI scheme into
      a protocol handler and TCP port *)
  type svc [@@deriving sexp]

  (** A rewrite function resolves a {{!svc}service} and a URI into
      a concrete endpoint. *)
  type rewrite_fn = svc -> Uri.t -> Conduit.endp io

  (** A service function maps the string (such as [http] or [ftp]) from
      a URI scheme into a {{!svc}service} description that includes
      enough metadata about the service to subsequently {{!rewrite_fn}resolve}
      it into an {{!endp}endpoint}. *)
  type service_fn = string -> svc option io

  val (++): service_fn -> service_fn -> service_fn
  (** [f ++ g] is the composition of the service functions [f] and
      [g]. *)

  (** [init ?service ?rewrites] will initialize the resolver and return
      a state handler.  The {{!service_fn}service} argument should
      contain the system-specific resolution mechanism for URI schemas.

      The [rewrites] argument can optionally override a subset of the
      URI domain name with the given {!rewrite_fn} to permit custom
      resolution rules.  For example, a rewrite rule for ".xen" would
      let the rewrite function resolve hostnames such as "foo.xen"
      into a shared memory channel for the "foo" virtual machine. *)
  val init :
    ?service:service_fn -> ?rewrites:(string * rewrite_fn) list ->
    unit -> t

  (** [add_rewrite ~host f t] will add to the [t] resolver the [f] rewrite rule
      for all the domain names that shortest-prefix match [host] *)
  val add_rewrite : host:string -> f:rewrite_fn -> t -> unit

  val set_service : f:service_fn -> t -> unit

  val service: t -> service_fn
  (** [service t] is the function which is called when trying to
      resolve a hostname with [t]. *)

  (** [resolve_uri ?rewrites ~uri t] will use [t] to resolve the
      [uri] into a concrete endpoint.  Any [rewrites] that are passed
      in will be overlayed on the existing rules within the [t]
      resolver, but not otherwise modify it. *)
  val resolve_uri :
    ?rewrites:(string * rewrite_fn) list ->
    uri:Uri.t -> t -> Conduit.endp io
end

(** Functor to construct a concrete resolver using a {!Conduit.IO}
    implementation, usually via either Lwt or Async *)
module Make (IO : Conduit.IO) : S
  with type svc = service
  and  type 'a io = 'a IO.t
