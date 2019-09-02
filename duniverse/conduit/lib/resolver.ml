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

open Sexplib.Std
open Astring

type service = {
  name: string;
  port: int;
  tls: bool
} [@@deriving sexp]

(** Module type for a {{!resolution}resolver} that can map URIs to
    concrete {{!Conduit.endp}endpoints} that stream connections can be
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
      it into an {{!Conduit.endp}endpoint}. *)
  type service_fn = string -> svc option io

  val (++): service_fn -> service_fn -> service_fn

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

  (** [resolve_uri ?rewrites ~uri t] will use [t] to resolve the
      [uri] into a concrete endpoint.  Any [rewrites] that are passed
      in will be overlayed on the existing rules within the [t]
      resolver, but not otherwise modify it. *)
  val resolve_uri :
    ?rewrites:(string * rewrite_fn) list ->
    uri:Uri.t -> t -> Conduit.endp io
end

module Make(IO:Conduit.IO) = struct
  open IO

  type svc = service [@@deriving sexp]
  type 'a io = 'a IO.t

  (** A rewrite modifies an input URI with more specialization
      towards a concrete [endp] *)
  type rewrite_fn = service -> Uri.t -> Conduit.endp IO.t [@@deriving sexp]
  type service_fn = string -> service option IO.t [@@deriving sexp]

  type t = {
    default_lookup : rewrite_fn;
    mutable domains: rewrite_fn Conduit_trie.t;
    mutable service: service_fn;
  } [@@deriving sexp]

  let default_lookup _ uri =
    (* TODO log *)
    let host =
      match Uri.host uri with
      | None -> ""
      | Some host -> host
    in
    return (`Unknown host)

  let default_service _name =
    (* TODO log *)
    return None

  let host_to_domain_list host =
    (* TODO: slow, specialise the Trie to be a rev string list instead *)
    String.concat ~sep:"." (List.rev (String.cuts ~sep:"." host))

  let add_rewrite ~host ~f t =
    t.domains <- Conduit_trie.insert (host_to_domain_list host) f t.domains

  let set_service ~f t =
    t.service <- f

  let service t = t.service

  let (++) f g h =
    f h >>= function
    | None -> g h
    | x    -> return x

  let init ?(service=default_service) ?(rewrites=[]) () =
    let domains = Conduit_trie.empty in
    let t = { domains; default_lookup; service } in
    List.iter (fun (host,f) -> add_rewrite ~host ~f t) rewrites;
    t

  let resolve_uri ?rewrites ~uri t =
    (* Find the service associated with the URI *)
    match Uri.scheme uri with
    | None ->
      return (`Unknown "no scheme")
    | Some scheme -> begin
        t.service scheme
        >>= function
        | None -> return (`Unknown "unknown scheme")
        | Some service ->
          let host =
            match Uri.host uri with
            | None -> "localhost"
            | Some host -> host
          in
          let trie =
            (* If there are local rewrites, add them to the trie *)
            match rewrites with
            | None -> t.domains
            | Some rewrites ->
              List.fold_left (fun acc (host, f) ->
                  Conduit_trie.insert (host_to_domain_list host) f acc)
                t.domains rewrites
          in
          (* Find the longest prefix function that matches this host *)
          let fn =
            match Conduit_trie.longest_prefix (host_to_domain_list host) trie
            with
            | None -> t.default_lookup
            | Some fn -> fn
          in
          fn service uri
          >>= fun endp ->
          if service.tls then
            return (`TLS (host, endp))
          else
            return endp
      end
end
