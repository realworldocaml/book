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

(** Interface for establishing reliable stream-oriented connections.

    This library abstracts the concerns of establishing connections to
    peers that may be running within the same host (e.g. in another
    virtual machine) or on a remote host via TCP.  It consists of one
    library that is responsible for {{!transport}establishing individual
    connections}, and a {{!resolution}name resolver} that maps URIs
    to endpoints.

    {2:transport Connection Establishment}

    Connections are created by identifying remote nodes using an
    {{!endp}endp} value.  To ensure portability, the {!endp} values
    are translated into concrete connections by separate modules that
    target [Lwt_unix], [Async] and [Mirage].  This lets those backends
    use the appropriate local technique for creating the connection
    (such as using OpenSSL on Unix, or a pure OCaml TLS+TCP
    implementation on Mirage, or some other combination).

    The modules dealing with connection establishment are:
    {!modules: Conduit_lwt_unix Conduit_async Conduit_mirage}

    {2:resolution Name Resolution}

    This deals with resolving URIs into a list of {!endp} addresses that can
    then be connected to by the {{!transport}connection establishment} modules.

    All of the name resolvers conform to the {!RESOLVER} module type.
    The OS-specific implementations of this interface are:
    {!modules: Resolver_lwt Resolver_lwt_unix Resolver_mirage}
 *)

(** End points that can potentially be connected to.
    These are typically returned by a call to a {{!resolution}resolver}. *)
type endp = [
  | `TCP of Ipaddr.t * int         (** IP address and destination port *)
  | `Unix_domain_socket of string  (** Unix domain file path *)
  | `Vchan_direct of int * string  (** domain id, port *)
  | `Vchan_domain_socket of string * string (** Vchan Xen domain socket *)
  | `TLS of string * endp          (** Wrap in a TLS channel, [hostname,endp] *)
  | `Unknown of string             (** Failed resolution *)
] [@@deriving sexp]

(** Module type for cooperative threading that can be satisfied by
    Lwt or Async *)
module type IO = sig
  type +'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end
