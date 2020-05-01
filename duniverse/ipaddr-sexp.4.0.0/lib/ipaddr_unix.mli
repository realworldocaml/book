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

(** Convert to and from [Unix] to [Ipaddr] representations

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** [to_inet_addr ip] is the {! Unix.inet_addr} equivalent of the
    IPv4 or IPv6 address [ip]. *)
val to_inet_addr : Ipaddr.t -> Unix.inet_addr

(** [of_inet_addr ip] is the {! Ipaddr.t} equivalent of the
    {! Unix.inet_addr} [ip]. *)
val of_inet_addr : Unix.inet_addr -> Ipaddr.t

module V4 : sig

  (** [to_inet_addr ip] is the {! Unix.inet_addr} equivalent of the
      IPv4 address [ip]. *)
  val to_inet_addr : Ipaddr.V4.t -> Unix.inet_addr

  (** [of_inet_addr_exn ip] is the {! Ipaddr.t} equivalent of the
      {!Unix.inet_addr} [ip] IPv4 address. Raises {! Ipaddr.Parse_error} if
      [ip] is not a valid representation of an IPv4 address. *)
  val of_inet_addr_exn : Unix.inet_addr -> Ipaddr.V4.t

  (** Same as [of_inet_addr_exn] but returns an option type instead of raising
      an exception. *)
  val of_inet_addr : Unix.inet_addr -> Ipaddr.V4.t option
end

module V6 : sig

  (** [to_inet_addr ip] is the {! Unix.inet_addr} equivalent of the
      IPv6 address [ip]. *)
  val to_inet_addr : Ipaddr.V6.t -> Unix.inet_addr


  (** [of_inet_addr_exn ip] is the {! Ipaddr.t} equivalent of the
      {!Unix.inet_addr} [ip] IPv6 address. Raises {! Ipaddr.Parse_error} if
      [ip] is not a valid representation of an IPv6 address. *)
  val of_inet_addr_exn : Unix.inet_addr -> Ipaddr.V6.t

  (** Same as [of_inet_addr_exn] but returns an option type instead of raising
      an exception. *)
  val of_inet_addr : Unix.inet_addr -> Ipaddr.V6.t option
end
