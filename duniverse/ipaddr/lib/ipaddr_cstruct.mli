(*
 * Copyright (c) 2019 Anil Madhavapeddy
 * Copyright (c) 2014 Nicolás Ojeda Bär
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

(** Convert to and from Cstructs and IP addresses *)

(** Ipv4 address conversions *)
module V4 : sig

  (** [of_cstruct c] parses the first 4 octets of [c] into an IPv4 address. *)
  val of_cstruct : Cstruct.t -> (Ipaddr.V4.t, [> `Msg of string ]) result

  (** [of_cstruct_exn] parses the first 4 octets of [c] into an IPv4 address.
      Raises {!Ipaddr.Parse_failure} on error. *)
  val of_cstruct_exn : Cstruct.t -> Ipaddr.V4.t

  (** [to_cstruct ipv4] is a cstruct of length 4 encoding [ipv4].
      The cstruct is allocated using [allocator]. If [allocator] is
      not provided, [Cstruct.create] is used. *)
  val to_cstruct: ?allocator:(int -> Cstruct.t) -> Ipaddr.V4.t -> Cstruct.t

  (** [write_cstruct_exn ipv4 cs] writes 4 bytes into [cs] representing
      the [ipv4] address octets. Raises {!Ipaddr.Parse_error} if [cs]
      is not at least 4 bytes long. *)
  val write_cstruct_exn : Ipaddr.V4.t -> Cstruct.t -> unit
end

(** Ipv6 address conversions *)
module V6 : sig

  (** [of_cstruct c] parses the first 16 octets of [c] into an IPv6 address. *)
  val of_cstruct : Cstruct.t -> (Ipaddr.V6.t, [> `Msg of string ]) result

  (** [of_cstruct_exn] parses the first 16 octets of [c] into an IPv6 address.
      Raises {!Ipaddr.Parse_failure} on error. *)
  val of_cstruct_exn : Cstruct.t -> Ipaddr.V6.t

  (** [to_cstruct ipv6] is a cstruct of length 16 encoding [ipv6].
      The cstruct is allocated using [allocator]. If [allocator] is
      not provided, [Cstruct.create] is used. *)
  val to_cstruct: ?allocator:(int -> Cstruct.t) -> Ipaddr.V6.t -> Cstruct.t

  (** [write_cstruct_exn ipv6 cs] writes 16 bytes into [cs] representing
      the [ipv6] address octets. Raises {!Ipaddr.Parse_error} if [cs]
      is not at least 16 bytes long. *)
  val write_cstruct_exn : Ipaddr.V6.t -> Cstruct.t -> unit
end
