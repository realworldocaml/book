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

(** Convert to and from Cstructs and MAC address. *)

(** [of_cstruct c] parses the 6 octets of [c] into a MAC address. *)
val of_cstruct : Cstruct.t -> (Macaddr.t, [> `Msg of string ]) result

(** [of_cstruct_exn] parses the 6 octets of [c] into a MAC address.
    Raises {!Macaddr.Parse_failure} on error. *)
val of_cstruct_exn : Cstruct.t -> Macaddr.t

(** [to_cstruct mac] is a cstruct of length 4 encoding [ipv4].
    The cstruct is allocated using [allocator]. If [allocator] is
    not provided, [Cstruct.create] is used. *)
val to_cstruct: ?allocator:(int -> Cstruct.t) -> Macaddr.t -> Cstruct.t

(** [write_cstruct_exn mac cs] writes 6 bytes into [cs] representing
    the [mac] address octets. Raises {!Macaddr.Parse_error} if [cs]
    is not 6 bytes long. *)
val write_cstruct_exn : Macaddr.t -> Cstruct.t -> unit
