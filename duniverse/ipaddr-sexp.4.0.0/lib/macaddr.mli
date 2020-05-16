(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
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
 *)

(** A library for manipulation of MAC address representations.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** [Parse_error (err,packet)] is raised when parsing of the MAC
    address syntax fails. [err] contains a human-readable error
    and [packet] is the original octet list that failed to parse. *)
exception Parse_error of string * string

(** Type of the hardware address (MAC) of an ethernet interface. *)
type t

(** {2 Functions converting MAC addresses to/from octets/string} *)

(** [of_octets_exn buf] is the hardware address extracted from
    [buf]. Raises [Parse_error] if [buf] has not length 6. *)
val of_octets_exn : string -> t

(** Same as {!of_octets_exn} but returns a result type instead of
    raising an exception. *)
val of_octets : string -> (t, [> `Msg of string]) result

(** [of_string_exn mac_string] is the human-readable hardware address represented by
    [mac_string]. Raises {!Parse_error} if [mac_string] is not a
    valid representation of a MAC address. *)
val of_string_exn : string -> t

(** Same as {!of_string_exn} but returns a result type instead of raising an
    exception. *)
val of_string : string ->  (t, [> `Msg of string]) result

(** [to_octets mac_addr] is a string of size 6 encoding [mac_addr] as a
    sequence of bytes. *)
val to_octets : t -> string

(** [to_string ?(sep=':') mac_addr] is the [sep]-separated string representation
    of [mac_addr], i.e. [xx:xx:xx:xx:xx:xx]. *)
val to_string : ?sep:char -> t -> string

(** [pp f mac_addr] outputs a human-readable representation of [mac_addr] to
    the formatter [f]. *)
val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]

(** [broadcast] is [ff:ff:ff:ff:ff:ff]. *)
val broadcast : t

(** [make_local bytegen] creates a unicast, locally administered MAC
    address given a function mapping octet offset to octet value. *)
val make_local : (int -> int) -> t

(** [get_oui macaddr] is the integer organization identifier for [macaddr]. *)
val get_oui : t -> int

(** [is_local macaddr] is the predicate on the locally administered bit
    of [macaddr]. *)
val is_local : t -> bool

(** [is_unicast macaddr] the is the predicate on the unicast bit of
    [macaddr]. *)
val is_unicast : t -> bool

include Map.OrderedType with type t := t
