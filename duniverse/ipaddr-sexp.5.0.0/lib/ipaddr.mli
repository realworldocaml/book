(*
 * Copyright (c) 2019 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
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

(** A library for manipulation of IP address representations.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** [Parse_error (err,packet)] is raised when parsing of the IP
    address syntax fails. [err] contains a human-readable error
    and [packet] is the original octet list that failed to parse. *)
exception Parse_error of string * string

(** Type of ordered address scope classifications *)
type scope =
| Point
| Interface
| Link
| Admin
| Site
| Organization
| Global

(** [string_of_scope scope] returns a human-readable representation
  of {!scope}. *)
val string_of_scope : scope -> string

(** [scope_of_string s] returns a {!scope} from a string representation
  of [s].  Valid string values for [s] can be obtained via {!string_of_scope}. *)
val scope_of_string : string -> (scope, [> `Msg of string]) result

(** [pp_scope fmt scope] outputs a human-readable representation
  of {!scope} to the [fmt] formatter. *)
val pp_scope : Format.formatter -> scope -> unit [@@ocaml.toplevel_printer]

(** A collection of functions for IPv4 addresses. *)
module V4 : sig
  (** Type of the internet protocol v4 address of a host *)
  type t

  (** Converts the low bytes of four int values into an abstract {! V4.t }. *)
  val make : int -> int -> int -> int -> t

  (** {3 Text string conversion}
      These manipulate human-readable IPv4 addresses (for example [192.168.1.2]). *)

  (** [of_string s] is the address {!t} represented by the human-readable IPv4
      address [s]. Returns a human-readable error string if parsing failed. *)
  val of_string : string -> (t, [> `Msg of string ]) result

  (** [of_string_exn s] is the address {!t} represented as a human-readable IPv4
      address [s]. Raises {!Parse_error} if [s] is not exactly 4 bytes long. *)
  val of_string_exn : string -> t

  (** [of_string_raw s off] acts as {!of_string_exn} but takes as an extra
      argument the offset into the string for reading. [off] will be
      mutated to an unspecified value during the function call. [s] will
      a {!Parse_error} exception if it is an invalid or truncated IP address. *)
  val of_string_raw : string -> int ref -> t

  (** [to_string ipv4] is the dotted decimal string representation
      of [ipv4], i.e. [XXX.XX.X.XXX]. *)
  val to_string : t -> string

  (** [to_buffer buf ipv4] writes the string representation of [ipv4] into the
      buffer [buf]. *)
  val to_buffer : Buffer.t -> t -> unit

  (** [pp f ipv4] outputs a human-readable representation of [ipv4] to
      the formatter [f]. *)
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]

  (** {3 Octets conversion}
      These manipulate IPv4 addresses represented as a sequence of
      four bytes. (e.g for example [0xc0a80102] will be the representation
      of the human-readable [192.168.1.2] address. *)

  (** [of_octets ?off s] is the address {!t} represented by the IPv4 octets
      represented by [s] starting from offset [off] within the string.
      [s] must be at least [off+4] bytes long.
      Returns a human-readable error string if parsing fails.
      [off] defaults to 0. *)
  val of_octets : ?off:int -> string -> (t, [> `Msg of string ]) result

  (** [of_octets_exn ipv4_octets] is the IPv4 address represented
      by [ipv4_octets] starting from offset [off] within the string.
      Raises {!Parse_error} if [ipv4_octets] is not at least [off+4] bytes long. 
      [off] defaults to 0. *)
  val of_octets_exn : ?off:int -> string -> t

  (** [write_octets ?off ipv4 b] writes the [ipv4] as octets to [b] starting
       from offset [off]. [b] must be at least [off+4] long or an error is
       returned. *)
  val write_octets : ?off:int -> t -> bytes -> (unit, [> `Msg of string ]) result

  (** [write_octets_exn ?off ipv4 b] writes the [ipv4] as octets to [b] starting
       from offset [off]. [b] must be at least [off+4] long or a
       {!Parse_error} is raised. *)
  val write_octets_exn : ?off:int -> t -> bytes -> unit

  (** [to_octets ipv4] returns the 4 bytes representing the [ipv4] octets. *)
  val to_octets : t -> string

  (** {3 Int conversion} *)

  (** [of_int32 ipv4_packed] is the address represented by
      [ipv4_packed]. *)
  val of_int32 : int32 -> t

  (** [to_int32 ipv4] is the 32-bit packed encoding of [ipv4]. *)
  val to_int32 : t -> int32

  (** [of_int16 ipv4_packed] is the address represented by
      [ipv4_packed]. *)
  val of_int16 : (int * int) -> t

  (** [to_int16 ipv4] is the 16-bit packed encoding of [ipv4]. *)
  val to_int16 : t -> int * int

  (** {3 MAC conversion} *)

  (** [multicast_to_mac ipv4] is the MAC address corresponding to the
      multicast address [ipv4]. Described by
      {{:http://tools.ietf.org/html/rfc1112#section-6.2}RFC 1112}. *)
  val multicast_to_mac : t -> Macaddr.t

  (** {3 Host conversion} *)

  (** [to_domain_name ipv4] is the domain name label list for reverse
      lookups of [ipv4]. This includes the [.in-addr.arpa] suffix. *)
  val to_domain_name : t -> [ `host ] Domain_name.t

  (** [of_domain_name name] is [Some t] if the [name] has an [.in-addr.arpa]
      suffix, and an IPv4 address prefixed. *)
  val of_domain_name : 'a Domain_name.t -> t option

  (** {3 Utility functions} *)

  (** [succ ipv4] is ip address next to [ipv4].
      Returns a human-readable error string if it's already the highest address. *)
  val succ : t -> (t, [> `Msg of string ]) result

  (** [pred ipv4] is ip address before [ipv4].
      Returns a human-readable error string if it's already the lowest address. *)
  val pred : t -> (t, [> `Msg of string ]) result

  (** {3 Common addresses} *)

  (** [any] is 0.0.0.0. *)
  val any : t

  (** [unspecified] is 0.0.0.0. *)
  val unspecified : t

  (** [broadcast] is 255.255.255.255. *)
  val broadcast : t

  (** [nodes] is 224.0.0.1. *)
  val nodes : t

  (** [routers] is 224.0.0.2. *)
  val routers : t

  (** [localhost] is 127.0.0.1. *)
  val localhost : t

  (** A module for manipulating IPv4 network prefixes (CIDR). *)
  module Prefix : sig
    type addr = t

    (** Type of a internet protocol subnet: an address and prefix length. *)
    type t

    (** [mask n] is the pseudo-address of an [n] bit subnet mask. *)
    val mask : int -> addr

    (** [make n addr] is the cidr of [addr] with [n] bits prefix. *)
    val make : int -> addr -> t

    (** [prefix cidr] is the subnet prefix of [cidr] where all non-prefix bits
        set to 0. *)
    val prefix : t -> t

    (** [network_address cidr addr] is the address with prefix [cidr]
        and suffix from [addr].
        See <http://tools.ietf.org/html/rfc4291#section-2.3>. *)
    val network_address : t -> addr -> addr

    (** [of_string cidr] is the subnet prefix represented by the CIDR
        string, [cidr]. Returns a human-readable parsing error message
        if [cidr] is not a valid representation of a CIDR notation routing
        prefix. *)
    val of_string : string ->  (t, [> `Msg of string ]) result

    (** [of_string_exn cidr] is the subnet prefix represented by the CIDR
        string, [cidr]. Raises [Parse_error] if [cidr] is not a valid
        representation of a CIDR notation routing prefix. *)
    val of_string_exn : string -> t

    (** Same as {!of_string_exn} but takes as an extra argument the offset
        into the string for reading. *)
    val of_string_raw : string -> int ref -> t

    (** [to_string cidr] is the CIDR notation string representation
        of [cidr], i.e. [XXX.XX.X.XXX/XX]. *)
    val to_string : t -> string

    (** [pp f cidr] outputs a human-readable representation of [cidr]
        to the formatter [f]. *)
    val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]

    (** [to_buffer buf cidr] writes the string representation
        of [cidr] into the buffer [buf]. *)
    val to_buffer : Buffer.t -> t -> unit

    (** [of_netmask_exn ~netmask ~address] is the subnet prefix of [address]
        with netmask [netmask]. *)
    val of_netmask_exn : netmask:addr -> address:addr -> t

    (** [of_netmask ~netmask ~address] is the cidr of [address] with
        netmask [netmask]. *)
    val of_netmask : netmask:addr -> address:addr ->
      (t, [> `Msg of string ]) result

    (** [mem ip subnet] checks whether [ip] is found within [subnet]. *)
    val mem : addr -> t -> bool

    (** [subset ~subnet ~network] checks whether [subnet] is contained
        within [network]. *)
    val subset : subnet:t -> network:t -> bool

    (** [of_addr ip] create a subnet composed of only one address, [ip].
        It is the same as [make 32 ip]. *)
    val of_addr : addr -> t

    (** The default route, all addresses in IPv4-space, 0.0.0.0/0. *)
    val global    : t

    (** The host loopback network, 127.0.0.0/8. *)
    val loopback  : t

    (** The local-link network, 169.254.0.0/16. *)
    val link      : t

    (** The relative addressing network, 0.0.0.0/8. *)
    val relative  : t

    (** The multicast network, 224.0.0.0/4. *)
    val multicast : t

    (** The private subnet with 10 as first octet, 10.0.0.0/8. *)
    val private_10  : t

    (** The private subnet with 172 as first octet, 172.16.0.0/12. *)
    val private_172 : t

    (** The private subnet with 192 as first octet, 192.168.0.0/16. *)
    val private_192 : t

    (** The privately addressable networks: [loopback], [link],
        [private_10], [private_172], [private_192]. *)
    val private_blocks : t list

    (** [broadcast subnet] is the broadcast address for [subnet]. *)
    val broadcast : t -> addr

    (** [network subnet] is the address for [subnet]. *)
    val network : t -> addr

    (** [netmask subnet] is the netmask for [subnet]. *)
    val netmask : t -> addr

    (** [address cidr] is the address for [cidr]. *)
    val address : t -> addr

    (** [bits cidr] is the bit size of the [cidr] prefix. *)
    val bits : t -> int

    (** [first cidr] is first valid unicast address in this [cidr]. *)
    val first : t -> addr

    (** [last cidr] is last valid unicast address in this [cidr]. *)
    val last : t -> addr

    include Map.OrderedType with type t := t
  end

  (** [scope ipv4] is the classification of [ipv4] by the {! scope }
      hierarchy. *)
  val scope : t -> scope

  (** [is_global ipv4] is a predicate indicating whether [ipv4] globally
      addresses a node. *)
  val is_global : t -> bool

  (** [is_multicast ipv4] is a predicate indicating whether [ipv4] is a
      multicast address. *)
  val is_multicast : t -> bool

  (** [is_private ipv4] is a predicate indicating whether [ipv4] privately
      addresses a node. *)
  val is_private : t -> bool

  include Map.OrderedType with type t := t
end


(** A collection of functions for IPv6 addresses. *)
module V6 : sig
  (** Type of the internet protocol v6 address of a host *)
  type t

  (** Converts the low bytes of eight int values into an abstract
      {! V6.t }. *)
  val make : int -> int -> int -> int -> int -> int -> int -> int -> t

  (** {3 Text string conversion} *)

  (** [of_string_exn ipv6_string] is the address represented
      by [ipv6_string]. Raises {!Parse_error} if [ipv6_string] is not a
      valid representation of an IPv6 address. *)
  val of_string_exn : string -> t

  (** Same as [of_string_exn] but returns an option type instead of raising
      an exception. *)
  val of_string : string -> (t, [> `Msg of string ]) result

  (** Same as [of_string_exn] but takes as an extra argument the offset into
      the string for reading. *)
  val of_string_raw : string -> int ref -> t

  (** [to_string ipv6] is the string representation of [ipv6],
      i.e. [XXX:XX:X::XXX:XX]. *)
  val to_string : t -> string

  (** [to_buffer buf ipv6] writes the string representation of [ipv6] into the
      buffer [buf]. *)
  val to_buffer : Buffer.t -> t -> unit

  (** [pp f ipv6] outputs a human-readable representation of [ipv6] to
      the formatter [f]. *)
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]

  (** {3 Octets conversion} *)

  (** [of_octets_exn ?off ipv6_octets] is the address represented
      by [ipv6_octets], starting from offset [off].
      Raises {!Parse_error} if [ipv6_octets] is not a valid representation of
      an IPv6 address. *)
  val of_octets_exn : ?off:int -> string -> t

  (** Same as {!of_octets_exn} but returns an result type instead of raising
      an exception. *)
  val of_octets : ?off:int -> string -> (t, [> `Msg of string ]) result

  (** [write_octets_exn ?off ipv6 b] writes 16 bytes that encode [ipv6] into [b] starting
      from offset [off] within [b].  [b] must be at least [off+16] bytes long or
      a {!Parse_error} exception will be raised. *)
  val write_octets_exn : ?off:int -> t -> bytes -> unit

  (** [write_octets ?off ipv6 b] writes 16 bytes that encode [ipv6] into [b] starting
      from offset [off] within [b].  [b] must be at least [off+16] bytes long or
      an error is returned. *)
  val write_octets : ?off:int -> t -> bytes -> (unit, [> `Msg of string ]) result

  (** [to_octets ipv6] returns the 16 bytes representing the [ipv6] octets. *)
  val to_octets : t -> string

  (** {3 Int conversion} *)

  (** [of_int64 (ho, lo)] is the IPv6 address represented by two int64. *)
  val of_int64 : int64 * int64 -> t

  (** [to_int64 ipv6] is the 128-bit packed encoding of [ipv6]. *)
  val to_int64 : t -> int64 * int64

  (** [of_int32 (a, b, c, d)] is the IPv6 address represented by four int32. *)
  val of_int32 : int32 * int32 * int32 * int32 -> t

  (** [to_int32 ipv6] is the 128-bit packed encoding of [ipv6]. *)
  val to_int32 : t -> int32 * int32 * int32 * int32

  (** [of_int16 (a, b, c, d, e, f, g, h)] is the IPv6 address represented by
      eight 16-bit int. *)
  val of_int16 : int * int * int * int * int * int * int * int -> t

  (** [to_int16 ipv6] is the 128-bit packed encoding of [ipv6]. *)
  val to_int16 : t -> int * int * int * int * int * int * int * int

  (** {3 MAC conversion} *)

  (** [multicast_to_mac ipv6] is the MAC address corresponding to the
      multicast address [ipv6]. Described by
      {{:https://tools.ietf.org/html/rfc2464#section-7}RFC 2464}. *)
  val multicast_to_mac : t -> Macaddr.t

  (** {3 Host conversion} *)

  (** [to_domain_name ipv6] is the domain name label list for reverse
      lookups of [ipv6]. This includes the [.ip6.arpa] suffix. *)
  val to_domain_name : t -> [ `host ] Domain_name.t

  (** [of_domain_name name] is [Some t] if the [name] has an [.ip6.arpa]
      suffix, and an IPv6 address prefixed. *)
  val of_domain_name : 'a Domain_name.t -> t option

  (** {3 Utility functions} *)

  (** [succ ipv6] is ip address next to [ipv6]. Returns a human-readable
      error string if it's already the highest address. *)
  val succ : t -> (t, [> `Msg of string ]) result

  (** [pred ipv6] is ip address before [ipv6]. Returns a human-readable
      error string if it's already the lowest address. *)
  val pred : t -> (t, [> `Msg of string ]) result

  (** {3 Common addresses} *)

  (** [unspecified] is ::. *)
  val unspecified : t

  (** [localhost] is ::1. *)
  val localhost : t

  (** [interface_nodes] is ff01::01. *)
  val interface_nodes : t

  (** [link_nodes] is ff02::01. *)
  val link_nodes : t

  (** [interface_routers] is ff01::02. *)
  val interface_routers : t

  (** [link_routers] is ff02::02. *)
  val link_routers : t

  (** [site_routers] is ff05::02. *)
  val site_routers : t

  (** A module for manipulating IPv6 network prefixes (CIDR). *)
  module Prefix : sig
    type addr = t

    (** Type of a internet protocol subnet: an address and a prefix length. *)
    type t

    (** [mask n] is the pseudo-address of an [n] bit subnet mask. *)
    val mask : int -> addr

    (** [make n addr] is the cidr of [addr] with [n] bit prefix. *)
    val make : int -> addr -> t

    (** [prefix cidr] is the subnet prefix of [cidr] where all non-prefix bits
        set to 0. *)
    val prefix : t -> t

    (** [network_address cidr addr] is the address with prefix [cidr]
        and suffix from [addr].
        See <http://tools.ietf.org/html/rfc4291#section-2.3>. *)
    val network_address : t -> addr -> addr

    (** [of_string_exn cidr] is the subnet prefix represented by the CIDR
        string, [cidr]. Raises {!Parse_error} if [cidr] is not a valid
        representation of a CIDR notation routing prefix. *)
    val of_string_exn : string -> t

    (** Same as {!of_string_exn} but returns a result type instead of raising
        an exception. *)
    val of_string : string -> (t, [> `Msg of string ]) result

    (** Same as {!of_string_exn} but takes as an extra argument the offset
        into the string for reading. *)
    val of_string_raw : string -> int ref -> t

    (** [to_string cidr] is the CIDR notation string representation
        of [cidr], i.e. XXX:XX:X::XXX/XX. *)
    val to_string : t -> string

    (** [pp f cidr] outputs a human-readable representation of [cidr]
        to the formatter [f]. *)
    val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]

    (** [to_buffer buf cidr] writes the string representation
        of [cidr] to the buffer [buf]. *)
    val to_buffer : Buffer.t -> t -> unit

    (** [of_netmask_exn ~netmask ~address] is the subnet prefix of [address]
        with netmask [netmask]. *)
    val of_netmask_exn : netmask:addr -> address:addr -> t

    (** [of_netmask ~netmask ~address] is the cidr of [address] with
        netmask [netmask]. *)
    val of_netmask : netmask:addr -> address:addr ->
      (t, [> `Msg of string ]) result

    (** [mem ip subnet] checks whether [ip] is found within [subnet]. *)
    val mem : addr -> t -> bool

    (** [subset ~subnet ~network] checks whether [subnet] is contained
        within [network]. *)
    val subset : subnet:t -> network:t -> bool

    (** [of_addr ip] create a subnet composed of only one address, [ip].
        It is the same as [make 128 ip]. *)
    val of_addr : addr -> t

    (** Global Unicast 001, 2000::/3. *)
    val global_unicast_001 : t

    (** The Unique Local Unicast (ULA), fc00::/7. *)
    val unique_local       : t

    (** Link-Local Unicast, fe80::/64. *)
    val link               : t

    (** The multicast network, ff00::/8. *)
    val multicast          : t

    (** IPv4-mapped addresses, ::ffff:0:0/96. *)
    val ipv4_mapped        : t

    (** Global Unicast addresses that don't use Modified EUI64 interface
        identifiers, ::/3. *)
    val noneui64_interface : t

    (** Solicited-Node multicast addresses *)
    val solicited_node : t

    (** [network subnet] is the address for [subnet]. *)
    val network : t -> addr

    (** [netmask subnet] is the netmask for [subnet]. *)
    val netmask : t -> addr

    (** [address cidr] is the address for [cidr]. *)
    val address : t -> addr

    (** [bits subnet] is the bit size of the [subnet] prefix. *)
    val bits : t -> int

    (** [first subnet] is first valid unicast address in this [subnet]. *)
    val first : t -> addr

    (** [last subnet] is last valid unicast address in this [subnet]. *)
    val last : t -> addr

    include Map.OrderedType with type t := t
  end

  (** [scope ipv6] is the classification of [ipv6] by the {! scope }
      hierarchy. *)
  val scope : t -> scope

  (** [link_address_of_mac mac] is the link-local address for an
      Ethernet interface derived by the IEEE MAC -> EUI-64 map with
      the Universal/Local bit complemented for IPv6.
      @see <https://tools.ietf.org/html/rfc2464#section-4> RFC 2464
  *)
  val link_address_of_mac : Macaddr.t -> t

  (** [is_global ipv6] is a predicate indicating whether [ipv6] globally
      addresses a node. *)
  val is_global : t -> bool

  (** [is_multicast ipv6] is a predicate indicating whether [ipv6] is a
      multicast address. *)
  val is_multicast : t -> bool

  (** [is_private ipv6] is a predicate indicating whether [ipv6] privately
      addresses a node. *)
  val is_private : t -> bool

  include Map.OrderedType with type t := t
end

(** Type of either an IPv4 value or an IPv6 value *)
type ('v4,'v6) v4v6 = V4 of 'v4 | V6 of 'v6

(** Type of any IP address *)
type t = (V4.t,V6.t) v4v6

(** [to_string addr] is the text string representation of [addr]. *)
val to_string : t -> string

(** [to_buffer buf addr] writes the text string representation of [addr] into
    [buf]. *)
val to_buffer : Buffer.t -> t -> unit

(** [pp f ip] outputs a human-readable representation of [ip] to the
    formatter [f]. *)
val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]

(** [of_string_exn s] parses [s] as an IPv4 or IPv6 address.
    Raises {!Parse_error} if [s] is not a valid string representation of an IP
    address. *)
val of_string_exn : string -> t

(** Same as {!of_string_exn} but returns a result type instead of raising an
    exception. *)
val of_string : string -> (t, [> `Msg of string ]) result

(** Same as [of_string_exn] but takes as an extra argument the offset into
    the string for reading. *)
val of_string_raw : string -> int ref -> t

(** [v4_of_v6 ipv6] is the IPv4 representation of the IPv6 address [ipv6].
    If [ipv6] is not an IPv4-mapped address, None is returned. *)
val v4_of_v6 : V6.t -> V4.t option

(** [to_v4 addr] is the IPv4 representation of [addr]. *)
val to_v4 : t -> V4.t option

(** [v6_of_v4 ipv4] is the IPv6 representation of the IPv4 address [ipv4]. *)
val v6_of_v4 : V4.t -> V6.t

(** [to_v6 addr] is the IPv6 representation of [addr]. *)
val to_v6 : t -> V6.t

(** [scope addr] is the classification of [addr] by the {! scope }
    hierarchy. *)
val scope : t -> scope

(** [is_global addr] is a predicate indicating whether [addr] globally
    addresses a node. *)
val is_global : t -> bool

(** [is_multicast addr] is a predicate indicating whether [addr] is a
    multicast address. *)
val is_multicast : t -> bool

(** [is_private addr] is a predicate indicating whether [addr] privately
    addresses a node. *)
val is_private : t -> bool

(** [multicast_to_mac addr] is the MAC address corresponding to the
    multicast address [addr]. See {!V4.multicast_to_mac} and
    {!V6.multicast_to_mac}.*)
val multicast_to_mac : t -> Macaddr.t

(** [to_domain_name addr] is the domain name label list for reverse
    lookups of [addr]. This includes the [.in-addr.arpa] or [.ip6.arpa] suffix. *)
val to_domain_name : t -> [ `host ] Domain_name.t

(** [of_domain_name name] is [Some t] if the [name] has an [.in-addr.arpa] or
    [ip6.arpa] suffix, and an IP address prefixed. *)
val of_domain_name : 'a Domain_name.t -> t option

(** [succ addr] is ip address next to [addr]. Returns a human-readable
    error string if it's already the highest address. *)
val succ : t -> (t, [> `Msg of string ]) result

(** [pred addr] is ip address before [addr]. Returns a human-readable
    error string if it's already the lowest address. *)
val pred : t -> (t, [> `Msg of string ]) result

module Prefix : sig
  type addr = t

  (** Type of a internet protocol subnet *)
  type t = (V4.Prefix.t, V6.Prefix.t) v4v6

  (** [to_string subnet] is the text string representation of [subnet]. *)
  val to_string : t -> string

  (** [to_buffer buf subnet] writes the text string representation of [subnet]
      into [buf]. *)
  val to_buffer : Buffer.t -> t -> unit

  (** [pp f subnet] outputs a human-readable representation of [subnet]
      to the formatter [f]. *)
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]

  (** [of_string_exn cidr] is the subnet prefix represented by the CIDR
      string, [cidr]. Raises {!Parse_error} if [cidr] is not a valid
      representation of a CIDR notation routing prefix. *)
  val of_string_exn : string -> t

  (** Same as {!of_string_exn} but returns a result type instead of raising
      an exception. *)
  val of_string : string -> (t, [> `Msg of string]) result

  (** Same as {!of_string_exn} but takes as an extra argument the offset
      into the string for reading. *)
  val of_string_raw : string -> int ref -> t

  (** [v4_of_v6 ipv6] is the IPv4 representation of the IPv6 subnet [ipv6].
      If [ipv6] is not an IPv4-mapped subnet, None is returned. *)
  val v4_of_v6 : V6.Prefix.t -> V4.Prefix.t option

  (** [to_v4 subnet] is the IPv4 representation of [subnet]. *)
  val to_v4 : t -> V4.Prefix.t option

  (** [v6_of_v4 ipv4] is the IPv6 representation of the IPv4 subnet [ipv4]. *)
  val v6_of_v4 : V4.Prefix.t -> V6.Prefix.t

  (** [to_v6 subnet] is the IPv6 representation of [subnet]. *)
  val to_v6 : t -> V6.Prefix.t

  (** [mem ip subnet] checks whether [ip] is found within [subnet]. *)
  val mem : addr -> t -> bool

  (** [subset ~subnet ~network] checks whether [subnet] is contained
      within [network]. *)
  val subset : subnet:t -> network:t -> bool

  (** [of_addr ip] create a subnet composed of only one address, [ip].*)
  val of_addr : addr -> t

  (** [network subnet] is the address for [subnet]. *)
  val network : t -> addr

  (** [netmask subnet] is the netmask for [subnet]. *)
  val netmask : t -> addr

  (** [first subnet] is first valid unicast address in this [subnet]. *)
  val first : t -> addr

  (** [last subnet] is last valid unicast address in this [subnet]. *)
  val last : t -> addr

  include Map.OrderedType with type t := t
end

include Map.OrderedType with type t := t
