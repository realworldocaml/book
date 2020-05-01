(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
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

(** Manipulate external memory buffers as C-like structures.

Cstruct is a library and ppx rewriter to make it easier to access C-like
structures directly from OCaml.  It supports both reading and writing to these
memory buffers, and they are accessed via the [Bigarray] module.

The library interface below is intended to be used in conjunction with the
ppx rewriter that is also supplied with this library (in the [cstruct-ppx]
ocamlfind package).

An example description for the pcap packet format is:

{[
[%%cstruct
type pcap_header = {
  magic_number:  uint32_t; (* magic number *)
  version_major: uint16_t; (* major version number *)
  version_minor: uint16_t; (* minor version number *)
  thiszone:      uint32_t; (* GMT to local correction *)
  sigfigs:       uint32_t; (* accuracy of timestamps *)
  snaplen:       uint32_t; (* max length of captured packets, in octets *)
  network:       uint32_t; (* data link type *)
} [@@little_endian]
]
[%%cstruct
type pcap_packet = {
  ts_sec:   uint32_t; (* timestamp seconds *)
  ts_usec:  uint32_t; (* timestamp microseconds *)
  incl_len: uint32_t; (* number of octets of packet saved in file *)
  orig_len: uint32_t; (* actual length of packet *)
} [@@little_endian]
]
[%%cstruct
type ethernet = {
  dst:       uint8_t;  [@len 6];
  src:       uint8_t;  [@len 6];
  ethertype: uint16_t;
} [@@big_endian]
]
[%%cstruct
type ipv4 = {
  hlen_version: uint8_t;
  tos:          uint8_t;
  len:          uint16_t;
  id:           uint16_t;
  off:          uint16_t;
  ttl:          uint8_t;
  proto:        uint8_t;
  csum:         uint16_t;
  src:          uint8_t;  [@len 4];
  dst:          uint8_t;  [@len 4]
} [@@big_endian]
]
]}

These will expand to get and set functions for every field, with types
appropriate to the particular definition.  For instance:

{[
val get_pcap_packet_ts_sec : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_ts_sec : Cstruct.t -> Cstruct.uint32 -> unit
val get_pcap_packet_ts_usec : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_ts_usec : Cstruct.t -> Cstruct.uint32 -> unit
val get_pcap_packet_incl_len : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_incl_len : Cstruct.t -> Cstruct.uint32 -> unit
val get_pcap_packet_orig_len : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_orig_len : Cstruct.t -> Cstruct.uint32 -> unit
val hexdump_pcap_packet_to_buffer : Buffer.t -> Cstruct.t -> unit
]}

The buffers generate a different set of functions. For the  [ethernet]
definitions, we have:

{[
val sizeof_ethernet : int
val get_ethernet_dst : Cstruct.t -> Cstruct.t
val copy_ethernet_dst : Cstruct.t -> string
val set_ethernet_dst : string -> int -> Cstruct.t -> unit
val blit_ethernet_dst : Cstruct.t -> int -> Cstruct.t -> unit
val get_ethernet_src : Cstruct.t -> Cstruct.t
val copy_ethernet_src : Cstruct.t -> string
]}

You can also declare C-like enums:

{[
[%%cenum
type foo32 =
  | ONE32
  | TWO32     [@id 0xfffffffel]
  | THREE32
[@@uint32_t]
]
[%%cenum
type bar16 =
  | ONE  [@id 1]
  | TWO
  | FOUR [@id 4
  | FIVE
[@@uint16_t]
]
]}

This generates signatures of the form:

{[
type foo32 = | ONE32 | TWO32 | THREE32
val int_to_foo32 : int32 -> foo32 option
val foo32_to_int : foo32 -> int32
val foo32_to_string : foo32 -> string
val string_to_foo32 : string -> foo32 option
type bar16 = | ONE | TWO | FOUR | FIVE
val int_to_bar16 : int -> bar16 option
val bar16_to_int : bar16 -> int
val bar16_to_string : bar16 -> string
val string_to_bar16 : string -> bar16 option
]}

*)

(** {2 Base types } *)

type buffer = (char, Bigarray_compat.int8_unsigned_elt, Bigarray_compat.c_layout) Bigarray_compat.Array1.t
(** Type of a buffer. A cstruct is composed of an underlying buffer
    and position/length within this buffer. *)

type t = private {
  buffer: buffer;
  off   : int;
  len   : int;
}
(** Type of a cstruct. *)

type byte = char
(** A single byte type *)

val byte : int -> byte
(** [byte v] convert [v] to a single byte.
    @raise Invalid_argument if [v] is negative or greater than 255. *)

type uint8 = int
(** 8-bit unsigned integer.  The representation is currently an
    unboxed OCaml integer. *)

type uint16 = int
(** 16-bit unsigned integer.  The representation is currently an
    unboxed OCaml integer. *)

type uint32 = int32
(** 32-bit unsigned integer.  The representation is currently a
    boxed OCaml int32. *)

type uint64 = int64
(** 64-bit unsigned integer.  The representation is currently a
    boxed OCaml int64. *)

(** {2 Creation and conversion} *)

val empty : t
(** [empty] is the cstruct of length 0. *)

val of_bigarray: ?off:int -> ?len:int -> buffer -> t
(** [of_bigarray ~off ~len b] is the cstruct contained in [b] starting
    at [off], of length [len]. *)

val to_bigarray: t -> buffer
(** [to_bigarray t] converts a {!t} into a {!buffer} Bigarray, using
    the Bigarray slicing to allocate a fresh array that preserves
    sharing of the underlying buffer. *)

val create : int -> t
(** [create len] is a fresh cstruct of size [len] with an offset of 0,
    filled with zero bytes. *)

val create_unsafe : int -> t
(** [create_unsafe len] is a cstruct of size [len] with an offset of 0.

    Note that the returned cstruct will contain arbitrary data,
    likely including the contents of previously-deallocated cstructs.

    Beware!

    Forgetting to replace this data could cause your application
    to leak sensitive information.
*)

val of_string: ?allocator:(int -> t) -> ?off:int -> ?len:int -> string -> t
(** [of_string ~allocator ~off ~len str] is the cstruct representation of [str]
    slice located at [off] offset and of [len] length,
    with the underlying buffer allocated by [alloc]. If [allocator] is not
    provided, [create] is used. *)

val of_bytes: ?allocator:(int -> t) -> ?off:int -> ?len:int -> bytes -> t
(** [of_bytes ~allocator byt] is the cstruct representation of [byt]
    slice located at [off] offset and of [len] length,
    with the underlying buffer allocated by [alloc]. If [allocator] is not
    provided, [create] is used. *)

val of_hex: string -> t
(** [of_hex str] is the cstruct [cs].  Every pair of hex-encoded characters in
    [str] are converted to one byte in [cs].  Whitespaces (space, newline, tab,
    carriage return) in [str] are skipped.  The resulting cstruct is exactly
    half the size of the non-skipped characters of [str].

    @raise Invalid_argument if the input string contains invalid characters or
    has an odd numbers of non-whitespace characters. *)

(** {2 Comparison } *)

val equal : t -> t -> bool
(** [equal t1 t2] is [true] iff [t1] and [t2] correspond to the same sequence of
    bytes. *)

val compare : t -> t -> int
(** [compare t1 t2] gives an unspecified total ordering over {!t}. *)

(** {2 Getters and Setters } *)

val byte_to_int : byte -> int
(** Convert a byte to an integer *)

val check_bounds : t -> int -> bool
(** [check_bounds cstr len] is [true] if [len] is a non-negative integer and
    [cstr.buffer]'s size is greater or equal than [len] [false] otherwise.*)

val check_alignment : t -> int -> bool
(** [check_alignment cstr alignment] is [true] if the first byte stored
    within [cstr] is at a memory address where [address mod alignment = 0],
    [false] otherwise.
    Typical uses are to check a buffer is aligned to a page or disk sector
    boundary.
    @raise Invalid_argument if [alignment] is not a positive integer. *)

val get_char: t -> int -> char
(** [get_char t off] returns the character contained in the cstruct
    at offset [off].
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val get_uint8: t -> int -> uint8
(** [get_uint8 t off] returns the byte contained in the cstruct
    at offset [off].
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val set_char: t -> int -> char -> unit
(** [set_char t off c] sets the byte contained in the cstruct
    at offset [off] to character [c].
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val set_uint8: t -> int -> uint8 -> unit
(** [set_uint8 t off c] sets the byte contained in the cstruct
    at offset [off] to byte [c].
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val sub: t -> int -> int -> t
(** [sub cstr off len] is [{ t with off = t.off + off; len }]
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val shift: t -> int -> t
(** [shift cstr len] is [{ cstr with off=t.off+len; len=t.len-len }]
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val copy: t -> int -> int -> string
(** [copy cstr off len] is the string representation of the segment of
    [t] starting at [off] of size [len].
    @raise Invalid_argument if [off] and [len] do not designate a
    valid segment of [t]. *)

val blit: t -> int -> t -> int -> int -> unit
(** [blit src srcoff dst dstoff len] copies [len] characters from
    cstruct [src], starting at index [srcoff], to cstruct [dst],
    starting at index [dstoff]. It works correctly even if [src] and
    [dst] are the same string, and the source and destination
    intervals overlap.

    @raise Invalid_argument if [srcoff] and [len] do not designate a
    valid segment of [src], or if [dstoff] and [len] do not designate
    a valid segment of [dst]. *)

val blit_from_string: string -> int -> t -> int -> int -> unit
(** [blit_from_string src srcoff dst dstoff len] copies [len]
    characters from string [src], starting at index [srcoff], to
    cstruct [dst], starting at index [dstoff].

    @raise Invalid_argument if [srcoff] and [len] do not designate a
    valid substring of [src], or if [dstoff] and [len] do not
    designate a valid segment of [dst]. *)

val blit_from_bytes: bytes -> int -> t -> int -> int -> unit
(** [blit_from_bytes src srcoff dst dstoff len] copies [len]
    characters from bytes [src], starting at index [srcoff], to
    cstruct [dst], starting at index [dstoff].

    @raise Invalid_argument if [srcoff] and [len] do not designate a
    valid subsequence of [src], or if [dstoff] and [len] do not
    designate a valid segment of [dst]. *)

val blit_to_bytes: t -> int -> bytes -> int -> int -> unit
(** [blit_to_bytes src srcoff dst dstoff len] copies [len] characters
    from cstruct [src], starting at index [srcoff], to the [dst] buffer,
    starting at index [dstoff].

    @raise Invalid_argument if [srcoff] and [len] do not designate a
    valid segment of [src], or if [dstoff] and [len] do not designate
    a valid segment of [dst]. *)

val blit_to_string: t -> int -> bytes -> int -> int -> unit
  [@@ocaml.deprecated "Use blit_to_bytes instead, blit_to_string will be removed in cstruct 5.0.0"]
(** [blit_to_string] is a deprecated alias of {!blit_to_bytes}. *)

val memset: t -> int -> unit
(** [memset t x] sets all the bytes of [t] to [x land 0xff]. *)

val len: t -> int
(** Returns the length of the current cstruct view.  Note that this
    length is potentially smaller than the actual size of the underlying
    buffer, as the [sub] or [set_len] functions can construct a smaller view. *)

val set_len : t -> int -> t
  [@@ocaml.deprecated "This function will be removed in cstruct 5.0.0. If you need this function, discuss other ways in the issue tracker https://github.com/mirage/ocaml-cstruct."]
(** [set_len t len] sets the length of the cstruct [t] to a new absolute
    value, and returns a fresh cstruct with these settings.
    @raise Invalid_argument if [len] exceeds the size of the buffer. *)

val add_len : t -> int -> t
  [@@ocaml.deprecated "This function will be removed in cstruct 5.0.0. If you need this function, discuss other ways in the issue tracker https://github.com/mirage/ocaml-cstruct."]
(** [add_len t l] will add [l] bytes to the length of the buffer, and return
    a fresh cstruct with these settings.
    @raise Invalid_argument if [len] exceeds the size of the buffer. *)

val split: ?start:int -> t -> int -> t * t
(** [split ~start cstr len] is a tuple containing the cstruct
    extracted from [cstr] at offset [start] (default: 0) of length
    [len] as first element, and the rest of [cstr] as second
    element.
    @raise Invalid_argument if [start] exceeds the cstruct length,
    or if there is a bounds violation of the cstruct via [len+start]. *)

val to_string: t -> string
(** [to_string t] will allocate a fresh OCaml [string] and copy the
    contents of the cstruct into it, and return that string copy. *)

val to_bytes: t -> bytes
(** [to_bytes t] will allocate a fresh OCaml [bytes] and copy the
    contents of the cstruct into it, and return that byte copy. *)

(** {2 Debugging } *)

val hexdump: t -> unit
(** When the going gets tough, the tough hexdump their cstructs
    and peer at it until the bug disappears.  This will directly
    prettyprint the contents of the cstruct to the standard output. *)

val hexdump_to_buffer: Buffer.t -> t -> unit
(** [hexdump_to_buffer buf c] will append the pretty-printed hexdump
    of the cstruct [c] to the buffer [buf]. *)

val hexdump_pp: Format.formatter -> t -> unit
(** [hexdump_pp f c] pretty-prints a hexdump of [c] to [f]. *)

val debug: t -> string
(** [debug t] will print out the internal details of a cstruct such
    as its base offset and the length, and raise an assertion failure
    if invariants have been violated.  Not intended for casual use. *)

module BE : sig

  (** Get/set big-endian integers of various sizes. The second
      argument of those functions is the position relative to the
      current offset of the cstruct. *)

  val get_uint16: t -> int -> uint16
  (** [get_uint16 cstr off] is the 16 bit long big-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint32: t -> int -> uint32
  (** [get_uint32 cstr off] is the 32 bit long big-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint64: t -> int -> uint64
  (** [get_uint64 cstr off] is the 64 bit long big-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint16: t -> int -> uint16 -> unit
  (** [set_uint16 cstr off i] writes the 16 bit long big-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint32: t -> int -> uint32 -> unit
  (** [set_uint32 cstr off i] writes the 32 bit long big-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint64: t -> int -> uint64 -> unit
  (** [set_uint64 cstr off i] writes the 64 bit long big-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)
end

module LE : sig

  (** Get/set little-endian integers of various sizes. The second
      argument of those functions is the position relative to the
      current offset of the cstruct. *)

  val get_uint16: t -> int -> uint16
  (** [get_uint16 cstr off] is the 16 bit long little-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint32: t -> int -> uint32
  (** [get_uint32 cstr off] is the 32 bit long little-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint64: t -> int -> uint64
  (** [get_uint64 cstr off] is the 64 bit long little-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint16: t -> int -> uint16 -> unit
  (** [set_uint16 cstr off i] writes the 16 bit long little-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint32: t -> int -> uint32 -> unit
  (** [set_uint32 cstr off i] writes the 32 bit long little-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint64: t -> int -> uint64 -> unit
  (** [set_uint64 cstr off i] writes the 64 bit long little-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

end

(** {2 List of buffers} *)

val lenv: t list -> int
(** [lenv cstrs] is the combined length of all cstructs in [cstrs].
    @raise Invalid_argument if computing the sum overflows. *)

val copyv: t list -> string
(** [copyv cstrs] is the string representation of the concatenation of
    all cstructs in [cstrs].
    @raise Invalid_argument if the length of the result would
    exceed [Sys.max_string_length]. *)

val fillv: src:t list -> dst:t -> int * t list
(** [fillv ~src ~dst] copies from [src] to [dst] until [src] is exhausted or [dst] is full.
    Returns the number of bytes copied and the remaining data from [src], if any.
    This is useful if you want buffer data into fixed-sized chunks. *)

(** {2 Iterations} *)

type 'a iter = unit -> 'a option
(** Type of an iterator. *)

val iter: (t -> int option) -> (t -> 'a) -> t -> 'a iter
(** [iter lenf of_cstr cstr] is an iterator over [cstr] that returns
    elements of size [lenf cstr] and type [of_cstr cstr]. *)

val fold: ('b -> 'a -> 'b) -> 'a iter -> 'b -> 'b
(** [fold f iter acc] is [(f iterN accN ... (f iter acc)...)]. *)

val append: t -> t -> t
(** [append t1 t2] is the concatenation [t1 || t2]. *)

val concat: t list -> t
(** [concat ts] is the concatenation of all the [ts]. It is not guaranteed that
 * the result is a newly created [t] in the zero- and one-element cases. *)

val rev: t -> t
(** [rev t] is [t] in reverse order. The return value is a freshly allocated
    cstruct, and the argument is not modified. *)

(**/**)
val sum_lengths : caller:string -> t list -> int
(** [sum_lengths ~caller acc l] is [acc] plus the sum of the lengths
    of the elements of [l].  Raises [Invalid_argument caller] if
    arithmetic overflows. *)
