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

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
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
    at offset [off] (default [0]) of length [len]
    (default [Bigarray.Array1.dim b - off]). *)

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
    slice located at offset [off] (default [0]) and of length [len] (default
    [String.length str - off]),
    with the underlying buffer allocated by [alloc]. If [allocator] is not
    provided, [create] is used.

    @raise Invalid_argument if [off] or [len] is negative, or
    [String.length str - off] < [len].
*)

val of_bytes: ?allocator:(int -> t) -> ?off:int -> ?len:int -> bytes -> t
(** [of_bytes ~allocator byt] is the cstruct representation of [byt]
    slice located at offset [off] (default [0]) and of length [len] (default
    [Bytes.length byt - off]),
    with the underlying buffer allocated by [alloc]. If [allocator] is not
    provided, [create] is used.

    @raise Invalid_argument if [off] or [len] is negative, or
    [Bytes.length str - off] < [len]. *)

val of_hex: ?off:int -> ?len:int -> string -> t
(** [of_hex ~off ~len str] is the cstruct [cs].  Every pair of hex-encoded
    characters in [str] starting at offset [off] (default [0]) of length [len]
    (default [String.length str - off]) are converted to one byte in [cs].
    Whitespaces (space, newline, tab, carriage return) in [str] are skipped.

    @raise Invalid_argument if the input string contains invalid characters or
    has an odd numbers of non-whitespace characters, or if [off] or [len] are
    negative, or [String.length str - off] < [len]. *)

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

val memset: t -> int -> unit
(** [memset t x] sets all the bytes of [t] to [x land 0xff]. *)

val split: ?start:int -> t -> int -> t * t
(** [split ~start cstr len] is a tuple containing the cstruct
    extracted from [cstr] at offset [start] (default: 0) of length
    [len] as first element, and the rest of [cstr] as second
    element.
    @raise Invalid_argument if [start] exceeds the cstruct length,
    or if there is a bounds violation of the cstruct via [len+start]. *)

val to_string: ?off:int -> ?len:int -> t -> string
(** [to_string ~off ~len t] will allocate a fresh OCaml [string] and copy the
    contents of the cstruct starting at offset [off] (default [0]) of length
    [len] (default [Cstruct.len t - off]) into it, and return that string.

    @raise Invalid_argument if [off] or [len] is negative, or
    [Cstruct.len str - off] < [len]. *)

val to_bytes: ?off:int -> ?len:int -> t -> bytes
(** [to_bytes ~off ~len t] will allocate a fresh OCaml [bytes] and copy the
    contents of the cstruct starting at offset [off] (default [0]) of length
    [len] (default [Cstruct.len t - off]) into it, and return that bytes.

    @raise Invalid_argument if [off] or [len] is negative, or
    [Cstruct.len str - off] < [len]. *)

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

module HE : sig

  (** Get/set host-endian integers of various sizes. The second
      argument of those functions is the position relative to the
      current offset of the cstruct. *)

  val get_uint16: t -> int -> uint16
  (** [get_uint16 cstr off] is the 16 bit long host-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint32: t -> int -> uint32
  (** [get_uint32 cstr off] is the 32 bit long host-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint64: t -> int -> uint64
  (** [get_uint64 cstr off] is the 64 bit long host-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint16: t -> int -> uint16 -> unit
  (** [set_uint16 cstr off i] writes the 16 bit long host-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint32: t -> int -> uint32 -> unit
  (** [set_uint32 cstr off i] writes the 32 bit long host-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint64: t -> int -> uint64 -> unit
  (** [set_uint64 cstr off i] writes the 64 bit long host-endian
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

val shiftv: t list -> int -> t list
(** [shiftv ts n] is [ts] without the first [n] bytes.
    It has the property that [equal (concat (shiftv ts n)) (shift (concat ts) n)].
    This operation is fairly fast, as it will share the tail of the list.
    The first item in the returned list is never an empty cstruct,
    so you'll get [[]] if and only if [lenv ts = n]. *)

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

(** {1 Helpers to parse.}

    [Cstruct] is used to manipulate {i payloads} which can be formatted
   according an {{:https://perdu.com/}RFC} or an user-defined format. In such context, this module
   provides utilities to be able to easily {i parse} {i payloads}.

    Due to the type {!Cstruct.t}, no copy are done when you use these utilities
   and you are able to extract your information without a big performance cost.

    More precisely, each values returned by these utilities will be located into
   the minor-heap where the base buffer will never be copied or relocated.

    For instance, to parse a Git tree object:

{v
  entry := perm ' ' name '\000' 20byte
  tree  := entry *
v}

    {[
      open Cstruct

      let ( >>= ) = Option.bind

      let rec hash_of_name ~name payload =
        if is_empty payload then raise Not_found
        else
          cut ~sep:(v " ") payload >>= fun (_, payload) ->
          cut ~sep:(v "\000") payload >>= fun (name', payload) ->
          if name = name' then with_range ~len:20 payload
          else hash_of_name ~name (shift payload 20)
    ]}

    A [Cstruct] defines a possibly empty subsequence of bytes in a {e base}
   buffer (a {!Bigarray.Array1.t}).

    The positions of a buffer [b] of length [l] are the slits found
   before each byte and after the last byte of the buffer. They are
   labelled from left to right by increasing number in the range \[[0];[l]\].

{v
positions  0   1   2   3   4    l-1    l
           +---+---+---+---+     +-----+
  indices  | 0 | 1 | 2 | 3 | ... | l-1 |
           +---+---+---+---+     +-----+
v}

    The [i]th byte index is between positions [i] and [i+1].

    Formally we define a subbuffer of [b] as being a subsequence
   of bytes defined by a {e off} position and a {e len} number. When
   [len] is [0] the subbuffer is {e empty}. Note that for a given
   base buffer there are as many empty subbuffers as there are positions
   in the buffer.

    Like in strings, we index the bytes of a subbuffer using zero-based
   indices.
*)

val get : t -> int -> char
(** [get cs zidx] is the byte of [cs] at its zero-based index [zidx].
    It's an alias of {!get_char}.

    @raise Invalid_argument if [zidx] is not an index of [cs]. *)

val get_byte : t -> int -> int
(** [get_byte cs zidx] is [Char.code (get cs zidx)]. It's an alias of {!get_uint8}. *)

val string : ?off:int -> ?len:int -> string -> t
(** [string ~off ~len str] is the subbuffer of [str] that starts at position [off]
   (defaults to [0]) and stops at position [off + len] (defaults to
   [String.length str]). [str] is fully-replaced by an fresh allocated
   {!Cstruct.buffer}.

    @raise Invalid_argument if [off] or [off + len] are not positions of [str].
*)

val buffer : ?off:int -> ?len:int -> buffer -> t
(** [buffer ~off ~len buffer] is the sub-part of [buffer] that starts at
   position [off] (default to [0]) and stops at position [off + len] (default to
   [Bigarray.Array1.dim buffer]). [buffer] is used as the base buffer of the
   returned value (no major-heap allocation are performed).

    @raise Invalid_argument if [off] or [off + len] are not positions of
   [buffer]. *)

val start_pos : t -> int
(** [start_pos cs] is [cs]'s start position in the base {!Cstruct.buffer}. *)

val stop_pos : t -> int
(** [stop_pos cs] is [cs]'s stop position in the base {!Cstruct.buffer}. *)

val length : t -> int
(** Returns the length of the current cstruct view.  Note that this
    length is potentially smaller than the actual size of the underlying
    buffer, as the [sub] function can construct a smaller view. *)

val head : ?rev:bool -> t -> char option
(** [head cs] is [Some (get cs h)] with [h = 0] if [rev = false] (default) or [h
   = length cs - 1] if [rev = true]. [None] is returned if [cs] is empty. *)

val tail : ?rev:bool -> t -> t
(** [tail cs] is [cs] without its first ([rev] is [false], default) or last
   ([rev] is [true]) byte or [cs] is empty. *)

val is_empty : t -> bool
(** [is_empty cs] is [length cs = 0]. *)

val is_prefix : affix:t -> t -> bool
(** [is_prefix ~affix cs] is [true] iff [affix.[zidx] = cs.[zidx]] for all
   indices [zidx] of [affix]. *)

val is_suffix : affix:t -> t -> bool
(** [is_suffix ~affix cs] is [true] iff [affix.[n - zidx] = cs.[m - zidx]] for
   all indices [zidx] of [affix] with [n = length affix - 1] and [m = length cs
   - 1]. *)

val is_infix : affix:t -> t -> bool
(** [is_infix ~affix cs] is [true] iff there exists an index [z] in [cs] such
   that for all indices [zidx] of [affix] we have [affix.[zidx] = cs.[z +
   zidx]]. *)

val for_all : (char -> bool) -> t -> bool
(** [for_all p cs] is [true] iff for all indices [zidx] of [cs], [p cs.[zidx] =
   true]. *)

val exists : (char -> bool) -> t -> bool
(** [exists p cs] is [true] iff there exists an index [zidx] of [cs] with [p
   cs.[zidx] = true]. *)

val start : t -> t
(** [start cs] is the empty sub-part at the start position of [cs]. *)

val stop : t -> t
(** [stop cs] is the empty sub-part at the stop position of [cs]. *)

val trim : ?drop:(char -> bool) -> t -> t
(** [trim ~drop cs] is [cs] with prefix and suffix bytes satisfying [drop] in
   [cs] removed. [drop] defaults to [function ' ' | '\r' .. '\t' -> true | _ ->
   false]. *)

val span : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t * t
(** [span ~rev ~min ~max ~sat cs] is [(l, r)] where:

    {ul
    {- if [rev] is [false] (default), [l] is at least [min] and at most
       [max] consecutive [sat] satisfying initial bytes of [cs] or {!empty}
       if there are no such bytes. [r] are the remaining bytes of [cs].}
    {- if [rev] is [true], [r] is at least [min] and at most [max]
       consecutive [sat] satisfying final bytes of [cs] or {!empty}
       if there are no such bytes. [l] are the remaining bytes of [cs].}}

    If [max] is unspecified the span is unlimited. If [min] is unspecified
    it defaults to [0]. If [min > max] the condition can't be satisfied and
    the left or right span, depending on [rev], is always empty. [sat]
    defaults to [(fun _ -> true)].

    The invariant [l ^ r = s] holds.

    For instance, the {i ABNF} expression:

{v
  time := 1*10DIGIT
v}

    can be translated to:

    {[
      let (time, _) = span ~min:1 ~max:10 is_digit cs in
    ]}

    @raise Invalid_argument if [max] or [min] is negative. *)

val take : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
(** [take ~rev ~min ~max ~sat cs] is the matching span of {!span} without the remaining one.
    In other words:

    {[(if rev then snd else fst) @@ span ~rev ~min ~max ~sat cs]} *)

val drop : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
(** [drop ~rev ~min ~max ~sat cs] is the remaining span of {!span} without the matching one.
    In other words:

    {[(if rev then fst else snd) @@ span ~rev ~min ~max ~sat cs]} *)

val cut : ?rev:bool -> sep:t -> t -> (t * t) option
(** [cut ~sep cs] is either the pair [Some (l, r)] of the two
    (possibly empty) sub-buffers of [cs] that are delimited by the first
    match of the non empty separator string [sep] or [None] if [sep] can't
    be matched in [cs]. Matching starts from the beginning of [cs] ([rev] is
    [false], default) or the end ([rev] is [true]).

    The invariant [l ^ sep ^ r = s] holds.

    For instance, the {i ABNF} expression:

{v
  field_name := *PRINT
  field_value := *ASCII
  field := field_name ":" field_value
v}

    can be translated to:

    {[
      match cut ~sep:":" value with
      | Some (field_name, field_value) -> ...
      | None -> invalid_arg "invalid field"
    ]}

    @raise Invalid_argument if [sep] is the empty buffer. *)

val cuts : ?rev:bool -> ?empty:bool -> sep:t -> t -> t list
(** [cuts ~sep cs] is the list of all sub-buffers of [cs] that are
    delimited by matches of the non empty separator [sep]. Empty sub-buffers are
    omitted in the list if [empty] is [false] (default to [true]).

    Matching separators in [cs] starts from the beginning of [cs]
    ([rev] is [false], default) or the end ([rev] is [true]). Once
    one is found, the separator is skipped and matching starts again,
    that is separator matches can't overlap. If there is no separator
    match in [cs], the list [[cs]] is returned.

    The following invariants hold:
    {ul
    {- [concat ~sep (cuts ~empty:true ~sep cs) = cs]}
    {- [cuts ~empty:true ~sep cs <> []]}}

    For instance, the {i ABNF} expression:

{v
  arg := *(ASCII / ",") ; any characters exclude ","
  args := arg *("," arg)
v}

    can be translated to:

    {[
      let args = cuts ~sep:"," buffer in
    ]}

    @raise Invalid_argument if [sep] is the empty buffer. *)

val fields : ?empty:bool -> ?is_sep:(char -> bool) -> t -> t list
(** [fields ~empty ~is_sep cs] is the list of (possibly empty)
    sub-buffers that are delimited by bytes for which [is_sep] is
    [true]. Empty sub-buffers are omitted in the list if [empty] is
    [false] (defaults to [true]). [is_sep c] if it's not define by the
    user is [true] iff [c] is an US-ASCII white space character,
    that is one of space [' '] ([0x20]), tab ['\t'] ([0x09]), newline
    ['\n'] ([0x0a]), vertical tab ([0x0b]), form feed ([0x0c]), carriage
    return ['\r'] ([0x0d]). *)

val find : ?rev:bool -> (char -> bool) -> t -> t option
(** [find ~rev sat cs] is the sub-buffer of [cs] (if any) that spans
    the first byte that satisfies [sat] in [cs] after position [start cs]
    ([rev] is [false], default) or before [stop cs] ([rev] is [true]).
    [None] is returned if there is no matching byte in [s]. *)

val find_sub : ?rev:bool -> sub:t -> t -> t option
(** [find_sub ~rev ~sub cs] is the sub-buffer of [cs] (if any) that spans
    the first match of [sub] in [cs] after position [start cs]
    ([rev] is [false], default) or before [stop cs] ([rev] is [true]).
    Only bytes are compared and [sub] can be on a different base buffer.
    [None] is returned if there is no match of [sub] in [s]. *)

val filter : (char -> bool) -> t -> t
(** [filter sat cs] is the buffer made of the bytes of [cs] that satisfy [sat],
    in the same order. *)

val filter_map : (char -> char option) -> t -> t
(** [filter_map f cs] is the buffer made of the bytes of [cs] as mapped by
    [f], in the same order. *)

val map : (char -> char) -> t -> t
(** [map f cs] is [cs'] with [cs'.[i] = f cs.[i]] for all indices [i]
    of [cs]. [f] is invoked in increasing index order. *)

val mapi : (int -> char -> char) -> t -> t
(** [map f cs] is [cs'] with [cs'.[i] = f i cs.[i]] for all indices [i]
    of [cs]. [f] is invoked in increasing index order. *)

(**/**)
val sum_lengths : caller:string -> t list -> int
(** [sum_lengths ~caller acc l] is [acc] plus the sum of the lengths
    of the elements of [l].  Raises [Invalid_argument caller] if
    arithmetic overflows. *)
