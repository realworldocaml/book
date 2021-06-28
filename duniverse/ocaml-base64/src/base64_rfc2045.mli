(*
 * Copyright (c) 2014-2016 Anil Madhavapeddy <anil@recoil.org>
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

(** Decode *)

val default_alphabet : string
(** A 64-character string specifying the regular Base64 alphabet. *)

(** The type for decoders. *)
type decoder

(** The type for input sources. With a [`Manual] source the client must provide
    input with {!src}. *)
type src = [`Manual | `Channel of in_channel | `String of string]

type decode =
  [`Await | `End | `Flush of string | `Malformed of string | `Wrong_padding]

val src : decoder -> Bytes.t -> int -> int -> unit
(** [src d s j l] provides [d] with [l] bytes to read, starting at [j] in [s].
    This byte range is read by calls to {!decode} with [d] until [`Await] is
    returned. To signal the end of input, call the function with [l = 0]. *)

val decoder : src -> decoder
(** [decoder src] is a decoder that inputs from [src]. *)

val decode : decoder -> decode
(** [decode d] is: {ul {- [`Await] if [d] has a [`Manual] input source and
    awaits for more input. The client must use {!src} to provide it.} {- [`End]
    if the end of input was reached} {- [`Malformed bytes] if the [bytes]
    sequence is malformed according to the decoded base64 encoding scheme. If
    you are interested in a best-effort decoding, you can still continue to
    decode after an error until the decode synchronizes again on valid bytes.}
    {- [`Flush data] if a [data] sequence value was decoded.} {-
    [`Wrong_padding] if decoder retrieve a wrong padding at the end of the
    input.}}

    {b Note}. Repeated invocation always eventually returns [`End], even in
    case of errors. *)

val decoder_byte_count : decoder -> int
(** [decoder_byte_count d] is the number of characters already decoded on [d]
    (included malformed ones). This is the last {!decode}'s end output offset
    counting from beginning of the stream. *)

val decoder_src : decoder -> src
(** [decoder_src d] is [d]'s input source. *)

val decoder_dangerous : decoder -> bool
(** [decoder_dangerous d] returns [true] if encoded input does not respect the
    80-columns rule. If your are interested in a best-effort decoding you can
    still continue to decode even if [decoder_dangerous d] returns [true].
    Nothing grow automatically internally in this state. *)

(** The type for output destinations. With a [`Manual] destination the client
    must provide output storage with {!dst}. *)
type dst = [`Channel of out_channel | `Buffer of Buffer.t | `Manual]

type encode = [`Await | `End | `Char of char]

(** The type for Base64 (RFC2045) encoder. *)
type encoder

val encoder : dst -> encoder
(** [encoder dst] is an encoder for Base64 (RFC2045) that outputs to [dst]. *)

val encode : encoder -> encode -> [`Ok | `Partial]
(** [encode e v]: is {ul {- [`Partial] iff [e] has a [`Manual] destination and
    needs more output storage. The client must use {!dst} to provide a new
    buffer and then call {!encode} with [`Await] until [`Ok] is returned.} {-
    [`Ok] when the encoder is ready to encode a new [`Char] or
    [`End]}}

    For [`Manual] destination, encoding [`End] always return [`Partial], the
    client should continue as usual with [`Await] until [`Ok] is returned at
    which point {!dst_rem} [encoder] is guaranteed to be the size of the last
    provided buffer (i.e. nothing was written).

    {b Raises.} [Invalid_argument] if a [`Char] or [`End] is
    encoded after a [`Partial] encode. *)

val encoder_dst : encoder -> dst
(** [encoder_dst encoder] is [encoder]'s output destination. *)

val dst : encoder -> Bytes.t -> int -> int -> unit
(** [dst e s j l] provides [e] with [l] bytes to write, starting at [j] in [s].
    This byte range is written by calls to {!encode} with [e] until [`Partial]
    is returned. Use {!dst_rem} to know the remaining number of non-written
    free bytes in [s]. *)

val dst_rem : encoder -> int
(** [dst_rem e] is the remaining number of non-written, free bytes in the last
    buffer provided with {!dst}. *)
