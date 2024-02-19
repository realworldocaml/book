(*
 * Copyright (c) 2015 Anil Madhavapeddy <anil@recoil.org>
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

(** Database of file extensions to MIME types from RFC2045 onwards. *)

(** [map_extension ~default e] converts the file extension [e] into a MIME type,
    defaulting to [default] (which is [application/octet-stream] by default) if
    it is unknown. *)
val map_extension : ?default:string -> string -> string

(** [map_file ~default f] converts the filename [f] into a MIME type,
    defaulting to [default] (which is [application/octet-stream] by default) if
    it is unknown. *)
val map_file : ?default:string -> string -> string

(** [map_mime m] converts the MIME type [m] into a list of acceptable
    file extensions, defaulting to an empty list if it is unknown. *)
val map_mime : string -> string list
