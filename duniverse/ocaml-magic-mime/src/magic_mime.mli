(*
 * Copyright (c) 2009-2015 Anil Madhavapeddy <anil@recoil.org>
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

(** Convert file extensions to MIME types *)

val lookup : ?default:string -> string -> string
(** [lookup ~default filename] will return a MIME type for the full [filename]
    supplied by examining its extension and look it up by using
    {!Mime_types.map_extension} or {!Mime_types.map_file} if there
    is no file extension present. *)

(** Convert MIME types to file extensions *)

val reverse_lookup : string -> string list
(** [reverse_lookup mime] will return a list of file extensions for the
    MIME type supplied by stripping any parameters and looking it up by
    using {!Mime_types.map_mime}.
    If an unknown MIME type is supplied, empty list is returned. *)
