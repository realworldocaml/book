(*
 * Copyright (c) 2014 Leo White <leo@lpw25.net>
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

open Or_error

(** Produces html fragment files from a mld file. *)

val from_mld : xref_base_uri:string -> env:Env.builder -> output:Fs.File.t ->
  warn_error:bool -> Fs.File.t -> (unit, [> msg]) result
(** [from_mld ~xref_base_uri ~env ~output input] parses the content of the [input]
    file as a documentation page ({e i.e.} the ocamldoc syntax), generates the
    equivalent HTML representation and writes the result into the [output]
    file. The produced file is an HTML fragment that can be embedded into other
    documents.

    Cross-reference resolution uses the provided [xref_base_uri] to locate docset
    packages. *)
