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

(** Produces .html files from a .odoc file. *)

val from_odoc :
  env:Env.builder -> ?syntax:Odoc_html.Tree.syntax -> ?theme_uri:Odoc_html.Tree.uri -> output:Fs.Directory.t ->
  Fs.File.t -> (unit, [> msg]) result

val from_mld : env:Env.builder -> ?syntax:Odoc_html.Tree.syntax -> package:Odoc_model.Root.Package.t ->
  output:Fs.Directory.t -> warn_error:bool -> Fs.File.t -> (unit, [> msg]) result
