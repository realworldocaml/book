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

(** Computes the dependencies required for each step of the pipeline to work
    correctly on a given input. *)

module Compile : sig
  type t

  val name : t -> string
  val digest : t -> Digest.t
end

val for_compile_step : Fs.File.t -> Compile.t list
(** Takes a .cm{i,t,ti} file and returns the list of its dependencies. *)

val for_html_step : Fs.Directory.t -> (Odoc_model.Root.t list, [> msg]) result
(** Takes the directory where the .odoc files of a given package are stored and
    returns the list of roots that need to be in odoc's load path to process
    html from these .odoc files. *)
