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

(** Management of the documentation environment.

    This is the module which does the link between packages, directories and
    {!DocOck}'s needs. *)

type t
type builder

val create :
  ?important_digests:bool -> directories:(Fs.Directory.t list) -> builder
(** Prepare the environment for a given list of
    {{!Fs.Directory.t} include directories}

    @param important_digests indicate whether digests should be compared when
    doc-ock tries to lookup or fetch a unit. It defaults to [true]. *)

val build : builder -> [ `Unit of Compilation_unit.t | `Page of Page.t ] -> t
(** Initialize the environment for the given unit. *)

val resolver : t -> Odoc_xref.resolver
(** Get a resolver from an env *)

val expander : t -> Odoc_xref.expander
(** Get an expander from an env *)

(* val forward_resolver : t -> Root.t DocOck.forward_resolver *)
