(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
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

open Odoc_model

type resolver

(** Lazily extract the components of units. Assumes that it is safe to
    use {!Hashtbl.hash} and structural equality (=) on ['a]. *)
val build_resolver: ?equal:(Root.t -> Root.t -> bool) -> ?hash:(Root.t -> int)
  -> (string -> Component_table.lookup_unit_result)
  -> (Root.t -> Lang.Compilation_unit.t)
  -> (string -> Root.t option) -> (Root.t -> Lang.Page.t)
  -> resolver

(** Try to resolve all paths and references within a unit. *)
val resolve : resolver -> Lang.Compilation_unit.t -> Lang.Compilation_unit.t

(** Try to resolve all paths and references within a page. *)
val resolve_page : resolver -> Lang.Page.t -> Lang.Page.t
