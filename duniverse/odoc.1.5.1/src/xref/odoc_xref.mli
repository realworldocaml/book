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

(** {2:resolving Resolving}

    This is the part of DocOck handling the resolving of path and references. *)

type resolver

type lookup_result_found = { root : Odoc_model.Root.t; hidden : bool }

type lookup_result =
  | Forward_reference
  | Found of lookup_result_found
  | Not_found

type msg = [ `Msg of string ]

(** Build a resolver. Optionally provide equality and hash on ['a]. *)
val build_resolver :
  ?equal:(Odoc_model.Root.t -> Odoc_model.Root.t -> bool) -> ?hash:(Odoc_model.Root.t -> int)
  -> (string -> lookup_result)
  -> (Odoc_model.Root.t -> (Odoc_model.Lang.Compilation_unit.t, msg) Result.result)
  -> (string -> Odoc_model.Root.t option)
  -> (Odoc_model.Root.t -> (Odoc_model.Lang.Page.t, msg) Result.result)
  -> resolver

val resolve :
  resolver
  -> Odoc_model.Lang.Compilation_unit.t
  -> (Odoc_model.Lang.Compilation_unit.t, [> msg]) Result.result

val resolve_page :
  resolver
  -> Odoc_model.Lang.Page.t
  -> (Odoc_model.Lang.Page.t, [> msg]) Result.result

(** {2:expansion Expansion}

    This is the part of DocOck in charge of performing substitutions, inlining
    of includes, etc. *)

type expander

(** Build an expander. Assumes that it is safe to use {!Hashtbl.hash} and
    structural equality (=) on ['a]. *)
val build_expander :
  ?equal:(Odoc_model.Root.t -> Odoc_model.Root.t -> bool) -> ?hash:(Odoc_model.Root.t -> int)
  -> (string -> lookup_result)
  -> (root:Odoc_model.Root.t -> Odoc_model.Root.t -> (Odoc_model.Lang.Compilation_unit.t, msg) Result.result)
  -> expander

val expand :
  expander
  -> Odoc_model.Lang.Compilation_unit.t
  -> (Odoc_model.Lang.Compilation_unit.t, [> msg]) Result.result

(** {2 Misc.}

    OCaml's predefined types and exceptions. *)

val core_types : Odoc_model.Lang.TypeDecl.t list

val core_exceptions : Odoc_model.Lang.Exception.t list

module Lookup = Lookup
