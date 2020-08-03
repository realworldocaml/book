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
open Paths

(** {3 Tables} *)

(** The type of tables of components *)
type t

type lookup_result_found = { root : Odoc_model.Root.t; hidden : bool }

(* FIXME: use different types for unit and page lookups. *)
type lookup_unit_result =
  | Forward_reference
  | Found of lookup_result_found
  | Not_found

(** Create a table of the components of units. Optionally provide
    equality and hash functons. *)
val create: ?equal:(Root.t -> Root.t -> bool) -> ?hash:(Root.t -> int) ->
  (string -> lookup_unit_result) -> (Root.t -> Lang.Compilation_unit.t) ->
  (string -> Root.t option) -> (Root.t -> Lang.Page.t) ->
  t

open Components

(** {3 Identifier Lookup} *)

(** Lookup the components of a signature identifier *)
val signature_identifier : t -> Identifier.Signature.t -> Sig.t

(** Lookup the components of a class signature identifier *)
val class_signature_identifier : t -> Identifier.ClassSignature.t ->
      ClassSig.t

(** Lookup the components of a datatype identifier *)
val datatype_identifier : t -> Identifier.Type.t -> Datatype.t

(** {3 Path Lookup} *)

(** Lookup the components of a resolved module path *)
val resolved_module_path : t -> Path.Resolved.Module.t -> Sig.t

(** Lookup the components of a resolved module type path *)
val resolved_module_type_path : t -> Path.Resolved.ModuleType.t -> Sig.t

(** Lookup the components of a resolved class type path *)
val resolved_class_type_path : t ->
      Path.Resolved.ClassType.t -> ClassSig.t

(** Lookup the components of a module path, needed for module
    applications. *)
val module_path : t -> Path.Module.t -> Sig.t

(** {3 Fragment Lookup} *)

(** Table specialised to lookup fragments based on a module expression
    or path. *)
type with_

(** Create specialised fragment table for a module type expression *)
val module_type_expr_with : t ->
      Identifier.Signature.t -> Lang.ModuleType.expr -> with_

(** Create specialised fragment table for a module path *)
val module_type_path_with : t ->
      Path.ModuleType.t -> with_

(** Lookup the components of a resolved module fragment *)
val resolved_signature_fragment : with_ ->
      Fragment.Resolved.Signature.t -> Sig.t

(** {3 Reference Lookup} *)

(** Lookup the components of a resolved signature reference *)
val resolved_signature_reference : t ->
      Reference.Resolved.Signature.t -> Sig.t

(** Lookup the components of a resolved class signature reference *)
val resolved_class_signature_reference : t ->
      Reference.Resolved.ClassSignature.t -> ClassSig.t

(** Lookup the components of a resolved datatype reference *)
val resolved_datatype_reference : t ->
  Reference.Resolved.DataType.t -> Datatype.t

val resolved_page_reference : t -> Reference.Resolved.Page.t -> Page.t

(** {3 Root lookup} *)

(** Lookup the base of a unit name *)
val base : t -> string -> lookup_unit_result

(** Lookup the base of a page name *)
val page_base : t -> string -> Root.t option
