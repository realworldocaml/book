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

type t

val empty : t

val add_page : Lang.Page.t -> t -> t

val add_unit : Lang.Compilation_unit.t -> t -> t

val add_signature_items : Lang.Signature.t -> t -> t

val add_module_type_expr_items : Lang.ModuleType.expr -> t -> t

val add_module_decl_items : Lang.Module.decl -> t -> t

val add_class_signature_items : Lang.ClassSignature.t -> t -> t

val add_class_type_expr_items : Lang.ClassType.expr -> t -> t

val add_class_decl_items : Lang.Class.decl -> t -> t

val lookup_module : t -> Reference.Module.t -> Reference.Module.t

val lookup_module_type : t -> Reference.ModuleType.t -> Reference.ModuleType.t

val lookup_type : t -> Reference.Type.t -> Reference.Type.t

val lookup_constructor : t -> Reference.Constructor.t -> Reference.Constructor.t

val lookup_field : t -> Reference.Field.t -> Reference.Field.t

val lookup_extension : t -> Reference.Extension.t -> Reference.Extension.t

val lookup_exception : t -> Reference.Exception.t -> Reference.Exception.t

val lookup_value : t -> Reference.Value.t -> Reference.Value.t

val lookup_class : t -> Reference.Class.t -> Reference.Class.t

val lookup_class_type : t -> Reference.ClassType.t -> Reference.ClassType.t

val lookup_method : t -> Reference.Method.t -> Reference.Method.t

val lookup_instance_variable : t -> Reference.InstanceVariable.t ->
  Reference.InstanceVariable.t

val lookup_label : t -> Reference.Label.t -> Reference.Label.t

val lookup_element : t -> Reference.t -> Reference.t

val lookup_section_title : t -> Reference.Resolved.Label.t ->
  Odoc_model.Comment.link_content option
