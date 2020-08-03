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



module Paths = Odoc_model.Paths
module Ident_env = Odoc_model.Ident_env



val read_interface: Odoc_model.Root.t -> string -> Odoc_model.Compat.signature ->
  Paths.Identifier.Module.t *
  Odoc_model.Comment.docs *
  Odoc_model.Lang.Signature.t

#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
val read_label : Asttypes.label -> Odoc_model.Lang.TypeExpr.label option
#else
val read_label : Asttypes.arg_label -> Odoc_model.Lang.TypeExpr.label option
#endif

val mark_type_expr : Types.type_expr -> unit

val read_type_expr : Ident_env.t ->
                     Types.type_expr -> Odoc_model.Lang.TypeExpr.t

val mark_type_extension : Types.type_expr list ->
                          Types.extension_constructor list ->
                          Types.type_expr list

val read_type_parameter : bool -> Types.Variance.t ->
                          Types.type_expr -> Odoc_model.Lang.TypeDecl.param

val mark_class_declaration : Types.class_declaration -> unit

val read_self_type : Types.type_expr -> Odoc_model.Lang.TypeExpr.t option

val read_type_constraints : Ident_env.t -> Types.type_expr list ->
                            (Odoc_model.Lang.TypeExpr.t
                             * Odoc_model.Lang.TypeExpr.t) list

val read_class_signature : Ident_env.t ->
                           Paths.Identifier.ClassSignature.t ->
                           Types.type_expr list -> Types.class_type ->
                           Odoc_model.Lang.ClassType.expr

val read_class_type : Ident_env.t ->
                      Paths.Identifier.ClassSignature.t ->
                      Types.type_expr list -> Types.class_type ->
                      Odoc_model.Lang.Class.decl

val read_module_type : Ident_env.t ->
                       Paths.Identifier.Signature.t -> int ->
                       Odoc_model.Compat.module_type -> Odoc_model.Lang.ModuleType.expr

val read_signature : Ident_env.t ->
                     Paths.Identifier.Signature.t ->
                     Odoc_model.Compat.signature -> Odoc_model.Lang.Signature.t

val read_extension_constructor : Ident_env.t ->
                       Paths.Identifier.Signature.t ->
                       Ident.t -> Types.extension_constructor ->
                       Odoc_model.Lang.Extension.Constructor.t

val read_exception : Ident_env.t ->
  Paths.Identifier.Signature.t -> Ident.t ->
  Types.extension_constructor -> Odoc_model.Lang.Exception.t
