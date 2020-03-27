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

val signature : t -> Lang.Signature.t -> Lang.Signature.t

val class_signature : t -> Lang.ClassSignature.t -> Lang.ClassSignature.t

val datatype : t -> Lang.TypeDecl.Representation.t ->
               Lang.TypeDecl.Representation.t

val module_ : t -> Lang.Module.t -> Lang.Module.t

val module_type : t -> Lang.ModuleType.t -> Lang.ModuleType.t

val type_decl : t -> Lang.TypeDecl.t -> Lang.TypeDecl.t

val constructor : t -> Lang.TypeDecl.Constructor.t ->
                  Lang.TypeDecl.Constructor.t

val field : t -> Lang.TypeDecl.Field.t -> Lang.TypeDecl.Field.t

val extension : t -> Lang.Extension.t -> Lang.Extension.t

val exception_ : t -> Lang.Exception.t -> Lang.Exception.t

val value : t -> Lang.Value.t -> Lang.Value.t

val class_ : t -> Lang.Class.t -> Lang.Class.t

val class_type : t -> Lang.ClassType.t -> Lang.ClassType.t

val method_ : t -> Lang.Method.t -> Lang.Method.t

val instance_variable : t -> Lang.InstanceVariable.t ->
                        Lang.InstanceVariable.t

val comment : t -> Odoc_model.Comment.docs_or_stop -> Odoc_model.Comment.docs_or_stop

val documentation : t -> Odoc_model.Comment.docs -> Odoc_model.Comment.docs

val identifier_module : t -> Identifier.Module.t ->
                        Identifier.Module.t

val identifier_signature : t -> Identifier.Signature.t ->
                        Identifier.Signature.t

val offset_identifier_signature : t -> Identifier.Signature.t * int ->
                                  Identifier.Signature.t * int

val module_type_expr : t -> Lang.ModuleType.expr -> Lang.ModuleType.expr

val module_expansion : t -> Lang.Module.expansion -> Lang.Module.expansion

val rename_signature : equal:(Root.t -> Root.t -> bool) ->
                       Identifier.Signature.t ->
                       Identifier.Signature.t ->
                       int -> t

val rename_class_signature : equal:(Root.t -> Root.t -> bool) ->
                             Identifier.ClassSignature.t ->
                             Identifier.ClassSignature.t ->
                             t

val rename_datatype : equal:(Root.t -> Root.t -> bool) ->
                      Identifier.DataType.t ->
                      Identifier.DataType.t ->
                      t

val prefix : equal:(Root.t -> Root.t -> bool) ->
             canonical:(Path.Module.t * Reference.Module.t) option ->
             Identifier.Module.t ->
             t

val strengthen : Path.Resolved.Module.t -> t

val pack : equal:(Root.t -> Root.t -> bool) -> hash:(Root.t -> int) ->
           (Identifier.Module.t * Identifier.Module.t) list -> t
