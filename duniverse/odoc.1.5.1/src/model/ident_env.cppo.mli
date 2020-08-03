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

open Names

type t

val empty : t

#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
val module_name_of_open : 'a Typedtree.open_infos -> string
#endif

val add_module : Paths.Identifier.Signature.t -> Ident.t -> ModuleName.t -> t -> t

val add_argument : Paths.Identifier.Signature.t -> int -> Ident.t -> ArgumentName.t -> t -> t

val add_module_type : Paths.Identifier.Signature.t -> Ident.t -> ModuleTypeName.t -> t -> t

val add_type : Paths.Identifier.Signature.t -> Ident.t -> TypeName.t -> t -> t

val add_class : Paths.Identifier.Signature.t -> Ident.t -> Ident.t -> Ident.t -> Ident.t -> ClassName.t -> t -> t

val add_class_type : Paths.Identifier.Signature.t -> Ident.t -> Ident.t -> Ident.t -> ClassTypeName.t -> t -> t

val add_signature_type_items : Paths.Identifier.Signature.t -> Compat.signature -> t -> t

val add_signature_tree_items : Paths.Identifier.Signature.t -> Typedtree.signature -> t -> t

val add_structure_tree_items : Paths.Identifier.Signature.t -> Typedtree.structure -> t -> t

module Path : sig

  val read_module : t -> Path.t -> Paths.Path.Module.t

  val read_module_type : t -> Path.t -> Paths.Path.ModuleType.t

  val read_type : t -> Path.t -> Paths.Path.Type.t

  val read_class_type : t -> Path.t -> Paths.Path.ClassType.t

end


module Fragment : sig

  val read_module : Longident.t -> Paths.Fragment.Module.t

  val read_type : Longident.t -> Paths.Fragment.Type.t

end
