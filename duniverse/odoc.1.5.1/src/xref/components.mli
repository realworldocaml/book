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

module rec Sig : sig

  type t

  (** {3 Parents} *)

  val find_parent_module : string -> t -> Parent.module_

  val find_parent_apply : (Path.Module.t -> t) -> Path.Module.t ->
        t -> Parent.module_

  val find_parent_module_type : string -> t -> Parent.module_type

  val find_parent_signature : string -> t -> Parent.signature

  val find_parent_class_signature : string -> t -> Parent.class_signature

  val find_parent_datatype : string -> t -> Parent.datatype

  val find_parent_sig_or_type : string -> t -> Parent.sig_or_type

  val find_parent_subst : t -> Parent.subst

  val find_parent : string -> t -> Parent.any

  (** {3 Elements} *)

  val find_module_element : string -> t -> Element.signature_module

  val find_apply_element : t -> Element.signature_module

  val find_module_type_element : string -> t -> Element.signature_module_type

  val find_type_element : string -> t -> Element.signature_type

  val find_constructor_element : string -> t -> Element.signature_constructor

  val find_field_element : string -> t -> Element.signature_field

  val find_extension_element : string -> t -> Element.signature_extension

  val find_exception_element : string -> t -> Element.signature_exception

  val find_value_element : string -> t -> Element.signature_value

  val find_class_element : string -> t -> Element.signature_class

  val find_class_type_element : string -> t -> Element.signature_class_type

  val find_label_element : string -> t -> Element.signature_label

  val find_element : string -> t -> Element.signature

  val find_section_title : string -> t -> Odoc_model.Comment.link_content

  (** {3 Lookup} *)

  val lookup_module : string -> t -> t

  val lookup_argument : int -> t -> t

  val lookup_apply : (Path.Module.t -> t) -> Path.Module.t -> t -> t

  val lookup_module_type  : string -> t -> t

  val lookup_class_type : string -> t -> ClassSig.t

  val lookup_datatype : string -> t -> Datatype.t

  (** {3 Constructors} *)

  type signature

  val empty : signature

  val add_module : string -> t -> signature -> signature

  val add_module_type : string -> t -> signature -> signature

  val add_datatype : string -> Datatype.t -> signature -> signature

  val add_class : string -> ClassSig.t -> signature -> signature

  val add_class_type : string -> ClassSig.t -> signature -> signature

  val add_element : string -> Element.signature -> signature -> signature

  val add_documentation : Odoc_model.Comment.docs -> signature -> signature

  val add_comment : Odoc_model.Comment.docs_or_stop -> signature -> signature

  val include_ : t -> signature -> signature

  val modules : t -> (Odoc_model.Names.ModuleName.t * t) list

  val module_types : t -> (Odoc_model.Names.ModuleTypeName.t * t) list

  val path : (Path.ModuleType.t -> t) -> Path.ModuleType.t -> t

  val alias : (Path.Module.t -> t) -> Path.Module.t -> t

  val signature : ('b -> signature) -> 'b -> t

  val functor_ : (Root.t -> Root.t -> bool) option -> (Root.t -> int) option ->
                 Identifier.Module.t -> t -> t -> t

  val generative : t -> t

  val abstract : t

  val unresolved : t

  val with_module : Fragment.Module.t -> t -> t -> t

  val with_module_subst : Fragment.Module.t -> t -> t

  val with_type_subst : Fragment.Type.t -> t -> t

  (** {3 Aliases handling} *)

  val set_canonical :
    t -> (Path.Module.t * Reference.Module.t) option -> t

  val get_canonical :
    t -> (Path.Module.t * Reference.Module.t) option

  (** {3 Hidding} *)

  val set_hidden : t -> bool -> t

  val get_hidden : t -> bool

end

and Datatype : sig

  type t

  (** {3 Elements} *)

  val find_constructor_element : string -> t -> Element.datatype_constructor

  val find_field_element : string -> t -> Element.datatype_field

  val find_label_element : string -> t -> Element.datatype_label

  val find_element : string -> t -> Element.datatype

  (** {3 Constructors} *)

  val add_documentation : Odoc_model.Comment.docs -> t -> t

  val abstract : t

  val variant : string -> string list -> t

  val record : string -> string list -> t

  val extensible : t

  val unresolved : t

end

and ClassSig : sig

  type t

  (** {3 Elements} *)

  val find_method_element : string -> t -> Element.class_signature_method

  val find_instance_variable_element : string -> t ->
        Element.class_signature_instance_variable

  val find_label_element : string -> t -> Element.class_signature_label

  val find_element : string -> t -> Element.class_signature

  (** {3 Constructors} *)

  type signature

  val empty : signature

  val add_element : string -> Element.class_signature -> signature -> signature

  val add_documentation : Odoc_model.Comment.docs -> signature -> signature

  val add_comment : Odoc_model.Comment.docs_or_stop -> signature -> signature

  val inherit_ : t -> signature -> signature

  val constr : (Path.ClassType.t -> t) -> Path.ClassType.t -> t

  val signature : ('b -> signature) -> 'b -> t

  val unresolved : t

end

and Parent : sig

  type t = [
    | `Module of Sig.t
    | `ModuleType of Sig.t
    | `Datatype of Datatype.t
    | `Class of ClassSig.t
    | `ClassType of ClassSig.t
  ]

  type signature = [
    | `Module of Sig.t
    | `ModuleType of Sig.t
  ]

  type class_signature = [
    | `Class of ClassSig.t
    | `ClassType of ClassSig.t
  ]


  type datatype = [
    | `Datatype of Datatype.t
  ]

  type module_ = [
    | `Module of Sig.t
  ]

  type module_type = [
    | `ModuleType of Sig.t
  ]

  type sig_or_type = [
    | `Module of Sig.t
    | `ModuleType of Sig.t
    | `Datatype of Datatype.t
  ]

  type any = t

  type subst =
    | Subst of Path.ModuleType.t
    | SubstAlias of Path.Module.t

end

and Page : sig

  type t

  (** {3 Elements} *)

  val find_label_element : string -> t -> Element.page_label

  val find_section_title : string -> t -> Odoc_model.Comment.link_content

  (** {3 Constructor} *)

  val of_doc : Odoc_model.Comment.docs -> t
end

and Element : sig

  type mod_t = { canonical : (Path.Module.t * Reference.Module.t) option
        ; hidden : bool }

  type s_module = [
    | `Module of mod_t
  ]

  type s_module_type = [
    | `ModuleType
  ]

  type s_type = [
    | `Type
  ]

  type s_constructor = [
    | `Constructor of string
  ]

  type s_field = [
    | `Field of string
  ]

  type s_extension = [
    | `Extension
  ]

  type s_exception = [
    | `Exception
  ]

  type s_value = [
    | `Value
  ]

  type s_class = [
    | `Class
  ]

  type s_class_type = [
    | `ClassType
  ]

  type s_method = [
    | `Method
  ]

  type s_instance_variable = [
    | `InstanceVariable
  ]

  type s_label = [
    | `Label of string option
  ]

  type t = [
    | s_module | s_module_type | s_type | s_constructor
    | s_field | s_extension | s_exception | s_value | s_class
    | s_class_type | s_method | s_instance_variable | s_label
  ]

  type signature_module = s_module

  type signature_module_type = s_module_type

  type signature_type = [ s_type | s_class | s_class_type ]

  type signature_constructor = [s_constructor | s_extension | s_exception]

  type signature_field = s_field

  type signature_extension = [s_extension | s_exception ]

  type signature_exception = s_exception

  type signature_value = s_value
  type signature_class = s_class

  type signature_class_type = [ s_class | s_class_type ]

  type signature_label = s_label

  type signature =
    [ s_module | s_module_type | s_type
         | s_constructor | s_field | s_extension
         | s_exception | s_value | s_class | s_class_type | s_label ]

  type datatype_constructor = s_constructor

  type datatype_field = s_field

  type datatype_label = s_label

  type datatype = [ s_constructor | s_field | s_label]

  type class_signature_method = s_method

  type class_signature_instance_variable = s_instance_variable

  type class_signature_label = s_label

  type class_signature = [ s_method | s_instance_variable | s_label ]

  type page_label = s_label
end
