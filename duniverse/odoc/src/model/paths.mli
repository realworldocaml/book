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

(** Identifiers for definitions *)

open Names

module Identifier : sig
  (** {2 Generic operations} *)

  module Signature : sig
    type t = Paths_types.Identifier.signature

    val equal : t -> t -> bool
    
    val hash : t -> int
 
    val root : t -> Root.t
  end

  module ClassSignature : sig
    type t = Paths_types.Identifier.class_signature

    val equal : t -> t -> bool

    val hash : t -> int

    val root : t -> Root.t
  end

  module DataType : sig
    type t = Paths_types.Identifier.datatype

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module Parent : sig
    type t = Paths_types.Identifier.parent

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module LabelParent : sig
    type t = Paths_types.Identifier.label_parent

    val equal : t -> t -> bool

    val hash : t -> int

    val root : t -> Root.t
  end

  module Module : sig
    type t = Paths_types.Identifier.module_

    val equal : t -> t -> bool

    val hash : t -> int

    val root : t -> Root.t
  end

  module ModuleType : sig
    type t = Paths_types.Identifier.module_type

    val equal : t -> t -> bool

    val hash : t -> int

    val root : t -> Root.t
  end

  module Type : sig
    type t = Paths_types.Identifier.type_

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module Constructor : sig
    type t = Paths_types.Identifier.constructor

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module Field : sig
    type t = Paths_types.Identifier.field

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module Extension : sig
    type t = Paths_types.Identifier.extension

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module Exception : sig
    type t = Paths_types.Identifier.exception_

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module Value : sig
    type t = Paths_types.Identifier.value

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module Class : sig
    type t = Paths_types.Identifier.class_

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module ClassType : sig
    type t = Paths_types.Identifier.class_type

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module Method : sig
    type t = Paths_types.Identifier.method_

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module InstanceVariable : sig
    type t = Paths_types.Identifier.instance_variable

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module Label : sig
    type t = Paths_types.Identifier.label

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module Page : sig
    type t = Paths_types.Identifier.page

    val equal : t -> t -> bool

    val hash : t -> int
  end

  module Path : sig
    
    module Module : sig
      type t = Paths_types.Identifier.path_module

      val equal : t -> t -> bool 
      
      val hash : t -> int
    end

    module ModuleType : sig
      type t = Paths_types.Identifier.path_module_type

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module Type : sig
      type t = Paths_types.Identifier.path_type

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module ClassType : sig
      type t = Paths_types.Identifier.path_class_type

      val equal : t -> t -> bool

      val hash : t -> int
    end

    type t = Paths_types.Identifier.path_any
  end

  type t = Paths_types.Identifier.any

  val page_of_t : t -> Page.t

  val signature_of_t : t -> Signature.t

  val class_signature_of_t : t -> ClassSignature.t

  val datatype_of_t : t -> DataType.t

  val module_of_t : t -> Module.t

  val module_type_of_t : t -> ModuleType.t

  val type_of_t : t -> Type.t

  val constructor_of_t : t -> Constructor.t

  val field_of_t : t -> Field.t

  val extension_of_t : t -> Extension.t

  val exception_of_t : t -> Exception.t

  val value_of_t : t -> Value.t

  val class_of_t : t -> Class.t

  val class_type_of_t : t -> ClassType.t

  val method_of_t : t -> Method.t

  val instance_variable_of_t : t -> InstanceVariable.t

  val label_of_t : t -> Label.t

  val parent_of_t : t -> Parent.t

  val equal : t -> t -> bool

  val hash : t -> int
  
  val name : [< t] -> string
end

(** Normal OCaml paths (i.e. the ones present in types) *)
module rec Path : sig

  module Resolved : sig

    module Module : sig
      type t = Paths_types.Resolved_path.module_

      val of_ident : Identifier.Path.Module.t -> t

      val equal : t -> t -> bool

      val hash : t -> int

      val is_hidden : t -> bool

      val identifier : t -> Identifier.Path.Module.t

      val rebase : Identifier.Signature.t -> t -> t

      val equal_identifier : Identifier.Path.Module.t -> t -> bool
    end

    module ModuleType : sig
      type t = Paths_types.Resolved_path.module_type

      val of_ident : Identifier.Path.ModuleType.t -> t

      val equal : t -> t -> bool

      val hash : t -> int

      val is_hidden : t -> bool

      val identifier : t -> Identifier.Path.ModuleType.t

      val rebase : Identifier.Signature.t -> t -> t

      val equal_identifier : Identifier.Path.ModuleType.t -> t -> bool
    end

    module Type : sig
      type t = Paths_types.Resolved_path.type_

      val of_ident : Identifier.Path.Type.t -> t

      val equal : t -> t -> bool

      val hash : t -> int

      val is_hidden : t -> bool

      val identifier : t -> Identifier.Path.Type.t

      val rebase : Identifier.Signature.t -> t -> t

      val equal_identifier : Identifier.Path.Type.t -> t -> bool
    end

    module ClassType : sig
      type t = Paths_types.Resolved_path.class_type

      val of_ident : Identifier.Path.ClassType.t -> t

      val equal : t -> t -> bool

      val hash : t -> int

      val is_hidden : t -> bool

      val identifier : t -> Identifier.Path.ClassType.t

      val rebase : Identifier.Signature.t -> t -> t

      val equal_identifier : Identifier.Path.ClassType.t -> t -> bool
    end

    type t = Paths_types.Resolved_path.any

    val module_of_t : t -> Module.t

    val module_type_of_t : t -> ModuleType.t

    val type_of_t : t -> Type.t

    val class_type_of_t : t -> ClassType.t

    val equal : t -> t -> bool

    val hash : t -> int

    val identifier : t -> Identifier.t
  end

  module Module : sig
    type t = Paths_types.Path.module_

    val equal : t -> t -> bool

    val hash : t -> int

    val is_hidden : t -> bool    
  end

  module ModuleType : sig
    type t = Paths_types.Path.module_type

    val equal : t -> t -> bool

    val hash : t -> int

    val is_hidden : t -> bool    
  end

  module Type : sig
    type t = Paths_types.Path.type_

    val equal : t -> t -> bool

    val hash : t -> int

    val is_hidden : t -> bool    
  end

  module ClassType : sig
    type t = Paths_types.Path.class_type

    val equal : t -> t -> bool

    val hash : t -> int

    val is_hidden : t -> bool    
  end

  type t = Paths_types.Path.any

  val module_of_t : t -> Module.t

  val module_type_of_t : t -> ModuleType.t

  val type_of_t : t -> Type.t

  val class_type_of_t : t -> ClassType.t

  val module_ : Module.t -> ModuleName.t -> Module.t

  val apply : Module.t -> Module.t -> Module.t

  val module_type : Module.t -> ModuleTypeName.t -> ModuleType.t

  val is_hidden : t -> bool

  val equal : t -> t -> bool

  val hash : t -> int
end

(** OCaml path fragments for specifying module substitutions *)
module Fragment : sig

  module Resolved : sig

    module Signature : sig
      type t = Paths_types.Resolved_fragment.signature

      val path : Path.Module.t -> t -> Path.Module.t

      val identifier : Identifier.Signature.t -> t -> Identifier.Signature.t

      val equal : t -> t -> bool

      val hash : t -> int

      val split : t -> string * t option
    end

    module Module : sig
      type t = Paths_types.Resolved_fragment.module_

      val path : Path.Module.t -> t -> Path.Module.t

      val identifier : Identifier.Signature.t -> t -> Identifier.Path.Module.t

      val equal : t -> t -> bool

      val hash : t -> int

      val split : t -> string * t option
    end

    module Type : sig
      type t = Paths_types.Resolved_fragment.type_

      val path : Path.Module.t -> t -> Path.Type.t

      val identifier : Identifier.Signature.t -> t -> Identifier.Path.Type.t

      val equal : t -> t -> bool

      val hash : t -> int

      val split : t -> string * t option

    end

    (** {2 Explicit coercions} *)

    type t = Paths_types.Resolved_fragment.any

    val identifier : Identifier.Signature.t -> t -> Identifier.t

    val signature_of_t : t -> Signature.t

    val module_of_t : t -> Module.t

    val type_of_t : t -> Type.t

  end

  module Signature : sig
    type t = Paths_types.Fragment.signature

    val equal : t -> t ->  bool

    val hash : t -> int

    val split : t -> string * t option

    val path : Path.Module.t -> t -> Path.Module.t
  end

  module Module : sig
    type t = Paths_types.Fragment.module_

    val equal : t -> t ->  bool

    val hash : t -> int

    val split : t -> string * t option

    val path : Path.Module.t -> t -> Path.Module.t
  end

  module Type : sig
    type t = Paths_types.Fragment.type_

    val equal : t -> t ->  bool

    val hash : t -> int

    val split : t -> string * t option

    val path : Path.Module.t -> t -> Path.Type.t
  end

  type t = Paths_types.Fragment.any

  val signature_of_t : t -> Signature.t

  val module_of_t : t -> Module.t

  val type_of_t : t -> Type.t

end


(** References present in documentation comments ([{!Foo.Bar}]) *)
module rec Reference : sig

  module Resolved : sig
    module Signature : sig 
      type t = Paths_types.Resolved_reference.signature

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.Signature.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    module ClassSignature : sig 
      type t = Paths_types.Resolved_reference.class_signature

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.ClassSignature.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    module DataType : sig 
      type t = Paths_types.Resolved_reference.datatype

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.DataType.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    module Parent : sig 
      type t = Paths_types.Resolved_reference.parent

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.Parent.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    module LabelParent : sig 
      type t = Paths_types.Resolved_reference.label_parent

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.LabelParent.t

      val rebase : Identifier.Signature.t -> t -> t
    end
    module Module : sig 
      type t = Paths_types.Resolved_reference.module_

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.Module.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    module ModuleType : sig 
      type t = Paths_types.Resolved_reference.module_type

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.ModuleType.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    module Type : sig 
      type t = Paths_types.Resolved_reference.type_

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.Path.Type.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    module Constructor : sig 
      type t = Paths_types.Resolved_reference.constructor

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Paths_types.Identifier.reference_constructor

      val rebase : Identifier.Signature.t -> t -> t
    end

    module Field : sig 
      type t = Paths_types.Resolved_reference.field

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Paths_types.Identifier.reference_field

      val rebase : Identifier.Signature.t -> t -> t
    end

    module Extension : sig 
      type t = Paths_types.Resolved_reference.extension

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Paths_types.Identifier.reference_extension

      val rebase : Identifier.Signature.t -> t -> t
    end

    module Exception : sig 
      type t = Paths_types.Resolved_reference.exception_

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.Exception.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    module Value : sig 
      type t = Paths_types.Resolved_reference.value

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.Value.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    module Class : sig 
      type t = Paths_types.Resolved_reference.class_

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.Class.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    module ClassType : sig 
      type t = Paths_types.Resolved_reference.class_type

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Paths_types.Identifier.reference_class_type

      val rebase : Identifier.Signature.t -> t -> t
    end

    module Method : sig 
      type t = Paths_types.Resolved_reference.method_

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.Method.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    module InstanceVariable : sig 
      type t = Paths_types.Resolved_reference.instance_variable

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.InstanceVariable.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    module Label : sig 
      type t = Paths_types.Resolved_reference.label

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.Label.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    module Page : sig 
      type t = Paths_types.Resolved_reference.page

      val equal : t -> t -> bool

      val hash : t -> int

      val identifier : t -> Identifier.Page.t

      val rebase : Identifier.Signature.t -> t -> t
    end

    type t = Paths_types.Resolved_reference.any

    val module_of_t : t -> Module.t

    val module_type_of_t : t -> ModuleType.t

    val signature_of_t : t -> Signature.t

    val class_signature_of_t : t -> ClassSignature.t

    val parent_of_t : t -> Parent.t

    val label_parent_of_t : t -> LabelParent.t

    val type_of_t : t -> Type.t

    val datatype_of_t : t -> DataType.t

    val constructor_of_t : t -> Constructor.t

    val field_of_t : t -> Field.t

    val extension_of_t : t -> Extension.t

    val exception_of_t : t -> Exception.t

    val value_of_t : t -> Value.t
  
    val class_of_t : t -> Class.t

    val class_type_of_t : t -> ClassType.t

    val method_of_t : t -> Method.t

    val instance_variable_of_t : t -> InstanceVariable.t

    val label_of_t : t -> Label.t

    val identifier : t -> Identifier.t
  end


  (** {2 Creators} *)

    module Signature : sig 
      type t = Paths_types.Reference.signature

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module ClassSignature : sig 
      type t = Paths_types.Reference.class_signature

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module DataType : sig 
      type t = Paths_types.Reference.datatype

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module Parent : sig 
      type t = Paths_types.Reference.parent

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module LabelParent : sig 
      type t = Paths_types.Reference.label_parent

      val equal : t -> t -> bool

      val hash : t -> int
    end
    module Module : sig 
      type t = Paths_types.Reference.module_

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module ModuleType : sig 
      type t = Paths_types.Reference.module_type

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module Type : sig 
      type t = Paths_types.Reference.type_

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module Constructor : sig 
      type t = Paths_types.Reference.constructor

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module Field : sig 
      type t = Paths_types.Reference.field

      val equal : t -> t -> bool

      val hash : t -> int

    end

    module Extension : sig 
      type t = Paths_types.Reference.extension

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module Exception : sig 
      type t = Paths_types.Reference.exception_

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module Value : sig 
      type t = Paths_types.Reference.value

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module Class : sig 
      type t = Paths_types.Reference.class_

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module ClassType : sig 
      type t = Paths_types.Reference.class_type

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module Method : sig 
      type t = Paths_types.Reference.method_

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module InstanceVariable : sig 
      type t = Paths_types.Reference.instance_variable

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module Label : sig 
      type t = Paths_types.Reference.label

      val equal : t -> t -> bool

      val hash : t -> int
    end

    module Page : sig 
      type t = Paths_types.Reference.page

      val equal : t -> t -> bool

      val hash : t -> int
    end

    type t = Paths_types.Reference.any

    val module_of_t : t -> Module.t

    val module_type_of_t : t -> ModuleType.t

    val signature_of_t : t -> Signature.t

    val class_signature_of_t : t -> ClassSignature.t

    val parent_of_t : t -> Parent.t

    val label_parent_of_t : t -> LabelParent.t

    val type_of_t : t -> Type.t

    val datatype_of_t : t -> DataType.t

    val constructor_of_t : t -> Constructor.t

    val field_of_t : t -> Field.t

    val extension_of_t : t -> Extension.t

    val exception_of_t : t -> Exception.t

    val value_of_t : t -> Value.t
  
    val class_of_t : t -> Class.t

    val class_type_of_t : t -> ClassType.t

    val method_of_t : t -> Method.t

    val instance_variable_of_t : t -> InstanceVariable.t

    val label_of_t : t -> Label.t

    val hash : t -> int

    val equal : t -> t -> bool
end

