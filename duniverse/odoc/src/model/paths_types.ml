(** {1 Paths} *)
open Names

module Identifier =
struct

  type signature = [
    | `Root of Root.t * UnitName.t
    | `Module of signature * ModuleName.t
    | `Argument of signature * int * ArgumentName.t
    | `ModuleType of signature * ModuleTypeName.t
  ]

  type class_signature = [
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
  ]

  type datatype = [
    | `Type of signature * TypeName.t
    | `CoreType of TypeName.t
  ]

  type parent = [
    | signature
    | datatype
    | class_signature
  ]

  type label_parent = [
    | parent
    | `Page of Root.t * PageName.t
  ]

  type module_ = [
    | `Root of Root.t * UnitName.t
    | `Module of signature * ModuleName.t
    | `Argument of signature * int * ArgumentName.t
  ]

  type module_type = [
    | `ModuleType of signature * ModuleTypeName.t
  ]

  type type_ = [
    | `Type of signature * TypeName.t
    | `CoreType of TypeName.t
  ]

  type constructor = [
    | `Constructor of type_ * ConstructorName.t
  ]

  type field = [
    | `Field of parent * FieldName.t
  ]

  type extension = [
    | `Extension of signature * ExtensionName.t
  ]

  type exception_ = [
    | `Exception of signature * ExceptionName.t
    | `CoreException of ExceptionName.t
  ]

  type value = [
    | `Value of signature * ValueName.t
  ]

  type class_ = [
    | `Class of signature * ClassName.t
  ]

  type class_type = [
    | `ClassType of signature * ClassTypeName.t
  ]

  type method_ = [
    | `Method of class_signature * MethodName.t
  ]

  type instance_variable = [
    | `InstanceVariable of class_signature * InstanceVariableName.t
  ]

  type label = [
    | `Label of label_parent * LabelName.t
  ]

  type page = [
    | `Page of Root.t * PageName.t
  ]

  type any = [
    | signature
    | class_signature
    | datatype
    | parent
    | label_parent
    | module_
    | module_type
    | type_
    | constructor
    | field
    | extension
    | exception_
    | value
    | class_
    | class_type
    | method_
    | instance_variable
    | label
    | page
  ]

  type path_module = module_

  type path_module_type = module_type

  type path_type = [
    | type_
    | class_
    | class_type
  ]

  type path_class_type = [
    | class_
    | class_type
  ]

  type path_any = [
    | path_module
    | path_module_type
    | path_type
    | path_class_type
  ]

  type fragment_module = path_module
  type fragment_type = path_type

  type reference_module = path_module
  type reference_module_type = path_module_type
  type reference_type = path_type
  type reference_constructor = [
    | constructor
    | extension
    | exception_
  ]
  type reference_field = field
  type reference_extension = [
    | extension
    | exception_
  ]
  type reference_exception = exception_
  type reference_value = value
  type reference_class = class_
  type reference_class_type = [
    | class_
    | class_type
  ]
  type reference_method = method_
  type reference_instance_variable = instance_variable
  type reference_label = label
  type reference_page = page
end

module rec Path :
sig
  type module_ = [
    | `Resolved of Resolved_path.module_
    | `Root of string
    | `Forward of string
    | `Dot of module_ * string
    | `Apply of module_ * module_
  ]
  type module_type = [
    | `Resolved of Resolved_path.module_type
    | `Dot of module_ * string
  ]
  type type_ = [
    | `Resolved of Resolved_path.type_
    | `Dot of module_ * string
  ]
  type class_type = [
    | `Resolved of Resolved_path.class_type
    | `Dot of module_ * string
  ]
  type any = [
    | `Resolved of Resolved_path.any
    | `Root of string
    | `Forward of string
    | `Dot of module_ * string
    | `Apply of module_ * module_
  ]
end = Path

and Resolved_path :
sig
  type module_ = [
    | `Identifier of Identifier.path_module
    | `Subst of module_type * module_
    | `SubstAlias of module_ * module_
    | `Hidden of module_
    | `Module of module_ * ModuleName.t
    (* TODO: The canonical path should be a reference not a path *)
    | `Canonical of module_ * Path.module_
    | `Apply of module_ * Path.module_
    ]

  and module_type = [
    | `Identifier of Identifier.path_module_type
    | `ModuleType of module_ * ModuleTypeName.t
  ]

  type module_no_id = [
    | `Subst of module_type * module_
    | `SubstAlias of module_ * module_
    | `Hidden of module_
    | `Module of module_ * ModuleName.t
    | `Canonical of module_ * Path.module_
    | `Apply of module_ * Path.module_
    ]

  type module_type_no_id = [
    | `ModuleType of module_ * ModuleTypeName.t
  ]

  type type_no_id = [
    | `Type of module_ * TypeName.t
    | `Class of module_ * ClassName.t
    | `ClassType of module_ * ClassTypeName.t
  ]

  type type_ = [
    | `Identifier of Identifier.path_type
    | type_no_id
  ]

  type class_type_no_id = [
    | `Class of module_ * ClassName.t
    | `ClassType of module_ * ClassTypeName.t
  ]

  type class_type = [
    | `Identifier of Identifier.path_class_type
    | class_type_no_id
  ]

  type any = [
    | `Identifier of Identifier.any
    | module_no_id
    | module_type_no_id
    | type_no_id
    | class_type_no_id
  ]
end = Resolved_path

module rec Fragment :
sig
  type signature = [
    | `Resolved of Resolved_fragment.signature
    | `Dot of signature * string
  ]

  type module_ = [
    | `Resolved of Resolved_fragment.module_
    | `Dot of signature * string
  ]

  type type_ = [
    | `Resolved of Resolved_fragment.type_
    | `Dot of signature * string
  ]

  type any = [
    | `Resolved of Resolved_fragment.any
    | `Dot of signature * string
  ]
end = Fragment

and Resolved_fragment :
sig

  type signature = [
    | `Root
    | `Subst of Resolved_path.module_type * module_
    | `SubstAlias of Resolved_path.module_ * module_
    | `Module of signature * ModuleName.t
  ]
  and module_ = [
    | `Subst of Resolved_path.module_type * module_
    | `SubstAlias of Resolved_path.module_ * module_
    | `Module of signature * ModuleName.t
  ]

  type type_ = [
    | `Type of signature * TypeName.t
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
  ]

  (* Absence of `Root here might make coersions annoying *)
  type any = [
    | `Root
    | `Subst of Resolved_path.module_type * module_
    | `SubstAlias of Resolved_path.module_ * module_
    | `Module of signature * ModuleName.t
    | `Type of signature * TypeName.t
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
  ]
end = Resolved_fragment

module rec Reference :
sig

  type tag_only_module = [
    | `TModule
   ]

  type tag_only_module_type = [
    | `TModuleType
  ]

  type tag_only_type = [
    | `TType
  ]

  type tag_only_constructor = [
    | `TConstructor
  ]

  type tag_only_field = [
    | `TField
  ]

  type tag_only_extension = [
    | `TExtension
  ]

  type tag_only_exception = [
    | `TException
  ]

  type tag_only_value = [
    | `TValue
  ]

  type tag_only_class = [
    | `TClass
  ]

  type tag_only_class_type = [
    | `TClassType
  ]

  type tag_only_method = [
    | `TMethod
  ]

  type tag_only_instance_variable = [
    | `TInstanceVariable
  ]

  type tag_only_label = [
    | `TLabel
  ]

  type tag_only_page = [
    | `TPage
  ]

  type tag_unknown = [
    `TUnknown
  ]

  type tag_any = [
    | tag_only_module
    | tag_only_module_type
    | tag_only_type
    | tag_only_constructor
    | tag_only_field
    | tag_only_extension
    | tag_only_exception
    | tag_only_value 
    | tag_only_class
    | tag_only_class_type
    | tag_only_method
    | tag_only_instance_variable
    | tag_only_label
    | tag_only_page
    | tag_unknown
  ]

  type tag_signature = [
    | tag_unknown
    | tag_only_module
    | tag_only_module_type
  ]

  type tag_class_signature = [
    | tag_unknown
    | tag_only_class
    | tag_only_class_type
  ]

  type tag_datatype = [
    | tag_unknown
    | tag_only_type
  ]

  type tag_parent = [
    | tag_unknown
    | tag_only_module
    | tag_only_module_type
    | tag_only_class
    | tag_only_class_type
    | tag_only_type
  ]

  type tag_label_parent = [
    | tag_unknown
    | tag_only_module
    | tag_only_module_type
    | tag_only_class
    | tag_only_class_type
    | tag_only_type
    | tag_only_page
  ]

  type signature = [
    | `Resolved of Resolved_reference.signature
    | `Root of UnitName.t * tag_signature
    | `Dot of label_parent * string
    | `Module of signature * ModuleName.t
    | `ModuleType of signature * ModuleTypeName.t
  ]

  and class_signature = [
    | `Resolved of Resolved_reference.class_signature
    | `Root of UnitName.t * tag_class_signature
    | `Dot of label_parent * string
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
  ]

  and datatype = [
    | `Resolved of Resolved_reference.datatype
    | `Root of UnitName.t * tag_datatype
    | `Dot of label_parent * string
    | `Type of signature * TypeName.t
  ]

  and parent = [
    | `Resolved of Resolved_reference.parent
    | `Root of UnitName.t * tag_parent
    | `Dot of label_parent * string
    | `Module of signature * ModuleName.t
    | `ModuleType of signature * ModuleTypeName.t
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
    | `Type of signature * TypeName.t
  ]

  and label_parent = [
    | `Resolved of Resolved_reference.label_parent
    | `Root of UnitName.t * tag_label_parent
    | `Dot of label_parent * string
    | `Module of signature * ModuleName.t
    | `ModuleType of signature * ModuleTypeName.t
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
    | `Type of signature * TypeName.t
  ]

  type tag_module = [
    | tag_only_module
    | tag_unknown
  ]

  type module_ = [
    | `Resolved of Resolved_reference.module_
    | `Root of UnitName.t * tag_module
    | `Dot of label_parent * string
    | `Module of signature * ModuleName.t
  ]

  type tag_module_type = [
    | tag_only_module_type
    | tag_unknown
  ]
  type module_type = [
    | `Resolved of Resolved_reference.module_type
    | `Root of UnitName.t * tag_module_type
    | `Dot of label_parent * string
    | `ModuleType of signature * ModuleTypeName.t
  ]

  type tag_type = [
    | tag_only_type
    | tag_only_class
    | tag_only_class_type
    | tag_unknown
  ]

  type type_ = [
    | `Resolved of Resolved_reference.type_
    | `Root of UnitName.t * tag_type
    | `Dot of label_parent * string
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
    | `Type of signature * TypeName.t
  ]

  type tag_constructor = [
    | tag_only_constructor
    | tag_only_extension
    | tag_only_exception
    | tag_unknown
  ]

  type constructor = [
    | `Resolved of Resolved_reference.constructor
    | `Root of UnitName.t * tag_constructor
    | `Dot of label_parent * string
    | `Constructor of datatype * ConstructorName.t
    | `Extension of signature * ExtensionName.t
    | `Exception of signature * ExceptionName.t
  ]

  type tag_field = [
    | tag_only_field
    | tag_unknown
  ]

  type field = [
    | `Resolved of Resolved_reference.field
    | `Root of UnitName.t * tag_field
    | `Dot of label_parent * string
    | `Field of parent * FieldName.t
  ]

  type tag_extension = [
    | tag_only_extension
    | tag_only_exception
    | tag_unknown
  ]
  type extension = [
    | `Resolved of Resolved_reference.extension
    | `Root of UnitName.t * tag_extension
    | `Dot of label_parent * string
    | `Extension of signature * ExtensionName.t
    | `Exception of signature * ExceptionName.t
  ]

  type tag_exception = [
    | tag_only_exception
    | tag_unknown
  ]

  type exception_ = [
    | `Resolved of Resolved_reference.exception_
    | `Root of UnitName.t * tag_exception
    | `Dot of label_parent * string
    | `Exception of signature * ExceptionName.t
  ]

  type tag_value = [
    | tag_only_value
    | tag_unknown
  ]

  type value = [
    | `Resolved of Resolved_reference.value
    | `Root of UnitName.t * tag_value
    | `Dot of label_parent * string
    | `Value of signature * ValueName.t
  ]

  type tag_class = [
    | tag_only_class
    | tag_unknown
  ]

  type class_ = [
    | `Resolved of Resolved_reference.class_
    | `Root of UnitName.t * tag_class
    | `Dot of label_parent * string
    | `Class of signature * ClassName.t
  ]

  type tag_class_type = [
    | tag_only_class
    | tag_only_class_type
    | tag_unknown
  ]

  type class_type = [
    | `Resolved of Resolved_reference.class_type
    | `Root of UnitName.t * tag_class_type
    | `Dot of label_parent * string
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
  ]

  type tag_method = [
    | tag_only_method
    | tag_unknown
  ]

  type method_ = [
    | `Resolved of Resolved_reference.method_
    | `Root of UnitName.t * tag_method
    | `Dot of label_parent * string
    | `Method of class_signature * MethodName.t
  ]

  type tag_instance_variable = [
    | tag_only_instance_variable
    | tag_unknown
  ]

  type instance_variable = [
    | `Resolved of Resolved_reference.instance_variable
    | `Root of UnitName.t * tag_instance_variable
    | `Dot of label_parent * string
    | `InstanceVariable of class_signature * InstanceVariableName.t
  ]

  type tag_label = [
    | tag_only_label
    | tag_unknown
  ]

  type label = [
    | `Resolved of Resolved_reference.label
    | `Root of UnitName.t * tag_label
    | `Dot of label_parent * string
    | `Label of label_parent * LabelName.t
  ]

  type tag_page = [
    | tag_only_page
    | tag_unknown
  ]

  type page = [
    | `Resolved of Resolved_reference.page
    | `Root of UnitName.t * tag_page
    | `Dot of label_parent * string
  ]

  type any = [
    | `Resolved of Resolved_reference.any
    | `Root of UnitName.t * tag_any
    | `Dot of label_parent * string
    | `Module of signature * ModuleName.t
    | `ModuleType of signature * ModuleTypeName.t
    | `Type of signature * TypeName.t
    | `Constructor of datatype * ConstructorName.t
    | `Field of parent * FieldName.t
    | `Extension of signature * ExtensionName.t
    | `Exception of signature * ExceptionName.t
    | `Value of signature * ValueName.t
    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
    | `Method of class_signature * MethodName.t
    | `InstanceVariable of class_signature * InstanceVariableName.t
    | `Label of label_parent * LabelName.t
  ]
end = Reference

and Resolved_reference :
sig

  (* Note - many of these are effectively unions of previous types,
    but they are declared here explicitly because OCaml isn't yet
    smart enough to accept the more natural expression of this. Hence
    we define here all those types that ever appear on the right hand
    side of the constructors and then below we redefine many with
    the actual hierarchy made more explicit. *)
  type datatype = [
    | `Identifier of Identifier.datatype

    | `Type of signature * TypeName.t
  ]

  and module_ = [
    | `Identifier of Identifier.module_

    | `SubstAlias of Resolved_path.module_ * module_
    | `Module of signature * ModuleName.t
    | `Canonical of module_ * Reference.module_
  ]

  (* Signature is [ module | moduletype ] *)
  and signature = [
    | `Identifier of Identifier.signature

    | `SubstAlias of Resolved_path.module_ * module_
    | `Module of signature * ModuleName.t
    | `Canonical of module_ * Reference.module_

    | `ModuleType of signature * ModuleTypeName.t
  ]

  and class_signature = [
    | `Identifier of Identifier.class_signature

    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t
  ]

  (* parent is [ signature | class_signature ] *)
  and parent = [
    | `Identifier of Identifier.parent

    | `SubstAlias of Resolved_path.module_ * module_
    | `Module of signature * ModuleName.t
    | `Canonical of module_ * Reference.module_

    | `ModuleType of signature * ModuleTypeName.t

    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t

    | `Type of signature * TypeName.t
  ]

  (* The only difference between parent and label_parent
     is that the Identifier allows more types *)
  and label_parent = [
    | `Identifier of Identifier.label_parent

    | `SubstAlias of Resolved_path.module_ * module_
    | `Module of signature * ModuleName.t
    | `Canonical of module_ * Reference.module_

    | `ModuleType of signature * ModuleTypeName.t

    | `Class of signature * ClassName.t
    | `ClassType of signature * ClassTypeName.t

    | `Type of signature * TypeName.t
  ]

  type s_substalias = [ `SubstAlias of Resolved_path.module_ * module_ ]
  type s_module = [ `Module of signature * ModuleName.t ]
  type s_canonical = [ `Canonical of module_ * Reference.module_ ]
  type s_module_type = [ `ModuleType of signature * ModuleTypeName.t ]
  type s_type =[ `Type of signature * TypeName.t ]
  type s_constructor = [ `Constructor of datatype * ConstructorName.t ]
  type s_field = [ `Field of parent * FieldName.t ]
  type s_extension = [ `Extension of signature * ExtensionName.t ]
  type s_exception = [ `Exception of signature * ExceptionName.t ]
  type s_value = [ `Value of signature * ValueName.t ]
  type s_class = [ `Class of signature * ClassName.t ]
  type s_class_type = [ `ClassType of signature * ClassTypeName.t ]
  type s_method = [ `Method of class_signature * MethodName.t ]
  type s_instance_variable = [ `InstanceVariable of class_signature * InstanceVariableName.t ]
  type s_label = [ `Label of label_parent * LabelName.t ]

  type module_no_id = [
    | s_substalias
    | s_module
    | s_canonical
  ]

  type signature_no_id = [
    | module_no_id
    | s_module_type
  ]

  type class_signature_no_id = [
    | s_class
    | s_class_type
  ]

  type datatype_no_id = [
    | s_type
  ]

  type parent_no_id = [
    | signature_no_id
    | class_signature_no_id
    | datatype_no_id
  ]

  type module_type = [
    | `Identifier of Identifier.reference_module_type
    | s_module_type
  ]

  type type_ = [
    | `Identifier of Identifier.reference_type
    | s_type
    | s_class
    | s_class_type
  ]

  type constructor = [
    | `Identifier of Identifier.reference_constructor
    | s_constructor
    | s_extension
    | s_exception
  ]

  type constructor_no_id = [
    | s_constructor
    | s_extension
    | s_exception
  ]

  type field = [
    | `Identifier of Identifier.reference_field
    | s_field
  ]

  type extension = [
    | `Identifier of Identifier.reference_extension
    | s_exception
    | s_extension
  ]

  type extension_no_id = [
    | s_exception
    | s_extension
  ]

  type exception_ = [
    | `Identifier of Identifier.reference_exception
    | s_exception
  ]

  type value = [
    | `Identifier of Identifier.reference_value
    | s_value
  ]

  type class_ = [
    | `Identifier of Identifier.reference_class
    | s_class
  ]

  type class_type = [
    | `Identifier of Identifier.reference_class_type
    | s_class
    | s_class_type
  ]

  type class_type_no_id = [
    | s_class
    | s_class_type
  ]

  type method_ = [
    | `Identifier of Identifier.reference_method
    | s_method
  ]

  type instance_variable = [
    | `Identifier of Identifier.reference_instance_variable
    | s_instance_variable
  ]

  type label = [
    | `Identifier of Identifier.reference_label
    | s_label
  ]

  type page = [
    | `Identifier of Identifier.reference_page
  ]

  type any = [
    | `Identifier of Identifier.any
    | s_substalias
    | s_module
    | s_canonical
    | s_module_type
    | s_type
    | s_constructor
    | s_field
    | s_extension
    | s_exception
    | s_value
    | s_class
    | s_class_type
    | s_method
    | s_instance_variable
    | s_label
  ]
end = Resolved_reference
