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

open Paths
open Lang
open Names

val option_map : ('a -> 'a) -> 'a option -> 'a option

val list_map : ('a -> 'a) -> 'a list -> 'a list

val pair_map : ('a -> 'a) -> ('b -> 'b) -> ('a * 'b) -> ('a * 'b)

class virtual identifier : object

  method virtual root : Root.t -> Root.t

  method identifier : Identifier.t -> Identifier.t

  method identifier_root_name : UnitName.t -> UnitName.t

  method identifier_page_name : PageName.t -> PageName.t

  method identifier_module_name : ModuleName.t -> ModuleName.t

  method identifier_argument_position : int -> int

  method identifier_argument_name : ArgumentName.t -> ArgumentName.t

  method identifier_module_type_name : ModuleTypeName.t -> ModuleTypeName.t

  method identifier_type_name : TypeName.t -> TypeName.t

  method identifier_core_type_name : TypeName.t -> TypeName.t

  method identifier_constructor_name : ConstructorName.t -> ConstructorName.t

  method identifier_field_name : FieldName.t -> FieldName.t

  method identifier_extension_name : ExtensionName.t -> ExtensionName.t

  method identifier_exception_name : ExceptionName.t -> ExceptionName.t

  method identifier_core_exception_name : ExceptionName.t -> ExceptionName.t

  method identifier_value_name : ValueName.t -> ValueName.t

  method identifier_class_name : ClassName.t -> ClassName.t

  method identifier_class_type_name : ClassTypeName.t -> ClassTypeName.t

  method identifier_method_name : MethodName.t -> MethodName.t

  method identifier_instance_variable_name : InstanceVariableName.t -> InstanceVariableName.t

  method identifier_label_name : LabelName.t -> LabelName.t

  method identifier_page : Identifier.Page.t -> Identifier.Page.t

  method identifier_signature : Identifier.Signature.t -> Identifier.Signature.t

  method identifier_class_signature : Identifier.ClassSignature.t ->
    Identifier.ClassSignature.t

  method identifier_datatype : Identifier.DataType.t -> Identifier.DataType.t

  method identifier_module : Identifier.Module.t -> Identifier.Module.t

  method identifier_module_type : Identifier.ModuleType.t ->
    Identifier.ModuleType.t

  method identifier_type : Identifier.Type.t -> Identifier.Type.t

  method identifier_constructor : Identifier.Constructor.t ->
    Identifier.Constructor.t

  method identifier_parent : Identifier.Parent.t -> Identifier.Parent.t

  method identifier_field : Identifier.Field.t -> Identifier.Field.t

  method identifier_extension : Identifier.Extension.t -> Identifier.Extension.t

  method identifier_exception : Identifier.Exception.t -> Identifier.Exception.t

  method identifier_value : Identifier.Value.t -> Identifier.Value.t

  method identifier_class : Identifier.Class.t -> Identifier.Class.t

  method identifier_class_type : Identifier.ClassType.t -> Identifier.ClassType.t

  method identifier_method : Identifier.Method.t -> Identifier.Method.t

  method identifier_instance_variable : Identifier.InstanceVariable.t ->
    Identifier.InstanceVariable.t

  method identifier_label : Identifier.Label.t -> Identifier.Label.t

end

class virtual path : object

  method virtual identifier : Identifier.t -> Identifier.t

  method path_resolved : Path.Resolved.t -> Path.Resolved.t

  method path_resolved_module_name : ModuleName.t -> ModuleName.t

  method path_resolved_module_type_name : ModuleTypeName.t -> ModuleTypeName.t

  method path_resolved_type_name : TypeName.t -> TypeName.t

  method path_resolved_class_name : ClassName.t -> ClassName.t

  method path_resolved_class_type_name : ClassTypeName.t -> ClassTypeName.t

  method path_resolved_module : Path.Resolved.Module.t -> Path.Resolved.Module.t

  method path_resolved_module_type : Path.Resolved.ModuleType.t ->
    Path.Resolved.ModuleType.t

  method path_resolved_type : Path.Resolved.Type.t -> Path.Resolved.Type.t

  method path_resolved_class_type : Path.Resolved.ClassType.t ->
    Path.Resolved.ClassType.t

  method path : Path.t -> Path.t

  method path_root_name : string -> string

  method path_dot_name : string -> string

  method path_module : Path.Module.t -> Path.Module.t

  method path_module_type : Path.ModuleType.t -> Path.ModuleType.t

  method path_type : Path.Type.t -> Path.Type.t

  method path_class_type : Path.ClassType.t -> Path.ClassType.t

end

class virtual fragment : object

  method virtual path_resolved : Path.Resolved.t -> Path.Resolved.t

  method virtual path_resolved_module : Path.Resolved.Module.t -> Path.Resolved.Module.t

  method virtual path_resolved_module_type : Path.Resolved.ModuleType.t -> Path.Resolved.ModuleType.t

  method fragment_resolved : Fragment.Resolved.t ->
    Fragment.Resolved.t

  method fragment_resolved_signature : Fragment.Resolved.Signature.t -> Fragment.Resolved.Signature.t 

  method fragment_resolved_module_name : ModuleName.t -> ModuleName.t

  method fragment_resolved_type_name : TypeName.t -> TypeName.t

  method fragment_resolved_class_name : ClassName.t -> ClassName.t

  method fragment_resolved_class_type_name : ClassTypeName.t -> ClassTypeName.t

  method fragment_resolved_module : Fragment.Resolved.Module.t ->
    Fragment.Resolved.Module.t

  method fragment_resolved_type : Fragment.Resolved.Type.t ->
    Fragment.Resolved.Type.t

  method fragment : Fragment.t -> Fragment.t

  method fragment_name : string -> string

  method fragment_module : Fragment.Module.t -> Fragment.Module.t

  method fragment_type : Fragment.Type.t -> Fragment.Type.t

  method fragment_signature : Fragment.Signature.t -> Fragment.Signature.t
end

class virtual reference : object

  method virtual identifier : Identifier.t -> Identifier.t

  method virtual path_resolved : Path.Resolved.t -> Path.Resolved.t

  method virtual path_resolved_module : Path.Resolved.Module.t -> Path.Resolved.Module.t

  method reference_resolved : Reference.Resolved.t ->
    Reference.Resolved.t

  method reference_resolved_module_name : ModuleName.t -> ModuleName.t

  method reference_resolved_module_type_name : ModuleTypeName.t -> ModuleTypeName.t

  method reference_resolved_type_name : TypeName.t -> TypeName.t

  method reference_resolved_class_name : ClassName.t -> ClassName.t

  method reference_resolved_class_type_name : ClassTypeName.t -> ClassTypeName.t

  method reference_resolved_constructor_name : ConstructorName.t -> ConstructorName.t

  method reference_resolved_extension_name : ExtensionName.t -> ExtensionName.t

  method reference_resolved_exception_name : ExceptionName.t -> ExceptionName.t

  method reference_resolved_field_name : FieldName.t -> FieldName.t

  method reference_resolved_value_name : ValueName.t -> ValueName.t

  method reference_resolved_method_name : MethodName.t -> MethodName.t

  method reference_resolved_instance_variable_name : InstanceVariableName.t -> InstanceVariableName.t

  method reference_resolved_label_name : LabelName.t -> LabelName.t

  method reference_resolved_signature : Reference.Resolved.Signature.t -> Reference.Resolved.Signature.t

  method reference_resolved_class_signature : Reference.Resolved.ClassSignature.t -> Reference.Resolved.ClassSignature.t

  method reference_resolved_parent : Reference.Resolved.Parent.t -> Reference.Resolved.Parent.t

  method reference_resolved_label_parent : Reference.Resolved.LabelParent.t -> Reference.Resolved.LabelParent.t

  method reference_resolved_module : Reference.Resolved.Module.t ->
    Reference.Resolved.Module.t

  method reference_resolved_datatype : Reference.Resolved.DataType.t -> Reference.Resolved.DataType.t

  method reference_resolved_module_type : Reference.Resolved.ModuleType.t ->
    Reference.Resolved.ModuleType.t

  method reference_resolved_type : Reference.Resolved.Type.t ->
    Reference.Resolved.Type.t

  method reference_resolved_constructor : Reference.Resolved.Constructor.t ->
    Reference.Resolved.Constructor.t

  method reference_resolved_field : Reference.Resolved.Field.t ->
    Reference.Resolved.Field.t

  method reference_resolved_extension : Reference.Resolved.Extension.t ->
    Reference.Resolved.Extension.t

  method reference_resolved_exception : Reference.Resolved.Exception.t ->
    Reference.Resolved.Exception.t

  method reference_resolved_value : Reference.Resolved.Value.t ->
    Reference.Resolved.Value.t

  method reference_resolved_class : Reference.Resolved.Class.t ->
    Reference.Resolved.Class.t

  method reference_resolved_class_type : Reference.Resolved.ClassType.t ->
    Reference.Resolved.ClassType.t

  method reference_resolved_method : Reference.Resolved.Method.t ->
    Reference.Resolved.Method.t

  method reference_resolved_instance_variable :
    Reference.Resolved.InstanceVariable.t ->
    Reference.Resolved.InstanceVariable.t

  method reference_resolved_label : Reference.Resolved.Label.t ->
    Reference.Resolved.Label.t

  method reference_root_name : UnitName.t -> UnitName.t

  method reference_dot_name : string -> string

  method reference_module_name : ModuleName.t -> ModuleName.t

  method reference_module_type_name : ModuleTypeName.t -> ModuleTypeName.t

  method reference_type_name : TypeName.t -> TypeName.t

  method reference_constructor_name : ConstructorName.t -> ConstructorName.t

  method reference_field_name : FieldName.t -> FieldName.t

  method reference_extension_name : ExtensionName.t -> ExtensionName.t

  method reference_exception_name : ExceptionName.t -> ExceptionName.t

  method reference_value_name : ValueName.t -> ValueName.t

  method reference_class_name : ClassName.t -> ClassName.t

  method reference_class_type_name : ClassTypeName.t -> ClassTypeName.t

  method reference_method_name : MethodName.t -> MethodName.t

  method reference_instance_variable_name : InstanceVariableName.t -> InstanceVariableName.t

  method reference_label_name : LabelName.t -> LabelName.t

  method reference_signature : Reference.Signature.t -> Reference.Signature.t 

  method reference_label_parent : Reference.LabelParent.t -> Reference.LabelParent.t 

  method reference_parent : Reference.Parent.t -> Reference.Parent.t

  method reference_module : Reference.Module.t -> Reference.Module.t

  method reference_datatype : Reference.DataType.t -> Reference.DataType.t

  method reference_module_type : Reference.ModuleType.t -> Reference.ModuleType.t

  method reference_type : Reference.Type.t -> Reference.Type.t

  method reference_constructor : Reference.Constructor.t -> Reference.Constructor.t

  method reference_field : Reference.Field.t -> Reference.Field.t

  method reference_extension : Reference.Extension.t -> Reference.Extension.t

  method reference_exception : Reference.Exception.t -> Reference.Exception.t

  method reference_value : Reference.Value.t -> Reference.Value.t

  method reference_class : Reference.Class.t -> Reference.Class.t

  method reference_class_type : Reference.ClassType.t -> Reference.ClassType.t

  method reference_class_signature : Reference.ClassSignature.t -> Reference.ClassSignature.t 

  method reference_method : Reference.Method.t -> Reference.Method.t

  method reference_instance_variable : Reference.InstanceVariable.t ->
    Reference.InstanceVariable.t

  method reference_label : Reference.Label.t -> Reference.Label.t

  method reference_any : Reference.t -> Reference.t

end

class virtual paths : object
  inherit identifier
  inherit path
  inherit fragment
  inherit reference
end

class virtual documentation : object

  method virtual identifier_label : Identifier.Label.t -> Identifier.Label.t

  method virtual identifier : Identifier.t -> Identifier.t

  method virtual path_module : Path.Module.t -> Path.Module.t

  method virtual reference_module : Reference.Module.t -> Reference.Module.t

  method virtual reference_module_type : Reference.ModuleType.t ->
    Reference.ModuleType.t

  method virtual reference_type : Reference.Type.t -> Reference.Type.t

  method virtual reference_constructor : Reference.Constructor.t ->
    Reference.Constructor.t

  method virtual reference_field : Reference.Field.t -> Reference.Field.t

  method virtual reference_extension : Reference.Extension.t ->
    Reference.Extension.t

  method virtual reference_exception : Reference.Exception.t ->
    Reference.Exception.t

  method virtual reference_value : Reference.Value.t -> Reference.Value.t

  method virtual reference_class : Reference.Class.t -> Reference.Class.t

  method virtual reference_class_type : Reference.ClassType.t ->
    Reference.ClassType.t

  method virtual reference_method : Reference.Method.t -> Reference.Method.t

  method virtual reference_instance_variable : Reference.InstanceVariable.t ->
    Reference.InstanceVariable.t

  method virtual reference_label : Reference.Label.t -> Reference.Label.t

  method virtual reference_any : Reference.t -> Reference.t

  method documentation_reference :
    Paths.Reference.t * Comment.link_content ->
      Paths.Reference.t * Comment.link_content

  method documentation : Comment.docs -> Comment.docs

  method documentation_comment : Comment.docs_or_stop -> Comment.docs_or_stop

end

class virtual module_ : object

  method virtual identifier_module : Identifier.Module.t -> Identifier.Module.t

  method virtual path_module : Path.Module.t -> Path.Module.t

  method virtual reference_module : Reference.Module.t -> Reference.Module.t

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual module_type_expr : ModuleType.expr -> ModuleType.expr

  method virtual signature : Signature.t -> Signature.t

  method virtual module_type_functor_param :
    FunctorParameter.t -> FunctorParameter.t

  method module_expansion : Module.expansion -> Module.expansion

  method module_decl : Module.decl -> Module.decl

  method module_ : Module.t -> Module.t

  method module_equation : Module.decl -> Module.decl

  method module_hidden : bool -> bool
end

class virtual module_substitution : object

  method virtual identifier_module : Identifier.Module.t -> Identifier.Module.t

  method virtual path_module : Path.Module.t -> Path.Module.t

  method virtual documentation : Comment.docs -> Comment.docs

  method module_substitution : ModuleSubstitution.t -> ModuleSubstitution.t
end

class virtual module_type : object

  method virtual identifier_module : Identifier.Module.t -> Identifier.Module.t

  method virtual identifier_module_type : Identifier.ModuleType.t ->
    Identifier.ModuleType.t

  method virtual path_module : Path.Module.t -> Path.Module.t

  method virtual path_module_type : Path.ModuleType.t -> Path.ModuleType.t

  method virtual path_type : Path.Type.t -> Path.Type.t

  method virtual fragment_module : Fragment.Module.t -> Fragment.Module.t

  method virtual fragment_type : Fragment.Type.t -> Fragment.Type.t

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual module_decl : Module.decl -> Module.decl

  method virtual module_equation : Module.Equation.t -> Module.Equation.t

  method virtual signature : Signature.t -> Signature.t

  method virtual type_decl_equation : TypeDecl.Equation.t -> TypeDecl.Equation.t

  method virtual type_decl_param_name : string -> string

  method virtual module_expansion : Module.expansion -> Module.expansion

  method module_type_substitution : ModuleType.substitution ->
    ModuleType.substitution

  method module_type_expr : ModuleType.expr -> ModuleType.expr

  method module_type_functor_param :
    FunctorParameter.t -> FunctorParameter.t

  method module_type : ModuleType.t -> ModuleType.t

end

class virtual signature : object

  method virtual documentation_comment :
    Comment.docs_or_stop -> Comment.docs_or_stop

  method virtual module_ : Module.t -> Module.t

  method virtual module_substitution : ModuleSubstitution.t -> ModuleSubstitution.t
  
  method virtual module_type : ModuleType.t -> ModuleType.t

  method virtual type_decl : TypeDecl.t -> TypeDecl.t

  method virtual extension : Extension.t -> Extension.t

  method virtual exception_ : Exception.t -> Exception.t

  method virtual value : Value.t -> Value.t

  method virtual external_ : External.t -> External.t

  method virtual class_ : Class.t -> Class.t

  method virtual class_type : ClassType.t -> ClassType.t

  method virtual include_ : Include.t -> Include.t

  method signature_item : Signature.item -> Signature.item

  method signature : Signature.t -> Signature.t

end

class virtual include_ : object

  method virtual module_decl : Module.decl -> Module.decl

  method virtual identifier_signature : Identifier.Signature.t ->
                                        Identifier.Signature.t

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual signature : Signature.t -> Signature.t

  method include_expansion_resolved : bool -> bool

  method include_expansion : Include.expansion -> Include.expansion

  method include_ : Include.t -> Include.t

end

class virtual type_decl : object

  method virtual identifier_type : Identifier.Type.t -> Identifier.Type.t

  method virtual identifier_constructor : Identifier.Constructor.t ->
    Identifier.Constructor.t

  method virtual identifier_field : Identifier.Field.t -> Identifier.Field.t

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method type_decl_constructor : TypeDecl.Constructor.t ->
    TypeDecl.Constructor.t

  method type_decl_constructor_argument : TypeDecl.Constructor.argument ->
    TypeDecl.Constructor.argument

  method type_decl_field : TypeDecl.Field.t -> TypeDecl.Field.t

  method type_decl_field_mutable : bool -> bool

  method type_decl_representation : TypeDecl.Representation.t ->
    TypeDecl.Representation.t

  method type_decl_variance : TypeDecl.variance -> TypeDecl.variance

  method type_decl_param_desc : TypeDecl.param_desc -> TypeDecl.param_desc

  method type_decl_param_name : string -> string

  method type_decl_param : TypeDecl.param -> TypeDecl.param

  method type_decl_equation : TypeDecl.Equation.t -> TypeDecl.Equation.t

  method type_decl_private : bool -> bool

  method type_decl_constraint : TypeExpr.t * TypeExpr.t ->
    TypeExpr.t * TypeExpr.t

  method type_decl : TypeDecl.t -> TypeDecl.t

end

class virtual extension : object

  method virtual identifier_extension : Identifier.Extension.t ->
    Identifier.Extension.t

  method virtual path_type : Path.Type.t -> Path.Type.t

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_decl_param : TypeDecl.param -> TypeDecl.param

  method virtual type_decl_private : bool -> bool

  method virtual type_decl_constructor_argument :
    TypeDecl.Constructor.argument -> TypeDecl.Constructor.argument

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method extension_constructor : Extension.Constructor.t ->
    Extension.Constructor.t

  method extension : Extension.t -> Extension.t

end

class virtual exception_ : object

  method virtual identifier_exception : Identifier.Exception.t ->
    Identifier.Exception.t

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method virtual type_decl_constructor_argument :
    TypeDecl.Constructor.argument -> TypeDecl.Constructor.argument

  method exception_ : Exception.t -> Exception.t

end

class virtual value : object

  method virtual identifier_value : Identifier.Value.t -> Identifier.Value.t

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method value : Value.t -> Value.t
end

class virtual external_ : object

  method virtual identifier_value : Identifier.Value.t -> Identifier.Value.t

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method external_ : External.t -> External.t

  method external_primitive : string -> string

end

class virtual class_ : object

  method virtual identifier_class : Identifier.Class.t -> Identifier.Class.t

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_decl_param : TypeDecl.param -> TypeDecl.param

  method virtual class_type_expr : ClassType.expr -> ClassType.expr

  method virtual type_expr_label : TypeExpr.label -> TypeExpr.label

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method virtual class_signature : ClassSignature.t -> ClassSignature.t

  method class_decl : Class.decl -> Class.decl

  method class_ : Class.t -> Class.t

  method class_virtual : bool -> bool

end

class virtual class_type : object

  method virtual identifier_class_type : Identifier.ClassType.t ->
    Identifier.ClassType.t

  method virtual path_class_type : Path.ClassType.t -> Path.ClassType.t

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_decl_param : TypeDecl.param -> TypeDecl.param

  method virtual class_signature : ClassSignature.t -> ClassSignature.t

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method class_type_expr : ClassType.expr -> ClassType.expr

  method class_type : ClassType.t -> ClassType.t

  method class_type_virtual : bool -> bool

end

class virtual class_signature : object

  method virtual documentation_comment :
    Comment.docs_or_stop -> Comment.docs_or_stop

  method virtual class_type_expr : ClassType.expr -> ClassType.expr

  method virtual method_ : Method.t -> Method.t

  method virtual instance_variable : InstanceVariable.t ->
    InstanceVariable.t

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method class_signature_item : ClassSignature.item ->
    ClassSignature.item

  method class_signature : ClassSignature.t -> ClassSignature.t

end

class virtual method_ : object

  method virtual identifier_method : Identifier.Method.t ->
    Identifier.Method.t

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method method_ : Method.t -> Method.t

  method method_private : bool -> bool

  method method_virtual : bool -> bool

end

class virtual instance_variable : object

  method virtual identifier_instance_variable :
    Identifier.InstanceVariable.t -> Identifier.InstanceVariable.t

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual type_expr : TypeExpr.t -> TypeExpr.t

  method instance_variable : InstanceVariable.t -> InstanceVariable.t

  method instance_variable_mutable : bool -> bool

  method instance_variable_virtual : bool -> bool

end

class virtual type_expr : object

  method virtual path_module_type : Path.ModuleType.t -> Path.ModuleType.t

  method virtual path_type : Path.Type.t -> Path.Type.t

  method virtual path_class_type : Path.ClassType.t -> Path.ClassType.t

  method virtual fragment_type : Fragment.Type.t -> Fragment.Type.t

  method virtual documentation : Comment.docs -> Comment.docs

  method type_expr_variant_kind : TypeExpr.Polymorphic_variant.kind ->
    TypeExpr.Polymorphic_variant.kind

  method type_expr_variant_element : TypeExpr.Polymorphic_variant.element ->
    TypeExpr.Polymorphic_variant.element

  method type_expr_variant_constructor_name : string -> string

  method type_expr_variant_constructor_const : bool -> bool

  method type_expr_variant :
    TypeExpr.Polymorphic_variant.t -> TypeExpr.Polymorphic_variant.t

  method type_expr_object_method : TypeExpr.Object.method_ ->
    TypeExpr.Object.method_

  method type_expr_object_method_name : string -> string

  method type_expr_object_field : TypeExpr.Object.field ->
    TypeExpr.Object.field

  method type_expr_object : TypeExpr.Object.t -> TypeExpr.Object.t

  method type_expr_object_open : bool -> bool

  method type_expr_package_substitution : TypeExpr.Package.substitution ->
    TypeExpr.Package.substitution

  method type_expr_package : TypeExpr.Package.t -> TypeExpr.Package.t

  method type_expr_label : TypeExpr.label -> TypeExpr.label

  method type_expr_label_name : string -> string

  method type_expr : TypeExpr.t -> TypeExpr.t

  method type_expr_var_name : string -> string

end

class virtual unit : object

  method virtual root : Root.t -> Root.t

  method virtual identifier_module : Identifier.Module.t ->
    Identifier.Module.t

  method virtual path_module :
    Path.Module.t -> Path.Module.t

  method virtual documentation : Comment.docs -> Comment.docs

  method virtual signature : Signature.t -> Signature.t

  method unit_import :
    Compilation_unit.Import.t -> Compilation_unit.Import.t

  method unit_import_name : string -> string

  method unit_import_digest : Digest.t -> Digest.t

  method unit_source :
    Compilation_unit.Source.t -> Compilation_unit.Source.t

  method unit_source_file : string -> string

  method unit_source_build_dir : string -> string

  method unit_source_digest : Digest.t -> Digest.t

  method unit_packed_item :
    Compilation_unit.Packed.item -> Compilation_unit.Packed.item

  method unit_packed :
    Compilation_unit.Packed.t -> Compilation_unit.Packed.t

  method unit_content :
    Compilation_unit.content -> Compilation_unit.content

  method unit : Compilation_unit.t -> Compilation_unit.t

  method unit_digest : Digest.t -> Digest.t

  method unit_interface : bool -> bool

  method unit_hidden : bool -> bool

end

class virtual page : object

  method virtual identifier_page : Identifier.Page.t -> Identifier.Page.t

  method virtual documentation : Comment.docs -> Comment.docs

  method page : Page.t -> Page.t

  method page_digest : Digest.t -> Digest.t

end

class virtual types : object
  inherit documentation
  inherit module_
  inherit module_substitution
  inherit module_type
  inherit signature
  inherit include_
  inherit type_decl
  inherit extension
  inherit exception_
  inherit value
  inherit external_
  inherit class_
  inherit class_type
  inherit class_signature
  inherit method_
  inherit instance_variable
  inherit type_expr
  inherit unit
  inherit page
end
