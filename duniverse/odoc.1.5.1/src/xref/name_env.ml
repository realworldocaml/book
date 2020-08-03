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

open Odoc_model.Paths
open Odoc_model.Lang
open Odoc_model.Predefined
open Odoc_model.Names

open Identifier

module StringTbl = Map.Make(String)

type type_ident = Odoc_model.Paths_types.Identifier.path_type

type constructor_ident = Odoc_model.Paths_types.Identifier.reference_constructor

type extension_ident = Odoc_model.Paths_types.Identifier.reference_extension

type class_type_ident = Odoc_model.Paths_types.Identifier.reference_class_type

type parent_ident = [Odoc_model.Paths_types.Identifier.path_any | Odoc_model.Paths_types.Identifier.page]

type signature_ident = [Odoc_model.Paths_types.Identifier.reference_module | Odoc_model.Paths_types.Identifier.reference_module_type]

type t =
  { modules : Module.t StringTbl.t;
    module_types : ModuleType.t StringTbl.t;
    types : type_ident StringTbl.t;
    constructors : constructor_ident StringTbl.t;
    fields : Field.t StringTbl.t;
    extensions : extension_ident StringTbl.t;
    exceptions : Exception.t StringTbl.t;
    values : Value.t StringTbl.t;
    classes : Class.t StringTbl.t;
    class_types : class_type_ident StringTbl.t;
    methods : Method.t StringTbl.t;
    instance_variables : InstanceVariable.t StringTbl.t;
    labels : Label.t StringTbl.t;
    parents : parent_ident StringTbl.t;
    elements : Identifier.t StringTbl.t;
    titles : Odoc_model.Comment.link_content StringTbl.t; (* Hack *)
    signatures : signature_ident StringTbl.t;
  }

let empty =
  { modules = StringTbl.empty;
    module_types = StringTbl.empty;
    types = StringTbl.empty;
    constructors = StringTbl.empty;
    fields = StringTbl.empty;
    extensions = StringTbl.empty;
    exceptions = StringTbl.empty;
    values = StringTbl.empty;
    classes = StringTbl.empty;
    class_types = StringTbl.empty;
    methods = StringTbl.empty;
    instance_variables = StringTbl.empty;
    labels = StringTbl.empty;
    parents = StringTbl.empty;
    elements = StringTbl.empty;
    titles = StringTbl.empty;
    signatures = StringTbl.empty
  }

let add_label_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (id : Identifier.Label.t :> Identifier.t) env.elements
  in
  let labels =
    StringTbl.add name id env.labels
  in
    { env with elements; labels }

let add_value_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (id : Identifier.Value.t :> Identifier.t) env.elements
  in
  let values =
    StringTbl.add name id env.values
  in
    { env with elements; values }

let add_external_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (id : Identifier.Value.t :> Identifier.t) env.elements
  in
  let values =
    StringTbl.add name id env.values
  in
    { env with elements; values }

let add_constructor_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (id : Identifier.Constructor.t :> Identifier.t) env.elements
  in
  let constructors =
    StringTbl.add name (id :> constructor_ident) env.constructors
  in
    { env with elements; constructors }

let add_field_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (id : Identifier.Field.t :> Identifier.t) env.elements
  in
  let fields =
    StringTbl.add name id env.fields
  in
    { env with elements; fields }

let add_type_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (id : Identifier.Type.t :> Identifier.t) env.elements
  in
  let parents =
    StringTbl.add name (id :> parent_ident) env.parents
  in
  let types =
    StringTbl.add name (id :> type_ident) env.types
  in
    { env with elements; parents; types }

let add_extension_constructor_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (id : Extension.t :> Identifier.t) env.elements
  in
  let constructors =
    StringTbl.add name (id :> constructor_ident) env.constructors
  in
  let extensions =
    StringTbl.add name (id :> extension_ident) env.extensions
  in
    { env with elements; constructors; extensions }

let add_exception_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (id : Exception.t :> Identifier.t) env.elements
  in
  let constructors =
    StringTbl.add name (id :> constructor_ident) env.constructors
  in
  let extensions =
    StringTbl.add name (id :> extension_ident) env.extensions
  in
  let exceptions =
    StringTbl.add name id env.exceptions
  in
    { env with elements; constructors; extensions; exceptions }

let add_class_type_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (id : ClassType.t :> Identifier.t) env.elements
  in
  let parents =
    StringTbl.add name (id :> parent_ident) env.parents
  in
  let types =
    StringTbl.add name (id :> type_ident) env.types
  in
  let class_types =
    StringTbl.add name (id :> class_type_ident) env.class_types
  in
    { env with elements; parents; types; class_types }

let add_class_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (id : Class.t :> Identifier.t) env.elements
  in
  let parents =
    StringTbl.add name (id :> parent_ident) env.parents
  in
  let types =
    StringTbl.add name (id :> type_ident) env.types
  in
  let class_types =
    StringTbl.add name (id :> class_type_ident) env.class_types
  in
  let classes =
    StringTbl.add name id env.classes
  in
    { env with elements; parents; types; class_types; classes }

let add_module_type_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (id : ModuleType.t :> Identifier.t) env.elements
  in
  let parents =
    StringTbl.add name (id :> parent_ident) env.parents
  in
  let module_types =
    StringTbl.add name id env.module_types
  in
  let signatures =
    StringTbl.add name (id :> signature_ident) env.signatures
  in
    { env with elements; parents; module_types; signatures }

let add_module_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (id : Module.t :> Identifier.t) env.elements
  in
  let parents =
    StringTbl.add name (id :> parent_ident) env.parents
  in
  let modules =
    StringTbl.add name (id :> Module.t) env.modules
  in
  let signatures =
    StringTbl.add name (id :> signature_ident) env.signatures
  in
    { env with elements; parents; modules; signatures }

let add_page_ident id env =
  let name = Identifier.name id in
  let parents = StringTbl.add name (id : Page.t :> parent_ident) env.parents in
    { env with parents }

let opt_fold f o acc =
  match o with
  | None -> acc
  | Some x -> f x acc

let add_label_ident_title id txt env =
  let name = Identifier.name id in
  let titles =
    StringTbl.add name txt env.titles
  in
    { env with titles }

let add_documentation doc env =
  List.fold_right (fun element env ->
    match element.Odoc_model.Location_.value with
    | `Heading (_, label, nested_elements) ->
      let env = add_label_ident label env in
      let env = add_label_ident_title label nested_elements env in
      env
    | _ -> env)
    doc env

let add_comment com env =
  match com with
  | `Docs doc -> add_documentation doc env
  | `Stop -> env

let add_value vl env =
  let open Odoc_model.Lang.Value in
  let env = add_documentation vl.doc env in
    add_value_ident vl.id env

let add_external xt env =
  let open External in
  let env = add_documentation xt.doc env in
    add_external_ident xt.id env

let add_constructor cstr env =
  let open TypeDecl.Constructor in
  let env = add_documentation cstr.doc env in
    add_constructor_ident cstr.id env

let add_field field env =
  let open TypeDecl.Field in
  let env = add_documentation field.doc env in
    add_field_ident field.id env

let add_representation repr env =
  let open TypeDecl.Representation in
  match repr with
  | Variant cstrs -> List.fold_right add_constructor cstrs env
  | Record fields -> List.fold_right add_field fields env
  | Extensible -> env

let add_type_decl decl env =
  let open TypeDecl in
  let env  = add_documentation decl.doc env in
  let env = opt_fold add_representation decl.representation env in
    add_type_ident decl.id env

let add_extension_constructor ext env =
  let open Odoc_model.Lang.Extension.Constructor in
  let env = add_documentation ext.doc env in
    add_extension_constructor_ident ext.id env

let add_extension tyext env =
  let open Odoc_model.Lang.Extension in
  let env = add_documentation tyext.doc env in
  let env =
    List.fold_right add_extension_constructor
      tyext.constructors env
  in
    env

let add_exception exn env =
  let open Odoc_model.Lang.Exception in
  let env = add_documentation exn.doc env in
    add_exception_ident exn.id env

let add_class_type cltyp env =
  let open Odoc_model.Lang.ClassType in
  let env = add_documentation cltyp.doc env in
    add_class_type_ident cltyp.id env

let add_class cl env =
  let open Odoc_model.Lang.Class in
  let env = add_documentation cl.doc env in
    add_class_ident cl.id env

let add_module_type mtyp env =
  let open Odoc_model.Lang.ModuleType in
  let env = add_documentation mtyp.doc env in
    add_module_type_ident mtyp.id env

let add_module md env =
  let open Odoc_model.Lang.Module in
  let env = add_documentation md.doc env in
    add_module_ident md.id env

let add_module_substitution m env =
  let open Odoc_model.Lang.ModuleSubstitution in
  let env = add_documentation m.doc env in
  add_module_ident m.id env

let add_unit unit env =
  let open Compilation_unit in
  let env = add_documentation unit.doc env in
    add_module_ident unit.id env

let add_page page env =
  let open Odoc_model.Lang.Page in
  let env = add_documentation page.content env in
    add_page_ident page.name env

let rec add_include incl env =
  let open Include in
  let env = add_documentation incl.doc env in
  add_signature_items incl.expansion.content env

and add_signature_item item env =
  let open Odoc_model.Lang.Signature in
  match item with
  | Module (_, md) -> add_module md env
  | ModuleType mtyp -> add_module_type mtyp env
  | Type (_, decl) -> add_type_decl decl env
  | TypExt tyext -> add_extension tyext env
  | Exception exn -> add_exception exn env
  | Value vl -> add_value vl env
  | External ext -> add_external ext env
  | Class (_, cl) -> add_class cl env
  | ClassType (_, cltyp) -> add_class_type cltyp env
  | Include incl -> add_include incl env
  | Comment com -> add_comment com env
  | ModuleSubstitution m -> add_module_substitution m env
  | TypeSubstitution t -> add_type_decl t env
  
and add_signature_items sg env =
  List.fold_right add_signature_item sg env

let rec add_module_type_expr_items expr env =
  let open Odoc_model.Lang.ModuleType in
    match expr with
    | Path _ -> env
    | Signature sg -> add_signature_items sg env
    | Functor(Unit, expr) -> add_module_type_expr_items expr env
    | Functor(Named { FunctorParameter. id; _ }, expr) ->
      add_module_ident id
        (add_module_type_expr_items expr env)
    | With(expr, _) -> add_module_type_expr_items expr env
    | TypeOf decl -> add_module_decl_items decl env

and add_module_decl_items decl env =
  let open Odoc_model.Lang.Module in
    match decl with
    | Alias _ -> env
    | ModuleType expr -> add_module_type_expr_items expr env

let add_method meth env =
  let open Odoc_model.Lang.Method in
  let env = add_documentation meth.doc env in
  let name = Identifier.name meth.id in
  let elements =
    StringTbl.add name (meth.id :> Identifier.t) env.elements
  in
  let methods =
    StringTbl.add name meth.id env.methods
  in
    { env with elements; methods }

let add_instance_variable inst env =
  let open Odoc_model.Lang.InstanceVariable in
  let env = add_documentation inst.doc env in
  let name = Identifier.name inst.id in
  let elements =
    StringTbl.add name (inst.id :> Identifier.t) env.elements
  in
  let instance_variables =
    StringTbl.add name inst.id env.instance_variables
  in
    { env with elements; instance_variables }

let add_class_signature_item item env =
  let open Odoc_model.Lang.ClassSignature in
    match item with
    | Method meth -> add_method meth env
    | InstanceVariable inst -> add_instance_variable inst env
    | Constraint _ -> env
    | Inherit _ -> env
    | Comment com -> add_comment com env

let add_class_signature_items clsig env =
  let open Odoc_model.Lang.ClassSignature in
    List.fold_right
      add_class_signature_item
      clsig.items env

let add_class_type_expr_items expr env =
  let open Odoc_model.Lang.ClassType in
    match expr with
    | Constr _ -> env
    | Signature clsig -> add_class_signature_items clsig env

let rec add_class_decl_items decl env =
  let open Odoc_model.Lang.Class in
    match decl with
    | ClassType expr -> add_class_type_expr_items expr env
    | Arrow(_, _, decl) -> add_class_decl_items decl env

open Odoc_model.Paths.Reference

let lookup_signature_ident env name : Signature.t =
  try
    let id = StringTbl.find name env.signatures in
      `Resolved (`Identifier id)
  with Not_found -> `Root (UnitName.of_string name, `TUnknown)

let lookup_module_ident env name : Module.t =
  try
    let id = StringTbl.find name env.modules in
      `Resolved (`Identifier id)
  with Not_found -> `Root (UnitName.of_string name, `TModule)

let lookup_module_type_ident env name : ModuleType.t =
  try
    let id = StringTbl.find name env.module_types in
      `Resolved (`Identifier id)
  with Not_found -> `Root (UnitName.of_string name, `TModuleType)

let lookup_type_ident env name : Type.t =
  try
    let id = StringTbl.find name env.types in
      `Resolved (`Identifier id)
  with Not_found ->
    match core_type_identifier name with
    | Some id -> `Resolved (`Identifier (id :> type_ident))
    | None -> `Root (UnitName.of_string name, `TType)

let lookup_constructor_ident env name : Constructor.t =
  try
    let id = StringTbl.find name env.constructors in
      `Resolved (`Identifier id)
  with Not_found ->
    match core_constructor_identifier name with
    | Some id -> `Resolved (`Identifier (id :> Odoc_model.Paths_types.Identifier.reference_constructor))
    | None ->
        match core_exception_identifier name with
        | Some id -> `Resolved (`Identifier (id :> Odoc_model.Paths_types.Identifier.reference_constructor))
        | None -> `Root (UnitName.of_string name, `TConstructor)

let lookup_field_ident env name : Field.t =
  try
    let id = StringTbl.find name env.fields in
      `Resolved (`Identifier id)
  with Not_found -> `Root (UnitName.of_string name, `TField)

let lookup_extension_ident env name : Extension.t =
  try
    let id = StringTbl.find name env.extensions in
      `Resolved (`Identifier id)
  with Not_found ->
    match core_exception_identifier name with
    | Some id -> `Resolved (`Identifier (id :> extension_ident))
    | None -> `Root (UnitName.of_string name, `TExtension)

let lookup_exception_ident env name : Exception.t =
  try
    let id = StringTbl.find name env.exceptions in
      `Resolved (`Identifier id)
  with Not_found ->
    match core_exception_identifier name with
    | Some id -> `Resolved (`Identifier id)
    | None -> `Root (UnitName.of_string name, `TException)

let lookup_value_ident env name : Value.t =
  try
    let id = StringTbl.find name env.values in
      `Resolved (`Identifier id)
  with Not_found -> `Root (UnitName.of_string name, `TValue)

let lookup_class_ident env name : Class.t =
  try
    let id = StringTbl.find name env.classes in
      `Resolved (`Identifier id)
  with Not_found -> `Root (UnitName.of_string name, `TClass)

let lookup_class_type_ident env name : ClassType.t =
  try
    let id = StringTbl.find name env.class_types in
      `Resolved (`Identifier id)
  with Not_found -> `Root (UnitName.of_string name, `TClassType)

let lookup_method_ident env name : Method.t =
  try
    let id = StringTbl.find name env.methods in
    `Resolved (`Identifier id)
  with Not_found -> `Root (UnitName.of_string name, `TMethod)

let lookup_instance_variable_ident env name : InstanceVariable.t =
  try
    let id = StringTbl.find name env.instance_variables in
      `Resolved (`Identifier id)
  with Not_found -> `Root (UnitName.of_string name, `TInstanceVariable)

let lookup_label_ident env name : Label.t =
  try
    let id = StringTbl.find name env.labels in
      `Resolved (`Identifier id)
  with Not_found -> `Root (UnitName.of_string name, `TLabel)

let lookup_parent_ident env name : Reference.Parent.t =
  match StringTbl.find name env.parents with
  | `Page _ ->
    assert false
  | `Root _ | `Module _ | `Argument _ | `ModuleType _ | `Type _ | `CoreType _
  | `Class _ | `ClassType _ as id ->
    `Resolved (`Identifier id)
  | exception Not_found ->
    match core_type_identifier name with
    | Some id -> `Resolved (`Identifier (id :> Identifier.Parent.t))
    | None -> `Root (UnitName.of_string name, `TUnknown)

let lookup_label_parent_ident env name : Reference.LabelParent.t =
  try
    let id = StringTbl.find name env.parents in
      `Resolved (`Identifier id)
  with Not_found ->
    match core_type_identifier name with
    | Some id -> `Resolved (`Identifier (id :> Identifier.LabelParent.t))
    | None -> `Root (UnitName.of_string name, `TUnknown)

let lookup_element_ident env name =
  try
    let id = StringTbl.find name env.elements in
      `Resolved (`Identifier id)
  with Not_found ->
    match core_type_identifier name with
    | Some id -> `Resolved (`Identifier (id :> Identifier.t))
    | None ->
        match core_constructor_identifier name with
        | Some id -> `Resolved (`Identifier (id :> Identifier.t))
        | None ->
            match core_exception_identifier name with
            | Some id -> `Resolved (`Identifier (id :> Identifier.t))
            | None -> `Root (UnitName.of_string name, `TUnknown)

let rec lookup_parent env : Reference.Parent.t -> Reference.Parent.t =
  function
  | `Resolved _ as r -> r
  | `Root (s, `TUnknown) -> lookup_parent_ident env (UnitName.to_string s)
  | `Root (s, `TModule) ->
    (lookup_module_ident env (UnitName.to_string s) :> Reference.Parent.t)
  | `Root (s, `TModuleType) ->
    (lookup_module_type_ident env (UnitName.to_string s) :> Reference.Parent.t)
  | `Root (s, `TType) as r ->
    begin match lookup_type_ident env (UnitName.to_string s) with
    | `Type _ | `Class _ | `ClassType _ | `Dot _ ->
      (* can't go from Root to any of these. *)
      assert false
    | `Root _ -> r
    | `Resolved (`Identifier (`CoreType _ | `Type _) | `Type _) as resolved ->
      (resolved :> Reference.Parent.t)
    | `Resolved (`Identifier (`Class _ | `ClassType _) | `Class _ | `ClassType _) as r
      -> r
    end
  | `Root (s, `TClass) ->
    (lookup_class_ident env (UnitName.to_string s) :> Reference.Parent.t)
  | `Root (s, `TClassType) ->
    (lookup_class_type_ident env (UnitName.to_string s) :> Reference.Parent.t)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Module(r, s) -> `Module(lookup_signature env r, s)
  | `ModuleType(r, s) -> `ModuleType(lookup_signature env r, s)
  | `Type(r, s) -> `Type(lookup_signature env r, s)
  | `Class(r, s) -> `Class(lookup_signature env r, s)
  | `ClassType(r, s) -> `ClassType(lookup_signature env r, s)

and lookup_label_parent env :
  Reference.LabelParent.t -> Reference.LabelParent.t =
  function
  | `Resolved _ as r -> r
  | `Root (s, `TUnknown) -> lookup_label_parent_ident env (UnitName.to_string s)
  | `Root (_, `TPage) as r -> r (* there are no local pages. *)
  | `Root (s, `TModule) ->
    (lookup_module_ident env (UnitName.to_string s) :> LabelParent.t)
  | `Root (s, `TModuleType) ->
    (lookup_module_type_ident env (UnitName.to_string s) :> LabelParent.t)
  | `Root (s, `TType) as r ->
    begin match lookup_type_ident env (UnitName.to_string s) with
    | `Type _ | `Class _ | `ClassType _ | `Dot _ ->
      (* can't go from Root to any of these. *)
      assert false
    | `Root _ -> r
    | `Resolved (`Identifier (`CoreType _ | `Type _ | `Class _ | `ClassType _) | `Type _ | `Class _ | `ClassType _ ) as resolved ->
      (resolved :> LabelParent.t)
    end
  | `Root (s, `TClass) ->
    (lookup_class_ident env (UnitName.to_string s) :> LabelParent.t)
  | `Root (s, `TClassType) ->
    (lookup_class_type_ident env (UnitName.to_string s) :> LabelParent.t)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Module(r, s) -> `Module(lookup_signature env r, s)
  | `ModuleType(r, s) -> `ModuleType(lookup_signature env r, s)
  | `Type(r, s) -> `Type(lookup_signature env r, s)
  | `Class(r, s) -> `Class(lookup_signature env r, s)
  | `ClassType(r, s) -> `ClassType(lookup_signature env r, s)

and lookup_signature env :
  Reference.Signature.t -> Reference.Signature.t = function
  | `Resolved _ as r -> r
  | `Root (s, `TUnknown) -> lookup_signature_ident env (UnitName.to_string s)
  | `Root (s, `TModule) ->
    (lookup_module_ident env (UnitName.to_string s) :> Reference.Signature.t)
  | `Root (s, `TModuleType) ->
    (lookup_module_type_ident env (UnitName.to_string s) :> Reference.Signature.t)
  | `Dot (p, s) -> `Dot (lookup_label_parent env p, s)
  | `Module (p,s) -> `Module (lookup_signature env p, s)
  | `ModuleType (p,s) -> `ModuleType(lookup_signature env p, s)

let lookup_module env : Reference.Module.t -> Reference.Module.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) -> lookup_module_ident env (UnitName.to_string s)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Module(p, s) -> `Module(lookup_signature env p, s)

let lookup_module_type env : Reference.ModuleType.t -> Reference.ModuleType.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) -> lookup_module_type_ident env (UnitName.to_string s)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `ModuleType(p, s) -> `ModuleType(lookup_signature env p, s)

let lookup_type env : Reference.Type.t -> Reference.Type.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) -> lookup_type_ident env (UnitName.to_string s)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Type(r, s) -> `Type(lookup_signature env r, s)
  | `Class(r, s) -> `Class(lookup_signature env r, s)
  | `ClassType(r, s) -> `ClassType(lookup_signature env r, s)

let lookup_datatype env : Reference.DataType.t -> Reference.DataType.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) as r -> begin
      match lookup_type_ident env (UnitName.to_string s) with
      | `Type _ | `Class _ | `ClassType _ | `Dot _ ->
        (* can't go from Root to any of these. *)
        assert false
      | `Root _ -> r
      | `Resolved (`Identifier (`CoreType _ | `Type _) | `Type _) as resolved ->
        resolved
      | `Resolved (`Identifier (`Class _ | `ClassType _) | `Class _ | `ClassType _) ->
        r
    end
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Type(r, s) -> `Type(lookup_signature env r, s)

let lookup_constructor env : Reference.Constructor.t -> Reference.Constructor.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) -> lookup_constructor_ident env (UnitName.to_string s)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Constructor(r, s) -> `Constructor(lookup_datatype env r, s)
  | `Extension(r, s) -> `Extension(lookup_signature env r, s)
  | `Exception(r, s) -> `Exception(lookup_signature env r, s)

let lookup_field env : Reference.Field.t -> Reference.Field.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) -> lookup_field_ident env (UnitName.to_string s)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Field(r, s) -> `Field(lookup_parent env r, s)

let lookup_extension env : Reference.Extension.t -> Reference.Extension.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) -> lookup_extension_ident env (UnitName.to_string s)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Extension(r, s) -> `Extension(lookup_signature env r, s)
  | `Exception(r, s) -> `Exception(lookup_signature env r, s)

let lookup_exception env : Reference.Exception.t -> Reference.Exception.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) -> lookup_exception_ident env (UnitName.to_string s)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Exception(r, s) -> `Exception(lookup_signature env r, s)

let lookup_value env : Reference.Value.t -> Reference.Value.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) -> lookup_value_ident env (UnitName.to_string s)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Value(r, s) -> `Value(lookup_signature env r, s)

let lookup_class env : Reference.Class.t -> Reference.Class.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) -> lookup_class_ident env (UnitName.to_string s)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Class(r, s) -> `Class(lookup_signature env r, s)

let lookup_class_type env : Reference.ClassType.t -> Reference.ClassType.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) -> lookup_class_type_ident env (UnitName.to_string s)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Class(r, s) -> `Class(lookup_signature env r, s)
  | `ClassType(r, s) -> `ClassType(lookup_signature env r, s)

let lookup_method env : Reference.Method.t -> Reference.Method.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) -> lookup_method_ident env (UnitName.to_string s)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Method(r, s) -> `Method(lookup_class_type env r, s)

let lookup_instance_variable env : Reference.InstanceVariable.t -> Reference.InstanceVariable.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) -> lookup_instance_variable_ident env (UnitName.to_string s)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `InstanceVariable(r, s) -> `InstanceVariable(lookup_class_type env r, s)

let lookup_label env : Reference.Label.t -> Reference.Label.t = function
  | `Resolved _ as r -> r
  | `Root (s, _) -> lookup_label_ident env (UnitName.to_string s)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Label(r, s) -> `Label(lookup_label_parent env r, s)

let lookup_element env : Reference.t -> Reference.t = function
  | `Resolved _ as r -> r
  | `Root (s, `TUnknown) -> (lookup_element_ident env (UnitName.to_string s) :> Reference.t)
  | `Root (_, `TPage) as r -> r (* there are no local pages. *)
  | `Root (s, `TModule) -> (lookup_module_ident env (UnitName.to_string s) :> Reference.t)
  | `Root (s, `TModuleType) -> (lookup_module_type_ident env (UnitName.to_string s) :> Reference.t)
  | `Root (s, `TType) -> (lookup_type_ident env (UnitName.to_string s) :> Reference.t)
  | `Root (s, `TConstructor) -> (lookup_constructor_ident env (UnitName.to_string s) :> Reference.t)
  | `Root (s, `TField) -> (lookup_field_ident env (UnitName.to_string s) :> Reference.t)
  | `Root (s, `TExtension) -> (lookup_extension_ident env (UnitName.to_string s) :> Reference.t)
  | `Root (s, `TException) -> (lookup_exception_ident env (UnitName.to_string s) :> Reference.t)
  | `Root (s, `TValue) -> (lookup_value_ident env (UnitName.to_string s) :> Reference.t)
  | `Root (s, `TClass) -> (lookup_class_ident env (UnitName.to_string s) :> Reference.t)
  | `Root (s, `TClassType) -> (lookup_class_type_ident env (UnitName.to_string s) :> Reference.t)
  | `Root (s, `TMethod) -> (lookup_method_ident env (UnitName.to_string s) :> Reference.t)
  | `Root (s, `TInstanceVariable) -> (lookup_instance_variable_ident env (UnitName.to_string s) :> Reference.t)
  | `Root (s, `TLabel) -> (lookup_label_ident env (UnitName.to_string s) :> Reference.t)
  | `Dot(r, s) -> `Dot(lookup_label_parent env r, s)
  | `Module _ as r -> (lookup_module env r :> Reference.t)
  | `ModuleType _ as r -> (lookup_module_type env r :> Reference.t)
  | `Type _ as r -> (lookup_type env r :> Reference.t)
  | `Constructor _ as r -> (lookup_constructor env r :> Reference.t)
  | `Field _ as r -> (lookup_field env r :> Reference.t)
  | `Extension _ as r -> (lookup_extension env r :> Reference.t)
  | `Exception _ as r -> (lookup_exception env r :> Reference.t)
  | `Value _ as r -> (lookup_value env r :> Reference.t)
  | `Class _ as r -> (lookup_class env r :> Reference.t)
  | `ClassType _ as r -> (lookup_class_type env r :> Reference.t)
  | `Method _ as r -> (lookup_method env r :> Reference.t)
  | `InstanceVariable _ as r -> (lookup_instance_variable env r :> Reference.t)
  | `Label _ as r -> (lookup_label env r :> Reference.t)


let lookup_section_title env lbl =
  match lbl with
  | `Identifier id ->
    let lbl = Identifier.name id in
    begin match StringTbl.find lbl env.titles with
    | txt -> Some txt
    | exception Not_found -> None
    end
  | _ -> None
