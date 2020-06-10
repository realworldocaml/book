
open Paths
open Lang

let rec list_map f l =
  match l with
  | [] -> l
  | x :: r ->
      let x' = f x in
        if x != x' then x' :: List.map f r
        else
          let r' = list_map f r in
            if r != r' then x' :: r'
            else l

let option_map f o =
  match o with
  | None -> o
  | Some x ->
      let x' = f x in
        if x != x' then Some x'
        else o

let pair_map f g p =
  let (a, b) = p in
  let a' = f a in
  let b' = g b in
    if a != a' || b != b' then (a', b')
    else p


class virtual identifier = object (self)

  method virtual root : Root.t -> Root.t

  method identifier : Identifier.t -> Identifier.t =
    fun id ->
        match id with
        | `Root(root, name) ->
            let root' = self#root root in
            let name' = self#identifier_root_name name in
              if root != root' || name != name' then `Root(root', name')
              else id
        | `Page(root, name) ->
            let root' = self#root root in
            let name' = self#identifier_page_name name in
              if root != root' || name != name' then `Page(root', name')
              else id
        | `Module(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_module_name name in
              if parent != parent' || name != name' then
                `Module(parent', name')
              else id
        | `Argument(parent, pos, name) ->
            let parent' = self#identifier_signature parent in
            let pos' = self#identifier_argument_position pos in
            let name' = self#identifier_argument_name name in
              if parent != parent' || pos != pos' || name != name' then
                `Argument(parent', pos', name')
              else id
        | `ModuleType(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_module_type_name name in
              if parent != parent' || name != name' then
                `ModuleType(parent', name')
              else id
        | `Type(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_type_name name in
              if parent != parent' || name != name' then
                `Type(parent', name')
              else id
        | `CoreType name ->
            let name' = self#identifier_core_type_name name in
              if name != name' then `CoreType name'
              else id
        | `Constructor(parent, name) ->
            let parent' = self#identifier_type parent in
            let name' = self#identifier_constructor_name name in
              if parent != parent' || name != name' then
                `Constructor(parent', name')
              else id
        | `Field(parent, name) ->
            let parent' = self#identifier_parent parent in
            let name' = self#identifier_field_name name in
              if parent != parent' || name != name' then
                `Field(parent', name')
              else id
        | `Extension(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_extension_name name in
              if parent != parent' || name != name' then
                `Extension(parent', name')
              else id
        | `Exception(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_exception_name name in
              if parent != parent' || name != name' then
                `Exception(parent', name')
              else id
        | `CoreException name ->
            let name' = self#identifier_core_exception_name name in
              if name != name' then `CoreException name'
              else id
        | `Value(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_value_name name in
              if parent != parent' || name != name' then
                `Value(parent', name')
              else id
        | `Class(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_class_name name in
              if parent != parent' || name != name' then
                `Class(parent', name')
              else id
        | `ClassType(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_class_type_name name in
              if parent != parent' || name != name' then
                `ClassType(parent', name')
              else id
        | `Method(parent, name) ->
            let parent' = self#identifier_class_signature parent in
            let name' = self#identifier_method_name name in
              if parent != parent' || name != name' then
                `Method(parent', name')
              else id
        | `InstanceVariable(parent, name) ->
            let parent' = self#identifier_class_signature parent in
            let name' = self#identifier_instance_variable_name name in
              if parent != parent' || name != name' then
                `InstanceVariable(parent', name')
              else id
        | `Label(parent, name) ->
            let parent' =
              match parent with
              | (`Root _ | `Module _ | `Argument _ | `ModuleType _) as parent ->
                  (self#identifier_signature parent :> Identifier.LabelParent.t)
              | (`Class _ | `ClassType _) as parent ->
                  (self#identifier_class_signature parent :> Identifier.LabelParent.t)
              | (`Type _ | `CoreType _) as parent ->
                  (self#identifier_datatype parent :> Identifier.LabelParent.t)
              | `Page _ as parent ->
                  (self#identifier_page parent :> Identifier.LabelParent.t)
            in
            let name' = self#identifier_label_name name in
              if parent != parent' || name != name' then
                `Label(parent', name')
              else id

  method identifier_root_name name = name

  method identifier_page_name name = name

  method identifier_module_name name = name

  method identifier_argument_position pos = pos

  method identifier_argument_name name = name

  method identifier_module_type_name name = name

  method identifier_type_name name = name

  method identifier_core_type_name name = name

  method identifier_constructor_name name = name

  method identifier_field_name name = name

  method identifier_extension_name name = name

  method identifier_exception_name name = name

  method identifier_core_exception_name name = name

  method identifier_value_name name = name

  method identifier_class_name name = name

  method identifier_class_type_name name = name

  method identifier_method_name name = name

  method identifier_instance_variable_name name = name

  method identifier_label_name name = name

  method identifier_parent : Identifier.Parent.t -> Identifier.Parent.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.parent_of_t

  method identifier_page : Identifier.Page.t -> Identifier.Page.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.page_of_t

  method identifier_signature : Identifier.Signature.t -> Identifier.Signature.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.signature_of_t

  method identifier_class_signature : Identifier.ClassSignature.t -> Identifier.ClassSignature.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.class_signature_of_t
    
  method identifier_datatype : Identifier.DataType.t -> Identifier.DataType.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.datatype_of_t
  
  method identifier_module : Identifier.Module.t -> Identifier.Module.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.module_of_t
    
  method identifier_module_type : Identifier.ModuleType.t -> Identifier.ModuleType.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.module_type_of_t
    
  method identifier_type : Identifier.Type.t -> Identifier.Type.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.type_of_t
    
  method identifier_constructor : Identifier.Constructor.t -> Identifier.Constructor.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.constructor_of_t
    
  method identifier_field : Identifier.Field.t -> Identifier.Field.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.field_of_t
    
  method identifier_extension : Identifier.Extension.t -> Identifier.Extension.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.extension_of_t
    
  method identifier_exception : Identifier.Exception.t -> Identifier.Exception.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.exception_of_t
    
  method identifier_value : Identifier.Value.t -> Identifier.Value.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.value_of_t
    
  method identifier_class : Identifier.Class.t -> Identifier.Class.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.class_of_t
    
  method identifier_class_type : Identifier.ClassType.t -> Identifier.ClassType.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.class_type_of_t
    
  method identifier_method : Identifier.Method.t -> Identifier.Method.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.method_of_t
    
  method identifier_instance_variable : Identifier.InstanceVariable.t -> Identifier.InstanceVariable.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.instance_variable_of_t
  
  method identifier_label : Identifier.Label.t -> Identifier.Label.t = fun id ->
    self#identifier (id :> Identifier.t) |>
    Identifier.label_of_t
    
end

class virtual path = object (self)

  method virtual identifier : Identifier.t -> Identifier.t

  method path_resolved : Path.Resolved.t -> Path.Resolved.t =
    fun p ->
        match p with
        | `Identifier id ->
            let id' = self#identifier id in
              if id != id' then `Identifier id'
              else p
        | `Subst(sub, orig) ->
            let sub' = self#path_resolved_module_type sub in
            let orig' = self#path_resolved_module orig in
              if sub != sub' || orig != orig' then `Subst(sub', orig')
              else p
        | `SubstAlias(sub, orig) ->
            let sub' = self#path_resolved_module sub in
            let orig' = self#path_resolved_module orig in
              if sub != sub' || orig != orig' then `SubstAlias(sub', orig')
              else p
        | `Hidden hp ->
            let hp' = self#path_resolved_module hp in
              if hp != hp' then `Hidden hp'
              else p
        | `Module(parent, name) ->
            let parent' = self#path_resolved_module parent in
            let name' = self#path_resolved_module_name name in
              if parent != parent' || name != name' then
                `Module(parent', name')
              else p
        | `Canonical(orig, cano) ->
            let orig' = self#path_resolved_module orig in
            let cano' = self#path_module cano in
              if orig != orig' || cano != cano' then `Canonical(orig', cano')
              else p
        | `Apply(fn, arg) ->
            let fn' = self#path_resolved_module fn in
            let arg' = self#path_module arg in
              if fn != fn' || arg != arg' then `Apply(fn', arg')
              else p
        | `ModuleType(parent, name) ->
            let parent' = self#path_resolved_module parent in
            let name' = self#path_resolved_module_type_name name in
              if parent != parent' || name != name' then
                `ModuleType(parent', name')
              else p
        | `Type(parent, name) ->
            let parent' = self#path_resolved_module parent in
            let name' = self#path_resolved_type_name name in
              if parent != parent' || name != name' then `Type(parent', name')
              else p
        | `Class(parent, name) ->
            let parent' = self#path_resolved_module parent in
            let name' = self#path_resolved_class_name name in
              if parent != parent' || name != name' then `Class(parent', name')
              else p
        | `ClassType(parent, name) ->
            let parent' = self#path_resolved_module parent in
            let name' = self#path_resolved_class_type_name name in
              if parent != parent' || name != name' then
                `ClassType(parent', name')
              else p

  method path_resolved_module_name name = name

  method path_resolved_module_type_name name = name

  method path_resolved_type_name name = name

  method path_resolved_class_name name = name

  method path_resolved_class_type_name name = name

  method path_resolved_module : Path.Resolved.Module.t -> Path.Resolved.Module.t = fun p ->
    self#path_resolved (p :> Path.Resolved.t) |>
    Path.Resolved.module_of_t

  method path_resolved_module_type : Path.Resolved.ModuleType.t -> Path.Resolved.ModuleType.t = fun p ->
    self#path_resolved (p :> Path.Resolved.t) |>
    Path.Resolved.module_type_of_t

  method path_resolved_type : Path.Resolved.Type.t -> Path.Resolved.Type.t = fun p ->
    self#path_resolved (p :> Path.Resolved.t) |>
    Path.Resolved.type_of_t

  method path_resolved_class_type : Path.Resolved.ClassType.t -> Path.Resolved.ClassType.t = fun p ->
    self#path_resolved (p :> Path.Resolved.t) |>
    Path.Resolved.class_type_of_t

  method path : Path.t -> Path.t =
    fun p ->
        match p with
        | `Resolved res ->
            let res' = self#path_resolved res in
              if res != res' then `Resolved res'
              else p
        | `Root name ->
            let name' = self#path_root_name name in
              if name != name' then `Root name'
              else p
        | `Forward name ->
            let name' = self#path_root_name name in
              if name != name' then `Forward name'
              else p
        | `Dot(parent, name) ->
            let parent' = self#path_module parent in
            let name' = self#path_dot_name name in
              if parent != parent' || name != name' then `Dot(parent', name')
              else p
        | `Apply(fn, arg) ->
            let fn' = self#path_module fn in
            let arg' = self#path_module arg in
              if fn != fn' || arg != arg' then `Apply(fn', arg')
              else p

  method path_root_name name = name

  method path_dot_name name = name

  method path_module : Path.Module.t -> Path.Module.t = fun p ->
    self#path (p :> Path.t) |> 
    Path.module_of_t

  method path_module_type : Path.ModuleType.t -> Path.ModuleType.t = fun p ->
    self#path (p :> Path.t) |>
    Path.module_type_of_t

  method path_type : Path.Type.t -> Path.Type.t = fun p ->
    self#path (p :> Path.t) |>
    Path.type_of_t

  method path_class_type : Path.ClassType.t -> Path.ClassType.t = fun p ->
    self#path (p :> Path.t) |>
    Path.class_type_of_t

end

class virtual fragment = object (self)

  method virtual path_resolved : Path.Resolved.t ->
                                   Path.Resolved.t

  method virtual path_resolved_module : Path.Resolved.Module.t -> Path.Resolved.Module.t

  method virtual path_resolved_module_type : Path.Resolved.ModuleType.t -> Path.Resolved.ModuleType.t

  method fragment_resolved : Fragment.Resolved.t -> Fragment.Resolved.t =
    fun p ->
        match p with
        | `Root -> p
        | `Subst(sub, orig) ->
            let sub' = self#path_resolved_module_type sub in
            let orig' = self#fragment_resolved_module orig in
              if sub != sub' || orig != orig' then `Subst(sub', orig')
              else p
        | `SubstAlias(sub, orig) ->
            let sub' = self#path_resolved_module sub in
            let orig' = self#fragment_resolved_module orig in
              if sub != sub' || orig != orig' then `SubstAlias(sub', orig')
              else p
        | `Module(parent, name) ->
            let parent' = self#fragment_resolved_signature parent in
            let name' = self#fragment_resolved_module_name name in
              if parent != parent' || name != name' then `Module(parent', name')
              else p
        | `Type(parent, name) ->
            let parent' = self#fragment_resolved_signature parent in
            let name' = self#fragment_resolved_type_name name in
              if parent != parent' || name != name' then `Type(parent', name')
              else p
        | `Class(parent, name) ->
            let parent' = self#fragment_resolved_signature parent in
            let name' = self#fragment_resolved_class_name name in
              if parent != parent' || name != name' then `Class(parent', name')
              else p
        | `ClassType(parent, name) ->
            let parent' = self#fragment_resolved_signature parent in
            let name' = self#fragment_resolved_class_type_name name in
              if parent != parent' || name != name' then
                `ClassType(parent', name')
              else p

  method fragment_resolved_module_name name = name

  method fragment_resolved_type_name name = name

  method fragment_resolved_class_name name = name

  method fragment_resolved_class_type_name name = name

  method fragment_resolved_signature : Fragment.Resolved.Signature.t -> Fragment.Resolved.Signature.t = fun p ->
    self#fragment_resolved (p :> Fragment.Resolved.t) |>
    Fragment.Resolved.signature_of_t
     
  method fragment_resolved_module : Fragment.Resolved.Module.t -> Fragment.Resolved.Module.t = fun p ->
    self#fragment_resolved (p :> Fragment.Resolved.t) |>
    Fragment.Resolved.module_of_t

  method fragment_resolved_type : Fragment.Resolved.Type.t -> Fragment.Resolved.Type.t = fun p ->
    self#fragment_resolved (p :> Fragment.Resolved.t) |>
    Fragment.Resolved.type_of_t

  method fragment : Fragment.t -> Fragment.t =
    fun p ->
        match p with
        | `Resolved res ->
            let res' = self#fragment_resolved res in
              if res != res' then `Resolved res'
              else p
        | `Dot(parent, name) ->
            let parent' = self#fragment_signature parent in
            let name' = self#fragment_name name in
              if parent != parent' || name != name' then `Dot(parent', name')
              else p

  method fragment_name name = name

  method fragment_signature : Fragment.Signature.t -> Fragment.Signature.t = fun p ->
    self#fragment (p :> Fragment.t) |>
    Fragment.signature_of_t

  method fragment_module : Fragment.Module.t -> Fragment.Module.t = fun p ->
    self#fragment (p :> Fragment.t) |>
    Fragment.module_of_t

  method fragment_type : Fragment.Type.t -> Fragment.Type.t = fun p ->
    self#fragment (p :> Fragment.t) |>
    Fragment.type_of_t

end

class virtual reference = object (self)

  method virtual identifier : Identifier.t -> Identifier.t

  method virtual path_resolved : Path.Resolved.t -> Path.Resolved.t

  method virtual path_resolved_module : Path.Resolved.Module.t -> Path.Resolved.Module.t

  method reference_resolved : Reference.Resolved.t -> Reference.Resolved.t =
    fun r ->
        match r with
        | `Identifier id ->
            let id' = self#identifier id in
              if id != id' then `Identifier id'
              else r
        | `SubstAlias(sub, orig) ->
            let sub' = self#path_resolved_module sub in
            let orig' = self#reference_resolved_module orig in
              if sub != sub' || orig != orig' then
                `SubstAlias(sub', orig')
              else r
        | `Module(parent, name) ->
            let parent' = self#reference_resolved_signature parent in
            let name' = self#reference_resolved_module_name name in
              if parent != parent' || name != name' then
                `Module(parent', name')
              else r
        | `Canonical(orig, cano) ->
            let orig' = self#reference_resolved_module orig in
            let cano' = self#reference_module cano in
              if orig != orig' || cano != cano' then
                `Canonical(orig', cano')
              else r
        | `ModuleType(parent, name) ->
            let parent' = self#reference_resolved_signature parent in
            let name' = self#reference_resolved_module_type_name name in
              if parent != parent' || name != name' then
                `ModuleType(parent', name')
              else r
        | `Type(parent, name) ->
            let parent' = self#reference_resolved_signature parent in
            let name' = self#reference_resolved_type_name name in
              if parent != parent' || name != name' then
                `Type(parent', name')
              else r
        | `Constructor(parent, name) ->
            let parent' = self#reference_resolved_datatype parent in
            let name' = self#reference_resolved_constructor_name name in
              if parent != parent' || name != name' then
                `Constructor(parent', name')
              else r
        | `Field(parent, name) ->
            let parent' = self#reference_resolved_parent parent in
            let name' = self#reference_resolved_field_name name in
              if parent != parent' || name != name' then
                `Field(parent', name')
              else r
        | `Extension(parent, name) ->
            let parent' = self#reference_resolved_signature parent in
            let name' = self#reference_resolved_extension_name name in
              if parent != parent' || name != name' then
                `Extension(parent', name')
              else r
        | `Exception(parent, name) ->
            let parent' = self#reference_resolved_signature parent in
            let name' = self#reference_resolved_exception_name name in
              if parent != parent' || name != name' then
                `Exception(parent', name')
              else r
        | `Value(parent, name) ->
            let parent' = self#reference_resolved_signature parent in
            let name' = self#reference_resolved_value_name name in
              if parent != parent' || name != name' then
                `Value(parent', name')
              else r
        | `Class(parent, name) ->
            let parent' = self#reference_resolved_signature parent in
            let name' = self#reference_resolved_class_name name in
              if parent != parent' || name != name' then
                `Class(parent', name')
              else r
        | `ClassType(parent, name) ->
            let parent' = self#reference_resolved_signature parent in
            let name' = self#reference_resolved_class_type_name name in
              if parent != parent' || name != name' then
                `ClassType(parent', name')
              else r
        | `Method(parent, name) ->
            let parent' = self#reference_resolved_class_signature parent in
            let name' = self#reference_resolved_method_name name in
              if parent != parent' || name != name' then
                `Method(parent', name')
              else r
        | `InstanceVariable(parent, name) ->
            let parent' = self#reference_resolved_class_signature parent in
            let name' = self#reference_resolved_instance_variable_name name in
              if parent != parent' || name != name' then
                `InstanceVariable(parent', name')
              else r
        | `Label(parent, name) ->
            let parent' = self#reference_resolved_label_parent parent in
            let name' = self#reference_resolved_label_name name in
              if parent != parent' || name != name' then
                `Label(parent', name')
              else r

  method reference_resolved_module_name name = name

  method reference_resolved_module_type_name name = name

  method reference_resolved_type_name name = name

  method reference_resolved_class_name name = name

  method reference_resolved_class_type_name name = name

  method reference_resolved_constructor_name name = name

  method reference_resolved_extension_name name = name

  method reference_resolved_exception_name name = name

  method reference_resolved_field_name name = name

  method reference_resolved_value_name name = name

  method reference_resolved_method_name name = name

  method reference_resolved_instance_variable_name name = name

  method reference_resolved_label_name name = name

  method reference_resolved_signature : Reference.Resolved.Signature.t -> Reference.Resolved.Signature.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.signature_of_t

  method reference_resolved_class_signature : Reference.Resolved.ClassSignature.t -> Reference.Resolved.ClassSignature.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.class_signature_of_t

  method reference_resolved_module : Reference.Resolved.Module.t -> Reference.Resolved.Module.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.module_of_t

  method reference_resolved_parent : Reference.Resolved.Parent.t -> Reference.Resolved.Parent.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.parent_of_t

  method reference_resolved_label_parent : Reference.Resolved.LabelParent.t -> Reference.Resolved.LabelParent.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.label_parent_of_t

  method reference_resolved_module_type
           : Reference.Resolved.ModuleType.t -> Reference.Resolved.ModuleType.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.module_type_of_t

  method reference_resolved_type : Reference.Resolved.Type.t -> Reference.Resolved.Type.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.type_of_t

  method reference_resolved_datatype : Reference.Resolved.DataType.t -> Reference.Resolved.DataType.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.datatype_of_t

  method reference_resolved_constructor : Reference.Resolved.Constructor.t -> Reference.Resolved.Constructor.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.constructor_of_t

  method reference_resolved_field : Reference.Resolved.Field.t -> Reference.Resolved.Field.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.field_of_t

  method reference_resolved_extension : Reference.Resolved.Extension.t -> Reference.Resolved.Extension.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.extension_of_t

  method reference_resolved_exception : Reference.Resolved.Exception.t -> Reference.Resolved.Exception.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.exception_of_t

  method reference_resolved_value : Reference.Resolved.Value.t -> Reference.Resolved.Value.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.value_of_t

  method reference_resolved_class : Reference.Resolved.Class.t -> Reference.Resolved.Class.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.class_of_t

  method reference_resolved_class_type : Reference.Resolved.ClassType.t -> Reference.Resolved.ClassType.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.class_type_of_t

  method reference_resolved_method : Reference.Resolved.Method.t -> Reference.Resolved.Method.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.method_of_t

  method reference_resolved_instance_variable
           : Reference.Resolved.InstanceVariable.t -> Reference.Resolved.InstanceVariable.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.instance_variable_of_t

  method reference_resolved_label : Reference.Resolved.Label.t -> Reference.Resolved.Label.t = fun r ->
    self#reference_resolved (r :> Reference.Resolved.t) |>
    Reference.Resolved.label_of_t

  method reference_any : Reference.t -> Reference.t =
    fun r ->
        match r with
        | `Resolved res ->
            let res' = self#reference_resolved (res :> Reference.Resolved.t) in
              if res != res' then `Resolved res'
              else r
        | `Root (name, kind) ->
            let name' = self#reference_root_name name in
              if name != name' then `Root (name', kind)
              else r
        | `Dot(parent, name) ->
            let parent' = self#reference_label_parent parent in
            let name' = self#reference_dot_name name in
              if parent != parent' || name != name' then `Dot(parent', name')
              else r
        | `Module(parent, name) ->
            let parent' = self#reference_signature parent in
            let name' = self#reference_module_name name in
              if parent != parent' || name != name' then `Module(parent', name')
              else r
        | `ModuleType(parent, name) ->
            let parent' = self#reference_signature parent in
            let name' = self#reference_module_type_name name in
              if parent != parent' || name != name' then `ModuleType(parent', name')
              else r
        | `Type(parent, name) ->
            let parent' = self#reference_signature parent in
            let name' = self#reference_type_name name in
              if parent != parent' || name != name' then `Type(parent', name')
              else r
        | `Constructor(parent, name) ->
            let parent' = self#reference_datatype parent in
            let name' = self#reference_constructor_name name in
              if parent != parent' || name != name' then `Constructor(parent', name')
              else r
        | `Extension(parent, name) ->
            let parent' = self#reference_signature parent in
            let name' = self#reference_extension_name name in
              if parent != parent' || name != name' then `Extension(parent', name')
              else r
        | `Exception(parent, name) ->
            let parent' = self#reference_signature parent in
            let name' = self#reference_exception_name name in
              if parent != parent' || name != name' then `Exception(parent', name')
              else r
        | `Field(parent, name) ->
            let parent' = self#reference_parent parent in
            let name' = self#reference_field_name name in
              if parent != parent' || name != name' then `Field(parent', name')
              else r
        | `Value(parent, name) ->
            let parent' = self#reference_signature parent in
            let name' = self#reference_value_name name in
              if parent != parent' || name != name' then `Value(parent', name')
              else r
        | `Class(parent, name) ->
            let parent' = self#reference_signature parent in
            let name' = self#reference_class_name name in
              if parent != parent' || name != name' then `Class(parent', name')
              else r
        | `ClassType(parent, name) ->
            let parent' = self#reference_signature parent in
            let name' = self#reference_class_type_name name in
              if parent != parent' || name != name' then `ClassType(parent', name')
              else r
        | `Method(parent, name) ->
            let parent' = self#reference_class_signature parent in
            let name' = self#reference_method_name name in
              if parent != parent' || name != name' then `Method(parent', name')
              else r
        | `InstanceVariable(parent, name) ->
            let parent' = self#reference_class_signature parent in
            let name' = self#reference_instance_variable_name name in
              if parent != parent' || name != name' then `InstanceVariable(parent', name')
              else r
        | `Label(parent, name) ->
            let parent' = self#reference_label_parent parent in
            let name' = self#reference_label_name name in
              if parent != parent' || name != name' then `Label(parent', name')
              else r

  method reference_root_name name = name

  method reference_dot_name name = name

  method reference_module_name name = name

  method reference_module_type_name name = name

  method reference_type_name name = name

  method reference_constructor_name name = name

  method reference_field_name name = name

  method reference_extension_name name = name

  method reference_exception_name name = name

  method reference_value_name name = name

  method reference_class_name name = name

  method reference_class_type_name name = name

  method reference_method_name name = name

  method reference_instance_variable_name name = name

  method reference_label_name name = name

  method reference_signature : Reference.Signature.t -> Reference.Signature.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.signature_of_t

  method reference_class_signature : Reference.ClassSignature.t -> Reference.ClassSignature.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.class_signature_of_t

  method reference_label_parent : Reference.LabelParent.t -> Reference.LabelParent.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.label_parent_of_t

  method reference_parent : Reference.Parent.t -> Reference.Parent.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.parent_of_t

  method reference_datatype : Reference.DataType.t -> Reference.DataType.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.datatype_of_t

  method reference_module : Reference.Module.t -> Reference.Module.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.module_of_t

  method reference_module_type : Reference.ModuleType.t -> Reference.ModuleType.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.module_type_of_t

  method reference_type : Reference.Type.t -> Reference.Type.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.type_of_t

  method reference_constructor : Reference.Constructor.t -> Reference.Constructor.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.constructor_of_t

  method reference_field : Reference.Field.t -> Reference.Field.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.field_of_t

  method reference_extension : Reference.Extension.t -> Reference.Extension.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.extension_of_t

  method reference_exception : Reference.Exception.t -> Reference.Exception.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.exception_of_t

  method reference_value : Reference.Value.t -> Reference.Value.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.value_of_t

  method reference_class : Reference.Class.t -> Reference.Class.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.class_of_t

  method reference_class_type : Reference.ClassType.t -> Reference.ClassType.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.class_type_of_t

  method reference_method : Reference.Method.t -> Reference.Method.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.method_of_t

  method reference_instance_variable : Reference.InstanceVariable.t -> Reference.InstanceVariable.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.instance_variable_of_t

  method reference_label : Reference.Label.t -> Reference.Label.t = fun r ->
    self#reference_any (r :> Reference.t) |>
    Reference.label_of_t

end

class virtual paths = object
  inherit identifier
  inherit path
  inherit fragment
  inherit reference
end

class virtual documentation = object (self)

  method virtual identifier_label :
    Identifier.Label.t -> Identifier.Label.t

  method virtual identifier :
    Identifier.t -> Identifier.t

  method virtual path_module :
    Path.Module.t -> Path.Module.t

  method virtual reference_module :
    Reference.Module.t -> Reference.Module.t

  method virtual reference_module_type :
    Reference.ModuleType.t -> Reference.ModuleType.t

  method virtual reference_type :
    Reference.Type.t -> Reference.Type.t

  method virtual reference_constructor :
    Reference.Constructor.t -> Reference.Constructor.t

  method virtual reference_field :
    Reference.Field.t -> Reference.Field.t

  method virtual reference_extension :
    Reference.Extension.t -> Reference.Extension.t

  method virtual reference_exception :
    Reference.Exception.t -> Reference.Exception.t

  method virtual reference_value :
    Reference.Value.t -> Reference.Value.t

  method virtual reference_class :
    Reference.Class.t -> Reference.Class.t

  method virtual reference_class_type :
    Reference.ClassType.t -> Reference.ClassType.t

  method virtual reference_method :
    Reference.Method.t -> Reference.Method.t

  method virtual reference_instance_variable :
    Reference.InstanceVariable.t -> Reference.InstanceVariable.t

  method virtual reference_label :
    Reference.Label.t -> Reference.Label.t

  method virtual reference_any :
    Reference.t -> Reference.t

  method documentation_reference ((path, content) as r) =
    let path' = self#reference_any path in
    if path' != path then
      (path', content)
    else
      r

  method private documentation_special_modules reference =
    let reference' = self#reference_module reference in
    if reference' != reference then
      reference'
    else
      reference

  method private documentation_inline_element element =
    let open Location_ in
    match element.value with
    | `Styled (style, nested_elements) ->
      let nested_elements' =
        list_map self#documentation_inline_element nested_elements
      in
      if nested_elements' != nested_elements then
        { element with value = `Styled (style, nested_elements') }
      else
        element
    | `Reference x ->
      let x' = self#documentation_reference x in
      if x' != x then
        { element with value = `Reference x' }
      else
        element
    | _ ->
      element

  method private documentation_nestable_block_element element =
    match element with
    | `Paragraph elements ->
      let elements' = list_map self#documentation_inline_element elements in
      if elements' != elements then
        `Paragraph elements'
      else
        element
    | `Modules modules ->
      let modules' = list_map self#documentation_special_modules modules in
      if modules' != modules then
        `Modules modules'
      else
        element
    | `List (tag, elements) ->
      let elements' =
        list_map
          (list_map self#documentation_located_nestable_block_element)
          elements
      in
      if elements' != elements then
        `List (tag, elements')
      else
        element
    | _ ->
      element

  method private documentation_located_nestable_block_element element =
    let open Location_ in
    let value' = self#documentation_nestable_block_element element.value in
    if element.value != value' then
      { element with value = value' }
    else
      element

  method private documentation_block_element element =
    let open Location_ in
    match element.value with
    | #Comment.nestable_block_element as value ->
        let value' = self#documentation_nestable_block_element value in
        if value' != value then
          { element with value = (value' :> Comment.block_element) }
        else
          element
    | `Tag tag -> begin
        match tag with
        | `Deprecated elements ->
            let elements' =
              list_map self#documentation_located_nestable_block_element
                elements
            in
            if elements' != elements then
              { element with value = `Tag (`Deprecated elements') }
            else
              element
        | `Param (s, elements) ->
            let elements' =
              list_map self#documentation_located_nestable_block_element
                elements
            in
            if elements' != elements then
              { element with value = `Tag (`Param (s, elements')) }
            else
              element
        | `Raise (s, elements) ->
            let elements' =
              list_map self#documentation_located_nestable_block_element
                elements
            in
            if elements' != elements then
              { element with value = `Tag (`Raise (s, elements')) }
            else
              element
        | `Return elements ->
            let elements' =
              list_map self#documentation_located_nestable_block_element
                elements
            in
            if elements' != elements then
              { element with value = `Tag (`Return elements') }
            else
              element
        | `See (k, s, elements) ->
            let elements' =
              list_map self#documentation_located_nestable_block_element
                elements
            in
            if elements' != elements then
              { element with value = `Tag (`See (k, s, elements')) }
            else
              element
        | `Before (s, elements) ->
            let elements' =
              list_map self#documentation_located_nestable_block_element
                elements
            in
            if elements' != elements then
              { element with value = `Tag (`Before (s, elements')) }
            else
              element
        | _ ->
            element
      end
    | _ ->
        element

  method documentation doc =
    list_map self#documentation_block_element doc

  method documentation_comment (comment : Comment.docs_or_stop) =
    match comment with
    | `Docs doc ->
      let doc' = self#documentation doc in
      if doc != doc' then `Docs doc'
      else comment
    | `Stop ->
      comment

end

class virtual module_ = object (self)

  method virtual identifier_module :
    Identifier.Module.t -> Identifier.Module.t

  method virtual path_module :
    Path.Module.t -> Path.Module.t

  method virtual reference_module :
    Reference.Module.t -> Reference.Module.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual module_type_expr :
    ModuleType.expr -> ModuleType.expr

  method virtual signature : Signature.t -> Signature.t

  method virtual module_type_functor_param :
    FunctorParameter.t -> FunctorParameter.t

  method module_hidden h = h

  method module_expansion expn =
    let open Module in
    match expn with
    | AlreadyASig -> AlreadyASig
    | Signature sg ->
        let sg' = self#signature sg in
        if sg != sg' then Signature sg'
        else expn
    | Functor (args, sg) ->
        let args' = list_map self#module_type_functor_param args in
        let sg' = self#signature sg in
        if args != args' || sg != sg' then Functor(args', sg')
        else expn

  method module_decl decl =
    let open Module in
      match decl with
      | Alias p ->
          let p' = self#path_module p in
            if p != p' then Alias p'
            else decl
      | ModuleType expr ->
          let expr' = self#module_type_expr expr in
            if expr != expr' then ModuleType expr'
            else decl

  method module_ md =
    let open Module in
    let {id; doc; type_; expansion; canonical; hidden; display_type} = md in
    let id' = self#identifier_module id in
    let doc' = self#documentation doc in
    let type' = self#module_decl type_ in
    let expansion' = option_map self#module_expansion expansion in
    let canonical' =
      option_map (pair_map self#path_module self#reference_module) canonical
    in
    let hidden' = self#module_hidden hidden in
    let display_type' = option_map self#module_decl display_type in
      if id != id' || doc != doc' || type_ != type'
         || expansion != expansion' || canonical != canonical'
         || hidden != hidden' || display_type != display_type'
      then
        {id = id'; doc = doc'; type_ = type'; expansion = expansion';
         canonical = canonical'; hidden = hidden'; display_type = display_type'}
      else md

  method module_equation eq =
    self#module_decl eq

end

class virtual module_substitution = object (self)

  method virtual identifier_module :
    Identifier.Module.t -> Identifier.Module.t
  
  method virtual documentation :
    Comment.docs -> Comment.docs
  
  method virtual path_module :
    Path.Module.t -> Path.Module.t

  method module_substitution subst =
    let open ModuleSubstitution in
    let {id; doc; manifest} = subst in
    let id' = self#identifier_module id in
    let doc' = self#documentation doc in
    let manifest' = self#path_module manifest in
    if id != id' || doc != doc' || manifest' != manifest
    then {id = id'; doc = doc'; manifest = manifest' }
    else subst
end

class virtual module_type = object (self)

  method virtual identifier_module :
    Identifier.Module.t -> Identifier.Module.t

  method virtual identifier_module_type :
    Identifier.ModuleType.t -> Identifier.ModuleType.t

  method virtual path_module :
    Path.Module.t -> Path.Module.t

  method virtual path_module_type :
    Path.ModuleType.t -> Path.ModuleType.t

  method virtual path_type :
    Path.Type.t -> Path.Type.t

  method virtual fragment_module :
    Fragment.Module.t -> Fragment.Module.t

  method virtual fragment_type :
    Fragment.Type.t -> Fragment.Type.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual module_decl :
    Module.decl -> Module.decl

  method virtual module_equation :
    Module.Equation.t -> Module.Equation.t

  method virtual signature :
    Signature.t -> Signature.t

  method virtual type_decl_equation :
    TypeDecl.Equation.t -> TypeDecl.Equation.t

  method virtual type_decl_param_name :
    string -> string

  method virtual module_expansion :
    Module.expansion -> Module.expansion

  method module_type_substitution subst =
    let open ModuleType in
      match subst with
      | ModuleEq(frag, eq) ->
          let frag' = self#fragment_module frag in
          let eq' = self#module_equation eq in
            if frag != frag' || eq != eq' then ModuleEq(frag', eq')
            else subst
      | TypeEq(frag, eq) ->
          let frag' = self#fragment_type frag in
          let eq' = self#type_decl_equation eq in
            if frag != frag' || eq != eq' then TypeEq(frag', eq')
            else subst
      | ModuleSubst(frag, p) ->
          let frag' = self#fragment_module frag in
          let p' = self#path_module p in
            if frag != frag' || p != p' then
              ModuleSubst(frag', p')
            else subst
      | TypeSubst(frag, eq) ->
          let frag' = self#fragment_type frag in
          let eq' = self#type_decl_equation eq in
            if frag != frag' || eq != eq' then TypeSubst(frag', eq')
            else subst

  method module_type_expr expr =
    let open ModuleType in
      match expr with
      | Path p ->
          let p' = self#path_module_type p in
            if p != p' then Path p'
            else expr
      | Signature sg ->
          let sg' = self#signature sg in
            if sg != sg' then Signature sg'
            else expr
      | Functor(arg, res) ->
          let arg' = self#module_type_functor_param arg in
          let res' = self#module_type_expr res in
            if arg != arg' || res != res' then Functor(arg', res')
            else expr
      | With(body, substs) ->
          let body' = self#module_type_expr body in
          let substs' = list_map self#module_type_substitution substs in
            if body != body' || substs != substs' then With(body', substs')
            else expr
      | TypeOf decl ->
          let decl' = self#module_decl decl in
            if decl != decl' then TypeOf decl'
            else expr

  method module_type_functor_param arg =
    match arg with
    | Unit -> Unit
    | Named { FunctorParameter. id; expr; expansion } ->
        let id' = self#identifier_module id in
        let expr' = self#module_type_expr expr in
        let expansion' = option_map self#module_expansion expansion in
          if id != id' || expr != expr' || expansion != expansion' then
            Named {FunctorParameter. id = id'; expr = expr'; expansion = expansion'}
          else arg

  method module_type mty =
    let open ModuleType in
    let {id; doc; expr; expansion} = mty in
    let id' = self#identifier_module_type id in
    let doc' = self#documentation doc in
    let expr' = option_map self#module_type_expr expr in
    let expansion' = option_map self#module_expansion expansion in
      if id != id' || doc != doc' || expr != expr' || expansion != expansion' then
        {id = id'; doc = doc'; expr = expr'; expansion = expansion'}
      else mty
end

class virtual signature = object (self)

  method virtual documentation_comment :
    Comment.docs_or_stop -> Comment.docs_or_stop

  method virtual module_ :
    Module.t -> Module.t

  method virtual module_type :
    ModuleType.t -> ModuleType.t

  method virtual type_decl :
    TypeDecl.t -> TypeDecl.t

  method virtual extension :
    Extension.t -> Extension.t

  method virtual exception_ :
    Exception.t -> Exception.t

  method virtual value :
    Value.t -> Value.t

  method virtual external_ :
    External.t -> External.t

  method virtual class_ :
    Class.t -> Class.t

  method virtual class_type :
    ClassType.t -> ClassType.t

  method virtual include_:
    Include.t -> Include.t

  method virtual module_substitution :
    ModuleSubstitution.t -> ModuleSubstitution.t

  method signature_item item =
    let open Signature in
      match item with
      | Value v ->
          let v' = self#value v in
            if v != v' then Value v'
            else item
      | External ve ->
          let ve' = self#external_ ve in
            if ve != ve' then External ve'
            else item
      | Type (recursive, decl) ->
          let decl' = self#type_decl decl in
            if decl != decl' then Type (recursive, decl')
            else item
      | TypExt ext ->
          let ext' = self#extension ext in
            if ext != ext' then TypExt ext'
            else item
      | Exception exn ->
          let exn' = self#exception_ exn in
            if exn != exn' then Exception exn'
            else item
      | Class (recursive, cls) ->
          let cls' = self#class_ cls in
            if cls != cls' then Class (recursive, cls')
            else item
      | ClassType (recursive, clty) ->
          let clty' = self#class_type clty in
            if clty != clty' then ClassType (recursive, clty')
            else item
      | Module (recursive, md) ->
          let md' = self#module_ md in
            if md != md' then Module (recursive, md')
            else item
      | ModuleType mty ->
          let mty' = self#module_type mty in
            if mty != mty' then ModuleType mty'
            else item
      | ModuleSubstitution msub ->
          let msub' = self#module_substitution msub in
          if msub' != msub then ModuleSubstitution msub'
          else item
      | TypeSubstitution tsub ->
          let tsub' = self#type_decl tsub in 
          if tsub' != tsub then TypeSubstitution tsub'
          else item
      | Include incl ->
          let incl' = self#include_ incl in
            if incl != incl' then Include incl'
            else item
      | Comment com ->
          let com' = self#documentation_comment com in
            if com != com' then Comment com'
            else item

  method signature sg =
    list_map self#signature_item sg

end

class virtual include_ = object (self)

  method virtual module_decl :
    Module.decl -> Module.decl

  method virtual identifier_signature :
    Identifier.Signature.t -> Identifier.Signature.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual signature : Signature.t -> Signature.t

  method include_expansion_resolved resolved =
    resolved

  method include_expansion expn =
    let open Include in
    let {resolved; content} = expn in
    let resolved' = self#include_expansion_resolved resolved in
    let content' = self#signature content in
      if content != content' || resolved != resolved' then
        {resolved = resolved'; content = content'}
      else expn

  method include_ incl =
    let open Include in
    let {parent; doc; decl; expansion} = incl in
    let parent' = self#identifier_signature parent in
    let doc' = self#documentation doc in
    let decl' = self#module_decl decl in
    let expansion' = self#include_expansion expansion in
      if parent != parent' || doc != doc' || decl != decl' || expansion != expansion' then
        {parent = parent'; doc = doc'; decl = decl'; expansion = expansion'}
      else incl

end

class virtual type_decl = object (self)

  method virtual identifier_type :
    Identifier.Type.t -> Identifier.Type.t

  method virtual identifier_constructor :
    Identifier.Constructor.t -> Identifier.Constructor.t

  method virtual identifier_field :
    Identifier.Field.t -> Identifier.Field.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method type_decl_constructor_argument arg =
    let open TypeDecl.Constructor in
    match arg with
    | Tuple args ->
        let args' = list_map self#type_expr args in
          if args != args' then Tuple args'
          else arg
    | Record fields ->
          let fields' = list_map self#type_decl_field fields in
            if fields != fields' then Record fields'
            else arg

  method type_decl_constructor cstr =
    let open TypeDecl.Constructor in
    let {id; doc; args; res} = cstr in
    let id' = self#identifier_constructor id in
    let doc' = self#documentation doc in
    let args' = self#type_decl_constructor_argument args in
    let res' = option_map self#type_expr res in
      if id != id' || doc != doc' || args != args' || res != res' then
        {id = id'; doc = doc'; args = args'; res = res'}
      else cstr

  method type_decl_field field =
    let open TypeDecl.Field in
    let {id; doc; mutable_; type_} = field in
    let id' = self#identifier_field id in
    let doc' = self#documentation doc in
    let mutable' = self#type_decl_field_mutable mutable_ in
    let type' = self#type_expr type_ in
      if id != id' || doc != doc'
         || mutable_ != mutable' || type_ != type'
      then
        {id = id'; doc = doc'; mutable_ = mutable'; type_ = type'}
      else field

  method type_decl_field_mutable mutable_ = mutable_

  method type_decl_representation repr =
    let open TypeDecl.Representation in
      match repr with
      | Variant cstrs ->
          let cstrs' = list_map self#type_decl_constructor cstrs in
            if cstrs != cstrs' then Variant cstrs'
            else repr
      | Record fields ->
          let fields' = list_map self#type_decl_field fields in
            if fields != fields' then Record fields'
            else repr
      | Extensible -> repr

  method type_decl_variance variance = variance

  method type_decl_param_desc desc =
    let open TypeDecl in
      match desc with
      | Any -> desc
      | Var name ->
          let name' = self#type_decl_param_name name in
            if name != name' then Var name'
            else desc

  method type_decl_param_name name = name

  method type_decl_param param =
    let desc, var = param in
    let desc' = self#type_decl_param_desc desc in
    let var' = option_map self#type_decl_variance var in
      if desc != desc' || var != var' then (desc', var')
      else param

  method type_decl_equation eq =
    let open TypeDecl.Equation in
    let {params; private_; manifest; constraints} = eq in
    let params' = list_map self#type_decl_param params in
    let private' = self#type_decl_private private_ in
    let manifest' = option_map self#type_expr manifest in
    let constraints' = list_map self#type_decl_constraint constraints in
      if params != params' || private_ != private'
         || manifest != manifest' || constraints != constraints'
      then
        {params = params'; private_ = private';
         manifest = manifest'; constraints = constraints'}
      else eq

  method type_decl_private priv = priv

  method type_decl_constraint cstr =
    let typ1, typ2 = cstr in
    let typ1' = self#type_expr typ1 in
    let typ2' = self#type_expr typ2 in
      if typ1 != typ1' || typ1 != typ1' then (typ1', typ2')
      else cstr

  method type_decl decl =
    let open TypeDecl in
    let {id; doc; equation; representation = repr} = decl in
    let id' = self#identifier_type id in
    let doc' = self#documentation doc in
    let equation' = self#type_decl_equation equation in
    let repr' =
      option_map self#type_decl_representation repr
    in
      if id != id' || doc != doc'
         || equation != equation' || repr != repr'
      then
        {id = id'; doc = doc';
         equation = equation'; representation = repr'}
      else decl

end

class virtual extension = object (self)

  method virtual identifier_extension :
    Identifier.Extension.t -> Identifier.Extension.t

  method virtual path_type :
    Path.Type.t -> Path.Type.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_decl_param :
    TypeDecl.param -> TypeDecl.param

  method virtual type_decl_private :
    bool -> bool

  method virtual type_decl_constructor_argument :
    TypeDecl.Constructor.argument -> TypeDecl.Constructor.argument

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method extension_constructor cstr =
    let open Extension.Constructor in
    let {id; doc; args; res} = cstr in
    let id' = self#identifier_extension id in
    let doc' = self#documentation doc in
    let args' = self#type_decl_constructor_argument args in
    let res' = option_map self#type_expr res in
      if id != id' || doc != doc' || args != args' || res != res' then
        {id = id'; doc = doc'; args = args'; res = res'}
      else cstr

  method extension ext =
    let open Extension in
    let {type_path; doc; type_params; private_; constructors} = ext in
    let type_path' = self#path_type type_path in
    let doc' = self#documentation doc in
    let type_params' = list_map self#type_decl_param type_params in
    let private' = self#type_decl_private private_ in
    let constructors' = list_map self#extension_constructor constructors in
      if type_path != type_path' || doc != doc' || type_params != type_params'
         || private_ != private' || constructors != constructors'
      then
        {type_path = type_path'; doc = doc'; type_params = type_params';
         private_ = private'; constructors = constructors'}
      else ext

end

class virtual exception_ = object (self)

  method virtual identifier_exception :
    Identifier.Exception.t -> Identifier.Exception.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method virtual type_decl_constructor_argument :
    TypeDecl.Constructor.argument -> TypeDecl.Constructor.argument

  method exception_ exn =
    let open Exception in
    let {id; doc; args; res} = exn in
    let id' = self#identifier_exception id in
    let doc' = self#documentation doc in
    let args' = self#type_decl_constructor_argument args in
    let res' = option_map self#type_expr res in
      if id != id' || doc != doc' || args != args' || res != res' then
        {id = id'; doc = doc'; args = args'; res = res'}
      else exn

end

class virtual value = object (self)

  method virtual identifier_value :
    Identifier.Value.t -> Identifier.Value.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method value v =
    let open Value in
    let {id; doc; type_} = v in
    let id' = self#identifier_value id in
    let doc' = self#documentation doc in
    let type' = self#type_expr type_ in
      if id != id' || doc != doc' || type_ != type' then
        {id = id'; doc = doc'; type_ = type'}
      else v

end

class virtual external_ = object (self)

  method virtual identifier_value :
    Identifier.Value.t -> Identifier.Value.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method external_ ve =
    let open External in
    let {id; doc; type_; primitives} = ve in
    let id' = self#identifier_value id in
    let doc' = self#documentation doc in
    let type' = self#type_expr type_ in
    let primitives' = list_map self#external_primitive primitives in
      if id != id' || doc != doc'
         || type_ != type' || primitives != primitives'
      then
        {id = id'; doc = doc'; type_ = type'; primitives = primitives'}
      else ve

  method external_primitive prim = prim

end

class virtual class_ = object (self)

  method virtual identifier_class :
    Identifier.Class.t -> Identifier.Class.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_decl_param :
    TypeDecl.param -> TypeDecl.param

  method virtual class_type_expr :
    ClassType.expr -> ClassType.expr

  method virtual type_expr_label :
    TypeExpr.label -> TypeExpr.label

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method virtual class_signature :
    ClassSignature.t -> ClassSignature.t

  method class_decl decl =
    let open Class in
      match decl with
      | ClassType expr ->
          let expr' = self#class_type_expr expr in
            if expr != expr' then ClassType expr'
            else decl
      | Arrow(lbl, typ, body) ->
          let lbl' = option_map self#type_expr_label lbl in
          let typ' = self#type_expr typ in
          let body' = self#class_decl body in
            if lbl != lbl' || typ != typ' || body != body' then
              Arrow(lbl', typ', body')
            else decl

  method class_ cls =
    let open Class in
    let {id; doc; virtual_; params; type_; expansion} = cls in
    let id' = self#identifier_class id in
    let doc' = self#documentation doc in
    let virtual' = self#class_virtual virtual_ in
    let params' = list_map self#type_decl_param params in
    let type' = self#class_decl type_ in
    let expansion' = option_map self#class_signature expansion in
      if id != id' || doc != doc' || virtual_ != virtual'
         || params != params' || type_ != type' || expansion != expansion'
      then
        {id = id'; doc = doc'; virtual_ = virtual';
         params = params'; type_ = type'; expansion = expansion'}
      else cls

  method class_virtual virt = virt

end

class virtual class_type = object (self)

  method virtual identifier_class_type :
    Identifier.ClassType.t -> Identifier.ClassType.t

  method virtual path_class_type :
    Path.ClassType.t -> Path.ClassType.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_decl_param :
    TypeDecl.param -> TypeDecl.param

  method virtual class_signature :
    ClassSignature.t -> ClassSignature.t

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method class_type_expr expr =
    let open ClassType in
      match expr with
      | Constr(p, params) ->
          let p' = self#path_class_type p in
          let params' = list_map self#type_expr params in
            if p != p' || params != params' then Constr(p', params')
            else expr
      | Signature csig ->
          let csig' = self#class_signature csig in
            if csig != csig' then Signature csig'
            else expr

  method class_type clty =
    let open ClassType in
    let {id; doc; virtual_; params; expr; expansion} = clty in
    let id' = self#identifier_class_type id in
    let doc' = self#documentation doc in
    let virtual' = self#class_type_virtual virtual_ in
    let params' = list_map self#type_decl_param params in
    let expr' = self#class_type_expr expr in
    let expansion' = option_map self#class_signature expansion in
      if id != id' || doc != doc' || virtual_ != virtual'
         || params != params' || expr != expr' || expansion != expansion'
      then
        {id = id'; doc = doc'; virtual_ = virtual';
         params = params'; expr = expr'; expansion = expansion'}
      else clty

  method class_type_virtual virt = virt

end

class virtual class_signature = object (self)

  method virtual documentation_comment :
    Comment.docs_or_stop -> Comment.docs_or_stop

  method virtual class_type_expr :
    ClassType.expr -> ClassType.expr

  method virtual method_ :
    Method.t -> Method.t

  method virtual instance_variable :
    InstanceVariable.t -> InstanceVariable.t

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method class_signature_item item =
    let open ClassSignature in
      match item with
      | InstanceVariable inst ->
          let inst' = self#instance_variable inst in
            if inst != inst' then InstanceVariable inst'
            else item
      | Method meth ->
          let meth' = self#method_ meth in
            if meth != meth' then Method meth'
            else item
      | Constraint(typ1, typ2) ->
          let typ1' = self#type_expr typ1 in
          let typ2' = self#type_expr typ2 in
            if typ1 != typ1' || typ1 != typ1' then Constraint(typ1', typ2')
            else item
      | Inherit expr ->
          let expr' = self#class_type_expr expr in
            if expr != expr' then Inherit expr'
            else item
      | Comment com ->
          let com' = self#documentation_comment com in
            if com != com' then Comment com'
            else item

  method class_signature csig =
    let open ClassSignature in
    let {self = slf; items} = csig in
    let slf' = option_map self#type_expr slf in
    let items' = list_map self#class_signature_item items in
      if slf != slf' || items != items' then
        {self = slf'; items = items'}
      else csig

end

class virtual method_ = object (self)

  method virtual identifier_method :
    Identifier.Method.t -> Identifier.Method.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method method_ meth =
    let open Method in
    let {id; doc; private_; virtual_; type_} = meth in
    let id' = self#identifier_method id in
    let doc' = self#documentation doc in
    let private' = self#method_private private_ in
    let virtual' = self#method_virtual virtual_ in
    let type' = self#type_expr type_ in
      if id != id' || doc != doc' || private_ != private'
         || virtual_ != virtual' || type_ != type'
      then
        {id = id'; doc = doc'; private_ = private';
         virtual_ = virtual'; type_ = type'}
      else meth

  method method_private priv = priv

  method method_virtual virt = virt

end

class virtual instance_variable = object (self)

  method virtual identifier_instance_variable :
    Identifier.InstanceVariable.t -> Identifier.InstanceVariable.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual type_expr :
    TypeExpr.t -> TypeExpr.t

  method instance_variable meth =
    let open InstanceVariable in
    let {id; doc; mutable_; virtual_; type_} = meth in
    let id' = self#identifier_instance_variable id in
    let doc' = self#documentation doc in
    let mutable' = self#instance_variable_mutable mutable_ in
    let virtual' = self#instance_variable_virtual virtual_ in
    let type' = self#type_expr type_ in
      if id != id' || doc != doc' || mutable_ != mutable'
         || virtual_ != virtual' || type_ != type'
      then
        {id = id'; doc = doc'; mutable_ = mutable';
         virtual_ = virtual'; type_ = type'}
      else meth

  method instance_variable_mutable mut = mut

  method instance_variable_virtual virt = virt

end

class virtual type_expr = object (self)

  method virtual path_module_type :
    Path.ModuleType.t -> Path.ModuleType.t

  method virtual path_type :
    Path.Type.t -> Path.Type.t

  method virtual path_class_type :
    Path.ClassType.t -> Path.ClassType.t

  method virtual fragment_type :
    Fragment.Type.t -> Fragment.Type.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method type_expr_variant_kind kind = kind

  method type_expr_variant_element elem =
    let open TypeExpr.Polymorphic_variant in
      match elem with
      | Type typ ->
          let typ' = self#type_expr typ in
            if typ != typ' then Type typ'
            else elem
      | Constructor {name; constant; arguments; doc} ->
        let name' = self#type_expr_variant_constructor_name name in
        let constant' = self#type_expr_variant_constructor_const constant in
        let arguments' = list_map self#type_expr arguments in
        let doc' = self#documentation doc in
        if name != name' ||
           constant != constant' ||
           arguments != arguments' ||
           doc' != doc then
          Constructor {
            name = name';
            constant = constant';
            arguments = arguments';
            doc = doc';
          }
        else
          elem

  method type_expr_variant_constructor_name name = name

  method type_expr_variant_constructor_const const = const

  method type_expr_variant var =
    let open TypeExpr.Polymorphic_variant in
    let {kind; elements} = var in
    let kind' = self#type_expr_variant_kind kind in
    let elements' = list_map self#type_expr_variant_element elements in
      if kind != kind' || elements != elements' then
        {kind = kind'; elements = elements'}
      else var

  method type_expr_object_method meth =
    let open TypeExpr.Object in
    let {name; type_} = meth in
    let name' = self#type_expr_object_method_name name in
    let type' = self#type_expr type_ in
      if name != name' || type_ != type' then
        {name = name'; type_ = type'}
      else meth

  method type_expr_object_method_name name = name

  method type_expr_object_field fld =
    let open TypeExpr.Object in
    match fld with
    | Method meth ->
        let meth' = self#type_expr_object_method meth in
          if meth != meth' then Method meth' else fld
    | Inherit typ ->
        let typ' = self#type_expr typ in
          if typ != typ' then Inherit typ' else fld

  method type_expr_object obj =
    let open TypeExpr.Object in
    let {fields; open_} = obj in
    let fields' = list_map self#type_expr_object_field fields in
    let open' = self#type_expr_object_open open_ in
      if fields != fields' || open_ != open' then
        {fields = fields'; open_ = open'}
      else obj

  method type_expr_object_open opn = opn

  method type_expr_package_substitution subst =
    let frag, typ = subst in
    let frag' = self#fragment_type frag in
    let typ' = self#type_expr typ in
      if frag != frag' || typ != typ' then (frag', typ')
      else subst

  method type_expr_package pkg =
    let open TypeExpr.Package in
    let {path; substitutions = substs} = pkg in
    let path' = self#path_module_type path in
    let substs' = list_map self#type_expr_package_substitution substs in
      if path != path' || substs != substs' then
        {path = path'; substitutions = substs'}
      else pkg

  method type_expr_label lbl =
    let open TypeExpr in
      match lbl with
      | Label name ->
          let name' = self#type_expr_label_name name in
            if name != name' then Label name'
            else lbl
      | Optional name ->
          let name' = self#type_expr_label_name name in
            if name != name' then Optional name'
            else lbl

  method type_expr_label_name name = name

  method type_expr typ =
    let open TypeExpr in
      match typ with
      | Var name ->
          let name' = self#type_expr_var_name name in
            if name != name' then Var name'
            else typ
      | Any -> typ
      | Alias(body, name) ->
          let body' = self#type_expr body in
          let name' = self#type_expr_var_name name in
            if body != body' || name != name' then Alias(body', name')
            else typ
      | Arrow(lbl, arg, res) ->
          let lbl' = option_map self#type_expr_label lbl in
          let arg' = self#type_expr arg in
          let res' = self#type_expr res in
            if lbl != lbl' || arg != arg' || res != res' then Arrow(lbl', arg', res')
            else typ
      | Tuple typs ->
          let typs' = list_map self#type_expr typs in
            if typs != typs' then Tuple typs'
            else typ
      | Constr(p, params) ->
          let p' = self#path_type p in
          let params' = list_map self#type_expr params in
            if p != p' || params != params' then Constr(p', params')
            else typ
      | Polymorphic_variant var ->
          let var' = self#type_expr_variant var in
            if var != var' then Polymorphic_variant var'
            else typ
      | Object obj ->
          let obj' = self#type_expr_object obj in
            if obj != obj' then Object obj'
            else typ
      | Class(p, params) ->
          let p' = self#path_class_type p in
          let params' = list_map self#type_expr params in
            if p != p' || params != params' then Class(p', params')
            else typ
      | Poly(vars, body) ->
          let vars' = list_map self#type_expr_var_name vars in
          let body' = self#type_expr body in
            if vars != vars' || body != body' then Poly(vars', body')
            else typ
      | Package pkg ->
          let pkg' = self#type_expr_package pkg in
            if pkg != pkg' then Package pkg'
            else typ

  method type_expr_var_name name = name

end

class virtual unit = object (self)

  method virtual root : Root.t -> Root.t

  method virtual identifier_module :
    Identifier.Module.t -> Identifier.Module.t

  method virtual path_module :
    Path.Module.t -> Path.Module.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method virtual signature :
    Signature.t -> Signature.t

  method unit_import import =
    let open Compilation_unit.Import in
      match import with
      | Unresolved(name, digest) ->
          let name' = self#unit_import_name name in
          let digest' = option_map self#unit_import_digest digest in
            if name != name' || digest != digest' then
              Unresolved(name', digest')
            else import
      | Resolved r ->
          let r' = self#root r in
            if r != r' then Resolved r'
            else import

  method unit_import_name name = name

  method unit_import_digest digest = digest

  method unit_source source =
    let open Compilation_unit.Source in
    let {file; build_dir; digest} = source in
    let file' = self#unit_source_file file in
    let build_dir' = self#unit_source_build_dir build_dir in
    let digest' = self#unit_source_digest digest in
      if file != file' || build_dir != build_dir' || digest != digest' then
        {file = file'; build_dir = build_dir'; digest = digest'}
      else source

  method unit_source_file file = file

  method unit_source_build_dir build_dir = build_dir

  method unit_source_digest digest = digest

  method unit_packed_item item =
    let open Compilation_unit.Packed in
    let {id; path} = item in
    let id' = self#identifier_module id in
    let path' = self#path_module path in
      if id != id' || path != path' then { id = id'; path = path' }
      else item

  method unit_packed items =
    list_map self#unit_packed_item items

  method unit_content content =
    let open Compilation_unit in
      match content with
      | Module items ->
          let items' = self#signature items in
            if items != items' then Module items'
            else content
      | Pack items ->
          let items' = self#unit_packed items in
            if items' != items then Pack items'
            else content

  method unit unit =
    let open Compilation_unit in
    let {id; doc; digest; imports;
         source; interface; hidden; content; expansion} = unit
    in
    let id' = self#identifier_module id in
    let doc' = self#documentation doc in
    let digest' = self#unit_digest digest in
    let imports' = list_map self#unit_import imports in
    let source' = option_map self#unit_source source in
    let interface' = self#unit_interface interface in
    let hidden' = self#unit_hidden hidden in
    let content' = self#unit_content content in
    let expansion' = option_map self#signature expansion in
      if id != id' || doc != doc' || digest != digest'
         || imports != imports' || source != source'
         || interface != interface' || hidden != hidden'
         || content != content' || expansion != expansion'
      then
        {id = id'; doc = doc'; digest = digest';
         imports = imports'; source = source';
         interface = interface'; hidden = hidden';
         content = content'; expansion = expansion'}
      else unit

  method unit_digest digest = digest

  method unit_interface intf = intf

  method unit_hidden hidden = hidden

end

class virtual page = object (self)

  method virtual identifier_page :
    Identifier.Page.t -> Identifier.Page.t

  method virtual documentation :
    Comment.docs -> Comment.docs

  method page page =
    let open Page in
    let {name; content; digest} = page in
    let name' = self#identifier_page name in
    let content' = self#documentation content in
    let digest' = self#page_digest digest in
    if name != name' || content != content' || digest != digest' then
      {name = name'; content = content'; digest = digest'}
    else
      page

  method page_digest digest = digest

end

class virtual types = object
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
