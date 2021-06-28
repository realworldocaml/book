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

module Reversed = struct
  type elt =
    | Root of UnitName.t
    | Module of ModuleName.t
    | ModuleType of ModuleTypeName.t
    | Argument of int * ArgumentName.t

  type t = elt list

  let rec remove_prefix prefix ~of_ =
    match prefix, of_ with
    | x1 :: xs1, x2 :: xs2 when x1 = x2 ->
      remove_prefix xs1 ~of_:xs2
    | _, _ -> of_
end

module Identifier = struct

  type t = Paths_types.Identifier.any

  let name : [< t] -> string = function
    | `Root(_, name) -> UnitName.to_string name
    | `Page(_, name) -> PageName.to_string name
    | `Module(_, name) -> ModuleName.to_string name
    | `Argument(_, _, name) -> ArgumentName.to_string name
    | `ModuleType(_, name) -> ModuleTypeName.to_string name
    | `Type(_, name) -> TypeName.to_string name
    | `CoreType name -> TypeName.to_string name
    | `Constructor(_, name) -> ConstructorName.to_string name
    | `Field(_, name) -> FieldName.to_string name
    | `Extension(_, name) -> ExtensionName.to_string name
    | `Exception(_, name) -> ExceptionName.to_string name
    | `CoreException name -> ExceptionName.to_string name
    | `Value(_, name) -> ValueName.to_string name
    | `Class(_, name) -> ClassName.to_string name
    | `ClassType(_, name) -> ClassTypeName.to_string name
    | `Method(_, name) -> MethodName.to_string name
    | `InstanceVariable(_, name) -> InstanceVariableName.to_string name
    | `Label(_, name) -> LabelName.to_string name

  let rec equal : t -> t -> bool =
    let open Paths_types.Identifier in
    fun p1 p2 ->
      match p1, p2 with
      | `Root (r1, n1), `Root (r2, n2) ->
        UnitName.equal n1 n2 && Root.equal r1 r2
      | `Module (s1,n1), `Module (s2,n2) ->
        ModuleName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any) 
      | `Argument (s1,i1,n1), `Argument (s2,i2,n2) ->
        i1=i2 && ArgumentName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `ModuleType (s1,n1), `ModuleType (s2,n2) ->
        ModuleTypeName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `Type (s1, n1), `Type (s2, n2) ->
        TypeName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `CoreType n1, `CoreType n2 ->
        TypeName.equal n1 n2
      | `Class (s1, n1), `Class (s2, n2) ->
        ClassName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `ClassType (s1, n1), `ClassType (s2, n2) ->
        ClassTypeName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `Page (r1, n1), `Page (r2, n2) ->
        PageName.equal n1 n2 && Root.equal r1 r2
      | `Constructor(t1, n1), `Constructor(t2, n2) ->
        ConstructorName.equal n1 n2 && equal (t1 : type_ :> any) (t2 : type_ :> any)
      | `Field(s1, n1), `Field(s2, n2) ->
        FieldName.equal n1 n2 && equal (s1 : parent :> any) (s2 : parent :> any)
      | `Extension(s1, n1), `Extension(s2, n2) ->
        ExtensionName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `Exception(s1, n1), `Exception(s2, n2) ->
        ExceptionName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `CoreException n1, `CoreException n2 ->
        ExceptionName.equal n1 n2
      | `Value(s1, n1), `Value(s2, n2) ->
        ValueName.equal n1 n2 && equal (s1 : signature :> any) (s2 : signature :> any)
      | `Method(s1, n1), `Method(s2, n2) ->
        MethodName.equal n1 n2 && equal (s1 : class_signature :> any) (s2 : class_signature :> any)
      | `InstanceVariable(s1, n1), `InstanceVariable(s2, n2) ->
        InstanceVariableName.equal n1 n2 && equal (s1 : class_signature :> any) (s2 : class_signature :> any)
      | `Label(s1, n1), `Label(s2, n2) ->
        LabelName.equal n1 n2 && equal (s1 : label_parent :> any) (s2 : label_parent :> any)
      | _, _ -> false

  let rec hash (id : Paths_types.Identifier.any) =
    let open Paths_types.Identifier in
    match id with
    | `Root(r, s) ->
      Hashtbl.hash (1, Root.hash r, s)
    | `Page(r, s) ->
      Hashtbl.hash (2, Root.hash r, s)
    | `Module(id, s) ->
      Hashtbl.hash (3, hash (id : signature :> any), s)
    | `Argument(id, n, s) ->
      Hashtbl.hash (4, hash (id : signature :> any), n, s)
    | `ModuleType(id, s) ->
      Hashtbl.hash (5, hash (id : signature :> any), s)
    | `Type(id, s) ->
      Hashtbl.hash (6, hash (id : signature :> any), s)
    | `CoreType s ->
      Hashtbl.hash (7, s)
    | `Constructor(id, s) ->
      Hashtbl.hash (8, hash (id : type_ :> any), s)
    | `Field(id, s) ->
      Hashtbl.hash (9, hash (id : parent :> any), s)
    | `Extension(id, s) ->
      Hashtbl.hash (10, hash (id : signature :> any), s)
    | `Exception(id, s) ->
      Hashtbl.hash (11, hash (id : signature :> any), s)
    | `CoreException s ->
      Hashtbl.hash (12, s)
    | `Value(id, s) ->
      Hashtbl.hash (13, hash (id : signature :> any), s)
    | `Class(id, s) ->
      Hashtbl.hash (14, hash (id : signature :> any), s)
    | `ClassType(id, s) ->
      Hashtbl.hash (15, hash (id : signature :> any), s)
    | `Method(id, s) ->
      Hashtbl.hash (16, hash (id : class_signature :> any), s)
    | `InstanceVariable(id, s) ->
      Hashtbl.hash (17, hash (id : class_signature :> any), s)
    | `Label(id, s) ->
      Hashtbl.hash (18, hash (id : label_parent :> any ), s)

  module Signature =
  struct
    type t = Paths_types.Identifier.signature 

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)

    let rec root = function
      | `Root(r, _) -> r
      | `Module(id, _)
      | `Argument(id, _, _)
      | `ModuleType(id, _) -> root id
  end

  module ClassSignature =
  struct
    type t = Paths_types.Identifier.class_signature 

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)

    let root = function
      | `Class (s, _) -> Signature.root s
      | `ClassType (s, _) -> Signature.root s
  end

  module DataType =
  struct
    type t = Paths_types.Identifier.datatype

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Parent =
  struct
    type t = Paths_types.Identifier.parent

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module LabelParent =
  struct
    type t = Paths_types.Identifier.label_parent

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)

    let root : t -> Root.t = function
      | `Root(r, _) -> r
      | `Module(id, _)
      | `Argument(id, _, _)
      | `ModuleType(id, _) -> Signature.root id
      | `Type(id,_) -> Signature.root id
      | `CoreType _ -> assert false
      | `Class (s, _) -> Signature.root s
      | `ClassType (s, _) -> Signature.root s
      | `Page (r, _) -> r
  end

  module Module =
  struct
    type t = Paths_types.Identifier.module_

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)

    let root = function
      | `Root(r, _) -> r
      | `Module(id, _)
      | `Argument(id, _, _) -> Signature.root id

  end

  module ModuleType =
  struct
    type t = Paths_types.Identifier.module_type

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)

    let root : t -> Root.t = function
      | `ModuleType(id, _) -> Signature.root id

  end

  module Type =
  struct
    type t = Paths_types.Identifier.type_

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Constructor =
  struct
    type t = Paths_types.Identifier.constructor

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Field =
  struct
    type t = Paths_types.Identifier.field

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Extension =
  struct
    type t = Paths_types.Identifier.extension

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Exception =
  struct
    type t = Paths_types.Identifier.exception_

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Value =
  struct
    type t = Paths_types.Identifier.value

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Class =
  struct
    type t = Paths_types.Identifier.class_

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module ClassType =
  struct
    type t = Paths_types.Identifier.class_type

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Method =
  struct
    type t = Paths_types.Identifier.method_

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module InstanceVariable =
  struct
    type t = Paths_types.Identifier.instance_variable

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Label =
  struct
    type t = Paths_types.Identifier.label

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Page =
  struct
    type t = Paths_types.Identifier.page

    let equal : t -> t -> bool =
      fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

    let hash : t -> int =
      fun t -> hash (t : t :> Paths_types.Identifier.any)
  end

  module Path =
  struct
    module Module =
    struct
      type t = Paths_types.Identifier.path_module

      let equal : t -> t -> bool =
        fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

      let hash : t -> int =
        fun t -> hash (t : t :> Paths_types.Identifier.any)
    end

    module ModuleType =
    struct
      type t = Paths_types.Identifier.path_module_type

      let equal : t -> t -> bool =
        fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

      let hash : t -> int =
        fun t -> hash (t : t :> Paths_types.Identifier.any)
    end

    module Type =
    struct
      type t = Paths_types.Identifier.path_type

      let equal : t -> t -> bool =
        fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

      let hash : t -> int =
        fun t -> hash (t : t :> Paths_types.Identifier.any)
    end

    module ClassType =
    struct
      type t = Paths_types.Identifier.path_class_type

      let equal : t -> t -> bool =
        fun t1 t2 -> equal (t1 : t :> Paths_types.Identifier.any) (t2 : t :> Paths_types.Identifier.any)

      let hash : t -> int =
        fun t -> hash (t : t :> Paths_types.Identifier.any)
    end

    type t = Paths_types.Identifier.path_any
  end

  let to_reversed i =
    let rec loop acc : Signature.t -> Reversed.t = function
      | `Root (_, s) -> Reversed.Root s :: acc
      | `Module (i, s) -> loop (Reversed.Module s :: acc) i
      | `ModuleType (i, s) -> loop (Reversed.ModuleType s :: acc) i
      | `Argument (i, d, s) -> loop (Reversed.Argument (d, s) :: acc) i
    in
    loop [] i

  let signature_of_module m = (m : Module.t :> Signature.t)

  (* It would be much nicer not to have to have these functions *)

  let page_of_t : t -> Page.t = function
    | #Page.t as result -> result
    | _ -> assert false

  let signature_of_t : t -> Signature.t = function
      | #Signature.t as result -> result
      | _ -> assert false

  let class_signature_of_t : t -> ClassSignature.t = function
      | #ClassSignature.t as result -> result
      | _ -> assert false

  let datatype_of_t : t -> DataType.t = function
      | #DataType.t as result -> result
      | _ -> assert false

  let module_of_t : t -> Module.t = function
      | #Module.t as result -> result
      | _ -> assert false

  let module_type_of_t : t -> ModuleType.t = function
      | #ModuleType.t as result -> result
      | _ -> assert false

  let type_of_t : t -> Type.t = function
      | #Type.t as result -> result
      | _ -> assert false

  let constructor_of_t : t -> Constructor.t = function
      | #Constructor.t as result -> result
      | _ -> assert false

  let field_of_t : t -> Field.t = function
      | #Field.t as result -> result
      | _ -> assert false

  let extension_of_t : t -> Extension.t = function
      | #Extension.t as result -> result
      | _ -> assert false

  let exception_of_t : t -> Exception.t = function
      | #Exception.t as result -> result
      | _ -> assert false

  let value_of_t : t -> Value.t = function
      | #Value.t as result -> result
      | _ -> assert false

  let class_of_t : t -> Class.t = function
      | #Class.t as result -> result
      | _ -> assert false

  let class_type_of_t : t -> ClassType.t = function
      | #ClassType.t as result -> result
      | _ -> assert false

  let method_of_t : t -> Method.t = function
      | #Method.t as result -> result
      | _ -> assert false

  let instance_variable_of_t : t -> InstanceVariable.t = function
      | #InstanceVariable.t as result -> result
      | _ -> assert false

  let label_of_t : t -> Label.t = function
      | #Label.t as result -> result
      | _ -> assert false

  let parent_of_t : t -> Parent.t = function
      | #Parent.t as result -> result
      | _ -> assert false

end



module Path = struct

  type t = Paths_types.Path.any

  let rec equal_resolved_path : Paths_types.Resolved_path.any -> Paths_types.Resolved_path.any -> bool =
    let open Paths_types.Resolved_path in
    fun p1 p2 ->
      match p1, p2 with
      | `Identifier id1, `Identifier id2 ->
        Identifier.equal id1 id2
      | `Subst(sub1, p1), `Subst(sub2, p2) ->
        equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
        && equal_resolved_path (sub1 : module_type :> any) (sub2 : module_type :> any)
      | `SubstAlias(sub1, p1), `SubstAlias(sub2, p2) ->
        equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
        && equal_resolved_path (sub1 : module_ :> any) (sub2 : module_ :> any)
      | `Module(p1, s1), `Module(p2, s2) ->
        s1 = s2 && equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
      | `Apply(p1, arg1), `Apply(p2, arg2) ->
        equal_path
          (arg1 : Paths_types.Path.module_ :> Paths_types.Path.any)
          (arg2 : Paths_types.Path.module_ :> Paths_types.Path.any)
        && equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
      | `ModuleType(p1, s1), `ModuleType(p2, s2) ->
        s1 = s2 && equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
      | `Type(p1, s1), `Type(p2, s2) ->
        s1 = s2 && equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
      | `Class(p1, s1), `Class(p2, s2) ->
        s1 = s2 && equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
      | `ClassType(p1, s1), `ClassType(p2, s2) ->
        s1 = s2 && equal_resolved_path (p1 : module_ :> any) (p2 : module_ :> any)
      | _, _ -> false

  and equal_path : Paths_types.Path.any -> Paths_types.Path.any -> bool =
    let open Paths_types.Path in
    fun p1 p2 ->
      match p1, p2 with
      | `Resolved p1, `Resolved p2 ->
        equal_resolved_path p1 p2
      | `Root s1, `Root s2 ->
        s1 = s2
      | `Dot(p1, s1), `Dot(p2, s2) ->
        s1 = s2 && equal_path (p1 : module_ :> any) (p2 : module_ :> any)
      | `Apply(p1, arg1), `Apply(p2, arg2) ->
        equal_path (arg1 : module_ :> any) (arg2 : module_ :> any)
        && equal_path (p1 : module_ :> any) (p2 : module_ :> any)
      | _, _ -> false

  let rec hash_resolved_path : Paths_types.Resolved_path.any -> int =
    fun p ->
    let open Paths_types.Resolved_path in
    match p with
    | `Identifier id ->
      Identifier.hash id
    | `Subst(sub, p) ->
      Hashtbl.hash (19, hash_resolved_path (sub : module_type :> any),
                    hash_resolved_path (p : module_ :> any))
    | `SubstAlias(sub, p) ->
      Hashtbl.hash (20, hash_resolved_path (sub : module_ :> any),
                    hash_resolved_path (p : module_ :> any))
    | `Hidden p -> Hashtbl.hash (21, hash_resolved_path (p : module_ :> any))
    | `Module(p, s) ->
      Hashtbl.hash (22, hash_resolved_path (p : module_ :> any), s)
    | `Canonical(p, canonical) ->
      Hashtbl.hash (23, hash_resolved_path (p : module_ :> any),
                    hash_path (canonical : Paths_types.Path.module_ :> Paths_types.Path.any))
    | `Apply(p, arg) ->
      Hashtbl.hash (24, hash_resolved_path (p : module_ :> any),
                    hash_path (arg : Paths_types.Path.module_ :> Paths_types.Path.any))
    | `ModuleType(p, s) ->
      Hashtbl.hash (25, hash_resolved_path (p : module_ :> any), s)
    | `Type(p, s) ->
      Hashtbl.hash (26, hash_resolved_path (p : module_ :> any), s)
    | `Class(p, s) ->
      Hashtbl.hash (27, hash_resolved_path (p : module_ :> any), s)
    | `ClassType(p, s) ->
      Hashtbl.hash (28, hash_resolved_path (p : module_ :> any), s)

  and hash_path : Paths_types.Path.any -> int =
    fun p ->
    let open Paths_types.Path in
    match p with
    | `Resolved p -> hash_resolved_path p
    | `Root s ->
      Hashtbl.hash (29, s)
    | `Forward s ->
      Hashtbl.hash (30, s)
    | `Dot(p, s) ->
      Hashtbl.hash (31, hash_path (p : module_ :> any), s)
    | `Apply(p, arg) ->
      Hashtbl.hash (32, hash_path (p : module_ :> any), hash_path (arg : module_ :> any))

  let rec is_resolved_hidden : Paths_types.Resolved_path.any -> bool =
    let open Paths_types.Resolved_path in
    function
    | `Identifier _ -> false
    | `Canonical (_, _) -> false
    | `Hidden _ -> true
    | `Subst(p1, p2) -> is_resolved_hidden (p1 : module_type :> any) || is_resolved_hidden (p2 : module_ :> any)
    | `SubstAlias(p1, p2) -> is_resolved_hidden (p1 : module_ :> any) || is_resolved_hidden (p2 : module_ :> any)
    | `Module (p, _) -> is_resolved_hidden (p : module_ :> any)
    | `Apply (p, _) -> is_resolved_hidden (p : module_ :> any)
    | `ModuleType (p, _) -> is_resolved_hidden (p : module_ :> any)
    | `Type (p, _) -> is_resolved_hidden (p : module_ :> any)
    | `Class (p, _) -> is_resolved_hidden (p : module_ :> any)
    | `ClassType (p, _) -> is_resolved_hidden (p : module_ :> any)

  and is_path_hidden : Paths_types.Path.any -> bool =
    let open Paths_types.Path in
    function
    | `Resolved r -> is_resolved_hidden r
    | `Root _ -> false
    | `Forward _ -> false
    | `Dot(p, _) -> is_path_hidden (p : module_ :> any)
    | `Apply(p1, p2) -> is_path_hidden (p1 : module_ :> any) || is_path_hidden (p2 : module_ :> any)

  module Resolved = struct


    type t = Paths_types.Resolved_path.any

    let rec parent_module_type_identifier : Paths_types.Resolved_path.module_type -> Identifier.Signature.t = function
      | `Identifier id -> (id : Identifier.ModuleType.t :> Identifier.Signature.t) 
      | `ModuleType(m, n) -> `ModuleType(parent_module_identifier m, n)

    and parent_module_identifier : Paths_types.Resolved_path.module_ -> Identifier.Signature.t = function
      | `Identifier id -> (id : Identifier.Module.t :> Identifier.Signature.t)
      | `Subst(sub, _) -> parent_module_type_identifier sub
      | `SubstAlias(sub, _) -> parent_module_identifier sub
      | `Hidden p -> parent_module_identifier p
      | `Module(m, n) -> `Module(parent_module_identifier m, n)
      | `Canonical(_, `Resolved p) -> parent_module_identifier p
      | `Canonical(p, _) -> parent_module_identifier p
      | `Apply(m, _) -> parent_module_identifier m

    let equal p1 p2 = equal_resolved_path p1 p2

    let hash p = hash_resolved_path p

    type rebase_result =
      | Stop of Paths_types.Resolved_path.module_
      | Continue of Paths_types.Identifier.path_module * Reversed.t

    let rec rebase_module_path : Reversed.t -> Paths_types.Resolved_path.module_ -> rebase_result =
      fun new_base t ->
      match t with
      | `Identifier id ->
        let rev = Identifier.(to_reversed @@ signature_of_module id) in
        let new_base' = Reversed.remove_prefix rev ~of_:new_base in
        if new_base == new_base' then
          Stop t
        else
          Continue (id, new_base')
      | `Subst (_, p)
      | `SubstAlias (_, p)
      | `Hidden p -> begin
          match rebase_module_path new_base p with
          | Stop p' when p == p' -> Stop t
          | otherwise -> otherwise
        end
      | `Module (m, s) ->
        begin match rebase_module_path new_base m with
          | Stop m' -> if m == m' then Stop t else Stop (`Module (m', s))
          | Continue (id, new_base) ->
            let id = `Module (Identifier.signature_of_module id, s) in
            match new_base with
            | Reversed.Module s' :: rest when s = s' ->
              Continue (id, rest)
            | _ ->
              Stop (`Identifier id)
        end
      | `Canonical (_, `Resolved p) ->
        (* We only care about printing at this point, so let's drop the lhs. *)
        rebase_module_path new_base p
      | `Canonical (rp, p) ->
        begin match rebase_module_path new_base rp with
          | Stop rp' -> Stop (`Canonical (rp', p))
          | _ ->
            (* We might come back at some point with a resolved rhs? So we don't want to
               drop it. *)
            Stop t
        end
      | `Apply _ -> Stop t
    (* TODO: rewrite which side? *)

    let rec equal_identifier :
      Identifier.t -> t -> bool =
      fun id p ->
      match id, p with
      | _, `Identifier id' -> Identifier.equal id id'
      | `Module (id, s1), `Module (p, s2) when s1 = s2 ->
        equal_identifier
          (id : Paths_types.Identifier.signature :> Paths_types.Identifier.any)
          (p : Paths_types.Resolved_path.module_ :> Paths_types.Resolved_path.any)
      | `ModuleType (id, s1), `ModuleType (p, s2) when s1 = s2 ->
        equal_identifier
          (id : Paths_types.Identifier.signature :> Paths_types.Identifier.any)
          (p : Paths_types.Resolved_path.module_ :> Paths_types.Resolved_path.any)
      | _, _ ->
        false

    module Module = struct

      type t = Paths_types.Resolved_path.module_

      let of_ident id = `Identifier id

      let equal m1 m2 = equal_resolved_path (m1 : t :> Paths_types.Resolved_path.any) (m2 : t :> Paths_types.Resolved_path.any)

      let hash m = hash (m : t :> Paths_types.Resolved_path.any)

      let is_hidden m = is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

        let rec identifier = function
        | `Identifier id -> id
        | `Subst(_, p) -> identifier p
        | `SubstAlias(_, p) -> identifier p
        | `Hidden p -> identifier p
        | `Module(m, n) -> `Module(parent_module_identifier m, n)
        | `Canonical(_, `Resolved p) -> identifier p
        | `Canonical(p, _) -> identifier p
        | `Apply(m, _) -> identifier m

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
        match t with
        | `Identifier _ -> t
        | `Subst _ -> t (* TODO: rewrite which side? *)
        | `SubstAlias _ -> t (* TODO: rewrite which side? *)
        | `Hidden p  -> begin
            match rebase_module_path new_base p with
            | Stop p' ->
              if p == p' then t else p'
            | Continue (id, _) -> `Identifier id
          end
        | `Module (mp, s) ->
          begin match rebase_module_path new_base mp with
            | Continue (id, _) ->
              `Identifier (`Module (Identifier.signature_of_module id, s))
            | Stop mp' -> `Module (mp', s)
          end
        | `Canonical (p, `Resolved rp) ->
          begin match rebase_module_path new_base rp with
            | Continue (id, _) -> `Identifier id
            | Stop rp ->
              (* Easier to reexport a canonical than get the type for rp right... *)
              `Canonical (p, `Resolved rp)
          end
        | `Canonical (rp, p) ->
          begin match rebase_module_path new_base rp with
            | Stop rp' -> `Canonical (rp', p)
            | _ ->
              (* We might come back at some point with a resolved rhs? So we don't want to
                 drop it. *)
              t
          end
        | `Apply (mp, arg) ->
          begin match rebase_module_path new_base mp with
            | Continue (id, _) -> `Apply (`Identifier id, arg)
            | Stop mp' -> `Apply (mp', arg)
          end

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t

      let equal_identifier :
        Identifier.Path.Module.t -> t -> bool =
        fun id t ->
        equal_identifier
          (id : Identifier.Path.Module.t :> Identifier.t)
          (t : t :> Paths_types.Resolved_path.any)
    end

    module ModuleType = struct

      type t = Paths_types.Resolved_path.module_type

      let of_ident id = `Identifier id

      let equal m1 m2 = equal_resolved_path (m1 : t :> Paths_types.Resolved_path.any) (m2 : t :> Paths_types.Resolved_path.any)

      let hash m = hash (m : t :> Paths_types.Resolved_path.any)

      let is_hidden m = is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let identifier = function
        | `Identifier id -> id
        | `ModuleType(m, n) -> `ModuleType(parent_module_identifier m, n)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
        match t with
        | `Identifier _ -> t
        | `ModuleType (mp, s) ->
          begin match rebase_module_path new_base mp with
            | Continue (id, _) ->
              `Identifier (`ModuleType (Identifier.signature_of_module id, s))
            | Stop mp' -> `ModuleType (mp', s)
          end

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t

      let equal_identifier :
        Identifier.Path.ModuleType.t -> t -> bool =
        fun id t ->
        equal_identifier
          (id : Identifier.Path.ModuleType.t :> Identifier.t)
          (t : t :> Paths_types.Resolved_path.any)
    end

    module Type = struct

      type t = Paths_types.Resolved_path.type_

      let of_ident id = `Identifier id

      let equal m1 m2 = equal_resolved_path (m1 : t :> Paths_types.Resolved_path.any) (m2 : t :> Paths_types.Resolved_path.any)

      let hash m = hash (m : t :> Paths_types.Resolved_path.any)

      let is_hidden m = is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let identifier = function
        | `Identifier id -> id
        | `Type(m, n) -> `Type(parent_module_identifier m, n)
        | `Class(m, n) -> `Class(parent_module_identifier m, n)
        | `ClassType(m, n) -> `ClassType(parent_module_identifier m, n)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
        match t with
        | `Identifier _ -> t
        | `Type (mp, s) ->
          begin match rebase_module_path new_base mp with
            | Continue (id, _) ->
              `Identifier (`Type (Identifier.signature_of_module id, s))
            | Stop mp' -> `Type (mp', s)
          end
        | `Class (mp, s) ->
          begin match rebase_module_path new_base mp with
            | Continue (id, _) ->
              `Identifier (`Class (Identifier.signature_of_module id, s))
            | Stop mp' -> `Class (mp', s)
          end
        | `ClassType (mp, s) ->
          begin match rebase_module_path new_base mp with
            | Continue (id, _) ->
              `Identifier (`ClassType (Identifier.signature_of_module id, s))
            | Stop mp' -> `ClassType (mp', s)
          end

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t

      let equal_identifier :
        Identifier.Path.Type.t -> t -> bool =
        fun id t ->
        equal_identifier
          (id : Identifier.Path.Type.t :> Identifier.t)
          (t : t :> Paths_types.Resolved_path.any)

    end

    module ClassType = struct

      type t = Paths_types.Resolved_path.class_type

      let of_ident id = `Identifier id

      let equal m1 m2 = equal_resolved_path (m1 : t :> Paths_types.Resolved_path.any) (m2 : t :> Paths_types.Resolved_path.any)

      let hash m = hash (m : t :> Paths_types.Resolved_path.any)

      let is_hidden m = is_resolved_hidden (m : t :> Paths_types.Resolved_path.any)

      let identifier = function
        | `Identifier id -> id
        | `Class(m, n) -> `Class(parent_module_identifier m, n)
        | `ClassType(m, n) -> `ClassType(parent_module_identifier m, n)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
        match t with
        | `Identifier _ -> t
        | `Class (mp, s) ->
          begin match rebase_module_path new_base mp with
            | Continue (id, _) ->
              `Identifier (`Class (Identifier.signature_of_module id, s))
            | Stop mp' -> `Class (mp', s)
          end
        | `ClassType (mp, s) ->
          begin match rebase_module_path new_base mp with
            | Continue (id, _) ->
              `Identifier (`ClassType (Identifier.signature_of_module id, s))
            | Stop mp' -> `ClassType (mp', s)
          end

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t

      let equal_identifier :
        Identifier.Path.ClassType.t -> t -> bool =
        fun id t ->
        equal_identifier
          (id : Identifier.Path.ClassType.t :> Identifier.t)
          (t : t :> Paths_types.Resolved_path.any)
    end

    let module_of_t : t -> Module.t = function
        | `Identifier (#Identifier.Path.Module.t)
        | #Paths_types.Resolved_path.module_no_id as x -> x
        | _ -> assert false

    let module_type_of_t : t -> ModuleType.t = function
        | `Identifier (#Identifier.Path.ModuleType.t)
        | #Paths_types.Resolved_path.module_type_no_id as x -> x
        | _ -> assert false

    let type_of_t : t -> Type.t = function
        | `Identifier (#Identifier.Path.Type.t)
        | #Paths_types.Resolved_path.type_no_id as x -> x
        | _ -> assert false

    let class_type_of_t : t -> ClassType.t = function
        | `Identifier (#Identifier.Path.ClassType.t)
        | #Paths_types.Resolved_path.class_type_no_id as x -> x
        | _ -> assert false


    let rec identifier : t -> Identifier.t = function
      | `Identifier id -> id
      | `Subst(_, p) -> identifier (p :> t)
      | `SubstAlias(_, p) -> identifier (p :> t)
      | `Hidden p -> identifier (p :> t)
      | `Module(m, n) -> `Module(parent_module_identifier m, n)
      | `Canonical(_, `Resolved p) -> identifier (p :> t)
      | `Canonical(p, _) -> identifier (p :> t)
      | `Apply(m, _) -> identifier (m :> t)
      | `Type(m, n) -> `Type(parent_module_identifier m, n)
      | `ModuleType(m, n) -> `ModuleType(parent_module_identifier m, n)
      | `Class(m, n) -> `Class(parent_module_identifier m, n)
      | `ClassType(m, n) -> `ClassType(parent_module_identifier m, n)

  end

  module Module =
  struct
    type t = Paths_types.Path.module_

    let equal : t -> t -> bool =
      fun p1 p2 ->
      equal_path (p1 : t :> Paths_types.Path.any) (p2 : t :> Paths_types.Path.any)

    let hash : t -> int =
      fun t ->
      hash_path (t : t :> Paths_types.Path.any)

    let is_hidden : t -> bool =
      fun t ->
      is_path_hidden (t : t :> Paths_types.Path.any)
  end

  module ModuleType =
  struct
    type t = Paths_types.Path.module_type

    let equal : t -> t -> bool =
      fun p1 p2 ->
      equal_path (p1 : t :> Paths_types.Path.any) (p2 : t :> Paths_types.Path.any)

    let hash : t -> int =
      fun t ->
      hash_path (t : t :> Paths_types.Path.any)

    let is_hidden : t -> bool =
      fun t ->
      is_path_hidden (t : t :> Paths_types.Path.any)
  end

  module Type =
  struct
    type t = Paths_types.Path.type_

    let equal : t -> t -> bool =
      fun p1 p2 ->
      equal_path (p1 : t :> Paths_types.Path.any) (p2 : t :> Paths_types.Path.any)

    let hash : t -> int =
      fun t ->
      hash_path (t : t :> Paths_types.Path.any)

    let is_hidden : t -> bool =
      fun t ->
      is_path_hidden (t : t :> Paths_types.Path.any)
  end

  module ClassType =
  struct
    type t = Paths_types.Path.class_type

    let equal : t -> t -> bool =
      fun p1 p2 ->
      equal_path (p1 : t :> Paths_types.Path.any) (p2 : t :> Paths_types.Path.any)

    let hash : t -> int =
      fun t ->
      hash_path (t : t :> Paths_types.Path.any)

    let is_hidden : t -> bool =
      fun t ->
      is_path_hidden (t : t :> Paths_types.Path.any)
  end

  let module_of_t : t -> Module.t = function
    | `Resolved (`Identifier (#Paths_types.Identifier.path_module))
    | `Resolved (#Paths_types.Resolved_path.module_no_id)
    | `Root _
    | `Forward _
    | `Dot (_,_)
    | `Apply (_,_) as x -> x
    | _ -> assert false

  let module_type_of_t : t -> ModuleType.t = function
    | `Resolved (`Identifier (#Paths_types.Identifier.path_module_type))
    | `Resolved (#Paths_types.Resolved_path.module_type_no_id)
    | `Dot (_,_) as x -> x
    | _ -> assert false

  let type_of_t : t -> Type.t = function
    | `Resolved (`Identifier (#Paths_types.Identifier.path_type))
    | `Resolved (#Paths_types.Resolved_path.type_no_id)
    | `Dot (_,_) as x -> x
    | _ -> assert false

  let class_type_of_t : t -> ClassType.t = function
    | `Resolved (`Identifier (#Paths_types.Identifier.path_class_type))
    | `Resolved (#Paths_types.Resolved_path.class_type_no_id)
    | `Dot (_,_) as x -> x
    | _ -> assert false

  let module_ : Module.t -> ModuleName.t -> Module.t = fun p name ->
    match p with
    | `Resolved p -> `Resolved (`Module(p, name))
    | p -> `Dot(p, ModuleName.to_string name)

  let apply p arg =
    match p with
    | `Resolved p -> `Resolved (`Apply(p, arg))
    | p -> `Apply(p, arg)

  let module_type p name =
    match p with
    | `Resolved p -> `Resolved (`ModuleType(p, name))
    | p -> `Dot(p, ModuleTypeName.to_string name)

  let is_hidden = is_path_hidden

  let equal = equal_path

  let hash = hash_path
end



module Fragment = struct

  module Resolved = struct

    type t = Paths_types.Resolved_fragment.any

    let t_of_module m = (m : Paths_types.Resolved_fragment.module_ :> t)
    let t_of_signature s = (s : Paths_types.Resolved_fragment.signature :> t)

    let equal p1 p2 =
      let rec loop : t -> t -> bool =
        fun p1 p2 ->
          match p1, p2 with
          | `Root, `Root -> true
          | `Subst(sub1, p1), `Subst(sub2, p2) ->
            Path.Resolved.ModuleType.equal sub1 sub2
            && loop (t_of_module p1) (t_of_module p2)
          | `SubstAlias(sub1, p1), `SubstAlias(sub2, p2) ->
            Path.Resolved.Module.equal sub1 sub2
            && loop (t_of_module p1) (t_of_module p2)
          | `Module(p1, s1), `Module(p2, s2) ->
            s1 = s2 && loop (t_of_signature p1) (t_of_signature p2)
          | `Type(p1, s1), `Type(p2, s2) ->
            s1 = s2 && loop (t_of_signature p1) (t_of_signature p2)
          | `Class(p1, s1), `Class(p2, s2) ->
            s1 = s2 && loop (t_of_signature p1) (t_of_signature p2)
          | `ClassType(p1, s1), `ClassType(p2, s2) ->
            s1 = s2 && loop (t_of_signature p1) (t_of_signature p2)
          | _, _ -> false
      in
      loop p1 p2

    let hash p =
      let rec loop : t -> int =
        fun p ->
          match p with
          | `Root -> Hashtbl.hash 32
          | `Subst(sub, p) ->
            Hashtbl.hash (34, Path.Resolved.ModuleType.hash sub, loop (t_of_module p))
          | `SubstAlias(sub, p) ->
            Hashtbl.hash (35, Path.Resolved.Module.hash sub, loop (t_of_module p))
          | `Module(p, s) ->
            Hashtbl.hash (36, loop (t_of_signature p), s)
          | `Type(p, s) ->
            Hashtbl.hash (37, loop (t_of_signature p), s)
          | `Class(p, s) ->
            Hashtbl.hash (38, loop (t_of_signature p), s)
          | `ClassType(p, s) ->
            Hashtbl.hash (39, loop (t_of_signature p), s)
      in
      loop p

    let sig_of_mod m =
      let open Paths_types.Resolved_fragment in 
      (m : module_ :> signature)

    let rec parent_resolved_path : Path.Resolved.Module.t -> Paths_types.Resolved_fragment.signature -> Path.Resolved.Module.t = fun root -> 
      function
      | `Root -> root
      | `Subst(sub, p) ->
        `Subst(sub, parent_resolved_path root (sig_of_mod p))
      | `SubstAlias(sub, p) ->
        `SubstAlias(sub, parent_resolved_path root (sig_of_mod p))
      | `Module(m, n) ->
        `Module(parent_resolved_path root m, n)

    let rec parent_unresolved_path : Path.Module.t -> Paths_types.Resolved_fragment.signature -> Path.Module.t = fun root -> function
      | `Root -> root
      | `Subst(_, p) -> parent_unresolved_path root (sig_of_mod p)
      | `SubstAlias(_, p) -> parent_unresolved_path root (sig_of_mod p)
      | `Module(m, n) -> `Dot(parent_unresolved_path root m, (ModuleName.to_string n))

    let parent_path root frag =
      match root with
      | `Resolved root -> `Resolved (parent_resolved_path root frag)
      | _ -> parent_unresolved_path root frag

    type base_name =
      | Base
      | Branch of ModuleName.t * Paths_types.Resolved_fragment.signature

    let rec split_parent : Paths_types.Resolved_fragment.signature -> base_name = function
      | `Root -> Base
      | `Subst(_, p) -> split_parent (sig_of_mod p)
      | `SubstAlias(_, p) -> split_parent (sig_of_mod p)
      | `Module(p, name) ->
        match split_parent p with
        | Base -> Branch(name, `Root)
        | Branch(base, m) -> Branch(base, `Module(m, name))


    module Signature =
    struct
      type t = Paths_types.Resolved_fragment.signature

      let equal s1 s2 =
        equal (s1 : t :> Paths_types.Resolved_fragment.any) (s2 : t :> Paths_types.Resolved_fragment.any)

      let hash s =
        hash (s : t :> Paths_types.Resolved_fragment.any)

      let rec resolved_path : Path.Resolved.Module.t -> t -> Path.Resolved.Module.t =
        fun root frag ->
        match frag with
        | `Root -> root
        | `Subst(sub, p) ->
          `Subst(sub, resolved_path root (sig_of_mod p))
        | `SubstAlias(sub, p) ->
          `SubstAlias(sub, resolved_path root (sig_of_mod p))
        | `Module(m, n) ->
          `Module(parent_resolved_path root m, n)

      let rec unresolved_path : Path.Module.t -> t -> Path.Module.t =
        fun root frag ->
        match frag with
        | `Root -> root
        | `Subst(_, p) -> unresolved_path root (sig_of_mod p)
        | `SubstAlias(_, p) -> unresolved_path root (sig_of_mod p)
        | `Module(m, n) -> `Dot(parent_unresolved_path root m, ModuleName.to_string n)

      let path : Path.Module.t -> t -> Path.Module.t =
        fun root frag ->
        match root with
        | `Resolved root -> `Resolved (resolved_path root frag)
        | _ -> unresolved_path root frag

      let rec split : t -> string * t option = function
        | `Root -> "", None
        | `Subst(_,p) -> split (sig_of_mod p)
        | `SubstAlias(_,p) -> split (sig_of_mod p)
        | `Module (m, name) -> begin
            match split_parent m with
            | Base -> (ModuleName.to_string name, None)
            | Branch(base,m) -> ModuleName.to_string base, Some (`Module(m,name))
          end

      let rec identifier : Identifier.Signature.t -> t -> Identifier.Signature.t =
        fun root -> function
          | `Root -> root
          | `Subst(_, p) -> identifier root (sig_of_mod p)
          | `SubstAlias(_, p) -> identifier root (sig_of_mod p)
          | `Module(m, n) -> `Module (identifier root m, n)

    end

    module Module =
    struct
      type t = Paths_types.Resolved_fragment.module_

      let equal s1 s2 =
        equal (s1 : t :> Paths_types.Resolved_fragment.any) (s2 : t :> Paths_types.Resolved_fragment.any)

      let hash s =
        hash (s : t :> Paths_types.Resolved_fragment.any)

      let resolved_path : Path.Resolved.Module.t -> t -> Path.Resolved.Module.t =
        fun root frag ->
        match frag with
        | `Subst(sub, p) ->
          `Subst(sub, Signature.resolved_path root (sig_of_mod p))
        | `SubstAlias(sub, p) ->
          `SubstAlias(sub, Signature.resolved_path root (sig_of_mod p))
        | `Module(m, n) ->
          `Module(parent_resolved_path root m, n)

      let unresolved_path : Path.Module.t -> t -> Path.Module.t =
        fun root frag ->
        match frag with
        | `Subst(_, p) -> Signature.unresolved_path root (sig_of_mod p)
        | `SubstAlias(_, p) -> Signature.unresolved_path root (sig_of_mod p)
        | `Module(m, n) -> `Dot(parent_unresolved_path root m, ModuleName.to_string n)

      let path : Path.Module.t -> t -> Path.Module.t =
        fun root frag ->
        match root with
        | `Resolved root -> `Resolved (resolved_path root frag)
        | _ -> unresolved_path root frag

      let rec split : t -> string * t option = function
        | `Subst(_,p) -> split p
        | `SubstAlias(_,p) -> split p
        | `Module (m, name) -> begin
            match split_parent m with
            | Base -> (ModuleName.to_string name, None)
            | Branch(base,m) -> ModuleName.to_string base, Some (`Module(m,name))
          end

      let rec identifier : Identifier.Signature.t -> t -> Identifier.Path.Module.t =
        fun root -> function
          | `Subst(_, p) -> identifier root p
          | `SubstAlias(_, p) -> identifier root p
          | `Module(m, n) -> `Module (Signature.identifier root m, n)

    end

    module Type =
    struct
      type t = Paths_types.Resolved_fragment.type_

      let equal s1 s2 =
        equal (s1 : t :> Paths_types.Resolved_fragment.any) (s2 : t :> Paths_types.Resolved_fragment.any)

      let hash s =
        hash (s : t :> Paths_types.Resolved_fragment.any)

      let resolved_path : Path.Resolved.Module.t -> t -> Path.Resolved.Type.t = fun root ->
        function
        | `Type(m,n) -> `Type (Signature.resolved_path root m, n)
        | `Class(m,n) -> `Class (Signature.resolved_path root m, n)
        | `ClassType(m,n) -> `ClassType (Signature.resolved_path root m, n)

      let unresolved_path : Path.Module.t -> t -> Path.Type.t = fun root ->
        function
        | `Type(m,n) -> `Dot (Signature.unresolved_path root m, TypeName.to_string n)
        | `Class(m,n) -> `Dot (Signature.unresolved_path root m, ClassName.to_string n)
        | `ClassType(m,n) -> `Dot (Signature.unresolved_path root m, ClassTypeName.to_string n)

      let path :  Path.Module.t -> t -> Path.Type.t = fun root frag ->
        match root with
        | `Resolved root -> `Resolved (resolved_path root frag)
        | _ -> unresolved_path root frag

      let split : t -> string * t option =
        function
        | `Type (m,name) -> begin
            match split_parent m with
            | Base -> TypeName.to_string name, None
            | Branch(base, m) -> ModuleName.to_string base, Some (`Type(m, name))
          end
        | `Class(m, name) -> begin
            match split_parent m with
            | Base -> ClassName.to_string name, None
            | Branch(base, m) -> ModuleName.to_string base, Some (`Class(m, name))
          end
        | `ClassType(m, name) -> begin
            match split_parent m with
            | Base -> ClassTypeName.to_string name, None
            | Branch(base, m) -> ModuleName.to_string base, Some (`ClassType(m, name))
          end

      let identifier : Identifier.Signature.t -> t -> Identifier.Path.Type.t =
        fun root -> function
          | `Type(m, n) -> `Type(Signature.identifier root m, n)
          | `Class(m, n) -> `Class(Signature.identifier root m, n)
          | `ClassType(m, n) -> `ClassType(Signature.identifier root m, n)

    end

    let signature_of_t : t -> Signature.t = function
      | #Signature.t as x -> x
      | _ -> assert false

    let module_of_t : t -> Module.t = function
      | #Module.t as x -> x
      | _ -> assert false

    let type_of_t : t -> Type.t = function
      | #Type.t as x -> x
      | _ -> assert false

    let rec identifier : Identifier.Signature.t -> t -> Identifier.t =
      fun root -> function
        | `Root -> (root :> Identifier.t)
        | `Subst(_, p) -> identifier root (p :> t)
        | `SubstAlias(_, p) -> identifier root (p :> t)
        | `Module(m, n) -> `Module (Signature.identifier root m, n)
        | `Type(m, n) -> `Type(Signature.identifier root m, n)
        | `Class(m, n) -> `Class(Signature.identifier root m, n)
        | `ClassType(m, n) -> `ClassType(Signature.identifier root m, n)


  end

  type t = Paths_types.Fragment.any

  let rec parent_path : Path.Module.t -> Paths_types.Fragment.signature -> Path.Module.t = fun root -> function
    | `Resolved r -> Resolved.parent_path root r
    | `Dot(m, n) -> `Dot(parent_path root m, n)

  type base_name =
    | Base
    | Branch of ModuleName.t * Paths_types.Fragment.signature

  let rec split_parent : Paths_types.Fragment.signature -> base_name =
    function
    | `Resolved r -> begin
        match Resolved.split_parent r with
        | Resolved.Base -> Base
        | Resolved.Branch(base, m) -> Branch(base, `Resolved m)
      end
    | `Dot(m,name) -> begin
        match split_parent m with
        | Base -> Branch(ModuleName.of_string name,`Resolved `Root)
        | Branch(base,m) -> Branch(base, `Dot(m,name))
      end

  let equal p1 p2 =
    let rec loop : t -> t -> bool =
      fun p1 p2 ->
        match p1, p2 with
        | `Resolved p1, `Resolved p2 ->
          Resolved.equal p1 p2
        | `Dot(p1, s1), `Dot(p2, s2) ->
          s1 = s2 && loop (p1 : Paths_types.Fragment.signature :> t) (p2 : Paths_types.Fragment.signature :> t)
        | _, _ -> false
    in
    loop p1 p2

  let hash p =
    let rec loop : t -> int =
      fun p ->
        match p with
        | `Resolved p -> Resolved.hash p
        | `Dot(p, s) ->
          Hashtbl.hash (40, loop (p : Paths_types.Fragment.signature :> t), s)
    in
    loop p

  module Signature = struct
    type t = Paths_types.Fragment.signature

    let equal t1 t2 =
      equal (t1 : t :> Paths_types.Fragment.any) (t2 : t :> Paths_types.Fragment.any)

    let hash t =
      hash (t : t :> Paths_types.Fragment.any)

    let split : t -> string * t option = function
      | `Resolved r ->
        let base, m = Resolved.Signature.split r in
        let m =
          match m with
          | None -> None
          | Some m -> Some (`Resolved m)
        in
        base, m
      | `Dot(m, name) ->
        match split_parent m with
        | Base -> name, None
        | Branch(base, m) -> ModuleName.to_string base, Some(`Dot(m, name))

    let path : Path.Module.t -> t -> Path.Module.t =
      fun root -> function
        | `Resolved r -> Resolved.Signature.path root r
        | `Dot(m, s) -> `Dot(parent_path root m, s)

  end

  module Module = struct
    type t = Paths_types.Fragment.module_

    let equal t1 t2 =
      equal (t1 : t :> Paths_types.Fragment.any) (t2 : t :> Paths_types.Fragment.any)

    let hash t =
      hash (t : t :> Paths_types.Fragment.any)

    let split : t -> string * t option = function
      | `Resolved r ->
        let base, m = Resolved.Module.split r in
        let m =
          match m with
          | None -> None
          | Some m -> Some (`Resolved m)
        in
        base, m
      | `Dot(m, name) ->
        match split_parent m with
        | Base -> name, None
        | Branch(base, m) -> ModuleName.to_string base, Some(`Dot(m, name))

    let path : Path.Module.t -> t -> Path.Module.t =
      fun root -> function
        | `Resolved r -> Resolved.Module.path root r
        | `Dot(m, s) -> `Dot(parent_path root m, s)
  end

  module Type = struct
    type t = Paths_types.Fragment.type_

    let equal t1 t2 =
      equal (t1 : t :> Paths_types.Fragment.any) (t2 : t :> Paths_types.Fragment.any)

    let hash t =
      hash (t : t :> Paths_types.Fragment.any)

    let split : t -> string * t option = function
      | `Resolved r ->
        let base, m = Resolved.Type.split r in
        let m =
          match m with
          | None -> None
          | Some m -> Some (`Resolved m)
        in
        base, m
      | `Dot(m, name) ->
        match split_parent m with
        | Base -> name, None
        | Branch(base, m) -> ModuleName.to_string base, Some(`Dot(m, name))

    let path : Path.Module.t -> t -> Path.Type.t =
      fun root -> function
        | `Resolved r -> Resolved.Type.path root r
        | `Dot(m, s) -> `Dot(parent_path root m, s)

  end

  let signature_of_t : t -> Signature.t = function
    | `Resolved (#Resolved.Signature.t) 
    | `Dot (_,_) as x -> x
    | _ -> assert false

  let module_of_t : t -> Module.t = function
    | `Resolved (#Resolved.Module.t) 
    | `Dot (_,_) as x -> x
    | _ -> assert false

  let type_of_t : t -> Type.t = function
    | `Resolved (#Resolved.Type.t) 
    | `Dot (_,_) as x -> x
    | _ -> assert false

end


module Reference = struct

  let rec hash_resolved : Paths_types.Resolved_reference.any -> int =
    fun p ->
      let open Paths_types.Resolved_reference in
      match p with
      | `Identifier id ->
        Identifier.hash id
      | `SubstAlias (r1, r2) ->
        Hashtbl.hash (41, Path.Resolved.Module.hash r1, hash_resolved (r2 : module_ :> any ))
      | `Module(p, s) ->
        Hashtbl.hash (42, hash_resolved (p : signature :> any), s)
      | `Canonical (rp, p) ->
        Hashtbl.hash (43, hash_resolved (rp : module_ :> any), hash_reference (p : Paths_types.Reference.module_ :> Paths_types.Reference.any))
      | `ModuleType(p, s) ->
        Hashtbl.hash (44, hash_resolved (p : signature :> any), s)
      | `Type(p, s) ->
        Hashtbl.hash (45, hash_resolved (p : signature :> any), s)
      | `Constructor(p, s) ->
        Hashtbl.hash (46, hash_resolved (p : datatype :> any), s)
      | `Field(p, s) ->
        Hashtbl.hash (47, hash_resolved (p : parent :> any), s)
      | `Extension(p, s) ->
        Hashtbl.hash (48, hash_resolved (p : signature :> any), s)
      | `Exception(p, s) ->
        Hashtbl.hash (49, hash_resolved (p : signature :> any), s)
      | `Value(p, s) ->
        Hashtbl.hash (50, hash_resolved (p : signature :> any), s)
      | `Class(p, s) ->
        Hashtbl.hash (51, hash_resolved (p : signature :> any), s)
      | `ClassType(p, s) ->
        Hashtbl.hash (52, hash_resolved (p : signature :> any), s)
      | `Method(p, s) ->
        Hashtbl.hash (53, hash_resolved (p : class_signature :> any), s)
      | `InstanceVariable(p, s) ->
        Hashtbl.hash (54, hash_resolved (p : class_signature :> any), s)
      | `Label(p, s) ->
        Hashtbl.hash (55, hash_resolved (p : label_parent :> any), s)

  and hash_reference : Paths_types.Reference.any -> int =
    fun p ->
      let open Paths_types.Reference in
      match p with
      | `Resolved p -> hash_resolved p
      | `Root (s, k) -> Hashtbl.hash (56, s, k)
      | `Dot (p,s) -> Hashtbl.hash (57, hash_reference (p : label_parent :> any), s)
      | `Module (p,s) -> Hashtbl.hash (58, hash_reference (p : signature :> any), s)
      | `ModuleType (p,s) -> Hashtbl.hash (59, hash_reference (p : signature :> any), s)
      | `Type (p,s) -> Hashtbl.hash (60, hash_reference (p : signature :> any), s)
      | `Constructor (p,s) -> Hashtbl.hash (61, hash_reference (p : datatype :> any), s)
      | `Field (p,s) -> Hashtbl.hash (62, hash_reference (p : parent :> any), s)
      | `Extension (p,s) -> Hashtbl.hash (63, hash_reference (p : signature :> any), s)
      | `Exception (p,s) -> Hashtbl.hash (64, hash_reference (p : signature :> any), s)
      | `Value (p,s) -> Hashtbl.hash (65, hash_reference (p : signature :> any), s)
      | `Class (p,s) -> Hashtbl.hash (66, hash_reference (p : signature :> any), s)
      | `ClassType (p,s) -> Hashtbl.hash (67, hash_reference (p : signature :> any), s)
      | `Method (p,s) -> Hashtbl.hash (68, hash_reference (p : class_signature :> any), s)
      | `InstanceVariable (p,s) -> Hashtbl.hash (69, hash_reference (p : class_signature :> any), s)
      | `Label (p,s) -> Hashtbl.hash (70, hash_reference (p : label_parent :> any), s)

  let rec resolved_equal : Paths_types.Resolved_reference.any -> Paths_types.Resolved_reference.any -> bool = 
    let open Paths_types.Resolved_reference in
    fun id1 id2 ->
      match id1, id2 with
      | `Identifier id1, `Identifier id2 ->
        Identifier.equal id1 id2
      | `SubstAlias (r1, m1), `SubstAlias (r2, m2) ->
        Path.Resolved.equal
          (r1 : Path.Resolved.Module.t :> Path.Resolved.t)
          (r2 : Path.Resolved.Module.t :> Path.Resolved.t)
        && resolved_equal (m1 : module_ :> any) (m2 : module_ :> any)
      | `Module(r1, s1), `Module(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Canonical(m1, r1), `Canonical(m2, r2) ->
        equal
          (r1 : Paths_types.Reference.module_ :> Paths_types.Reference.any)
          (r2 : Paths_types.Reference.module_ :> Paths_types.Reference.any)
        && resolved_equal (m1 : module_ :> any) (m2 : module_ :> any)
      | `ModuleType(r1, s1), `ModuleType(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Type(r1, s1), `Type(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Constructor(r1, s1), `Constructor(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : datatype :> any) (r2 : datatype :> any)
      | `Field(r1, s1), `Field(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : parent :> any) (r2 : parent :> any)
      | `Extension(r1, s1), `Extension(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Exception(r1, s1), `Exception(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Value(r1, s1), `Value(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Class(r1, s1), `Class(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `ClassType(r1, s1), `ClassType(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : signature :> any) (r2 : signature :> any)
      | `Method(r1, s1), `Method(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : class_signature :> any) (r2 : class_signature :> any)
      | `InstanceVariable(r1, s1), `InstanceVariable(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : class_signature :> any) (r2 : class_signature :> any)
      | `Label(r1, s1), `Label(r2, s2) ->
        s1 = s2 && resolved_equal (r1 : label_parent :> any) (r2 : label_parent :> any)
      | _, _ -> false

  and equal : Paths_types.Reference.any -> Paths_types.Reference.any -> bool = 
    let open Paths_types.Reference in
    fun r1 r2 ->
      match r1, r2 with
      | `Resolved r1, `Resolved r2 ->
        resolved_equal r1 r2
      | `Root (s1, k1), `Root (s2, k2) ->
        s1 = s2 && k1 = k2
      | `Dot(r1, s1), `Dot(r2, s2) ->
        s1 = s2 && equal (r1 : label_parent :> any) (r2 : label_parent :> any)
      | `Module(r1, s1), `Module(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `ModuleType(r1, s1), `ModuleType(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `Type(r1, s1), `Type(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `Constructor(r1, s1), `Constructor(r2, s2) ->
        s1 = s2 && equal (r1 : datatype :> any) (r2 : datatype :> any)
      | `Field(r1, s1), `Field(r2, s2) ->
        s1 = s2 && equal (r1 : parent :> any) (r2 : parent :> any)
      | `Extension(r1, s1), `Extension(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `Exception(r1, s1), `Exception(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `Class(r1, s1), `Class(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `ClassType(r1, s1), `ClassType(r2, s2) ->
        s1 = s2 && equal (r1 : signature :> any) (r2 : signature :> any)
      | `Method(r1, s1), `Method(r2, s2) ->
        s1 = s2 && equal (r1 : class_signature :> any) (r2 : class_signature :> any)
      | `InstanceVariable(r1, s1), `InstanceVariable(r2, s2) ->
        s1 = s2 && equal (r1 : class_signature :> any) (r2 : class_signature :> any)
      | `Label(r1, s1), `Label(r2, s2) ->
        s1 = s2 && equal (r1 : label_parent :> any) (r2 : label_parent :> any)
      | _, _ -> false

  module Resolved = struct
    open Paths_types.Resolved_reference

    let rec parent_signature_identifier : Paths_types.Resolved_reference.signature -> Identifier.Signature.t =
      function
      | `Identifier id -> id
      | `SubstAlias(sub, _) -> Path.Resolved.parent_module_identifier sub
      | `Module(m, n) -> `Module(parent_signature_identifier m, n)
      | `Canonical(_, `Resolved r) ->
        parent_signature_identifier (r : module_ :> signature)
      | `Canonical (r, _) -> parent_signature_identifier (r : module_ :> signature)
      | `ModuleType(m, s) -> `ModuleType(parent_signature_identifier m, s)

    let parent_type_identifier : datatype -> Identifier.DataType.t =
      function
      | `Identifier id -> id
      | `Type(sg, s) -> `Type(parent_signature_identifier sg, s)

    let parent_class_signature_identifier :
      class_signature -> Identifier.ClassSignature.t =
      function
      | `Identifier id -> id
      | `Class(sg, s) -> `Class(parent_signature_identifier sg, s)
      | `ClassType(sg, s) -> `ClassType(parent_signature_identifier sg, s)

    let rec parent_identifier : parent -> Identifier.Parent.t =
      function
      | `Identifier id -> id
      | `SubstAlias(sub, _) ->
        let id = Path.Resolved.parent_module_identifier sub in
        (id : Identifier.Signature.t :> Identifier.Parent.t)
      | `Module(m, n) -> `Module(parent_signature_identifier m, n)
      | `Canonical(_, `Resolved r) ->
        parent_identifier (r : module_ :> parent)
      | `Canonical (r, _) -> parent_identifier (r : module_ :> parent)
      | `ModuleType(m, s) -> `ModuleType(parent_signature_identifier m, s)
      | `Type(sg, s) -> `Type(parent_signature_identifier sg, s)
      | `Class(sg, s) -> `Class(parent_signature_identifier sg, s)
      | `ClassType(sg, s) -> `ClassType(parent_signature_identifier sg, s)

    let rec label_parent_identifier : label_parent -> Identifier.LabelParent.t =
      function
      | `Identifier id -> id
      | `SubstAlias(sub, _) ->
        let id = Path.Resolved.parent_module_identifier sub in
        (id : Identifier.Signature.t :> Identifier.LabelParent.t)
      | `Module(m, n) -> `Module(parent_signature_identifier m, n)
      | `Canonical(_, `Resolved r) ->
        label_parent_identifier (r : module_ :> label_parent)
      | `Canonical (r, _) -> label_parent_identifier (r : module_ :> label_parent)
      | `ModuleType(m, s) -> `ModuleType(parent_signature_identifier m, s)
      | `Type(sg, s) -> `Type(parent_signature_identifier sg, s)
      | `Class(sg, s) -> `Class(parent_signature_identifier sg, s)
      | `ClassType(sg, s) -> `ClassType(parent_signature_identifier sg, s)


    type mod_rebase_result =
      | MStop of Paths_types.Resolved_reference.module_
      | MContinue of Identifier.Module.t * Reversed.t

    type sig_rebase_result =
      | SStop of Paths_types.Resolved_reference.signature
      | SContinue of Identifier.Signature.t * Reversed.t

    let rec rebase_module_reference :
      Reversed.t -> Paths_types.Resolved_reference.module_ -> mod_rebase_result =
      fun new_base t ->
      match t with
      | `Identifier id ->
        let rev = Identifier.(to_reversed @@ signature_of_module id) in
        let new_base = Reversed.remove_prefix rev ~of_:new_base in
        MContinue (id, new_base)
      | `SubstAlias _ -> MStop t (* FIXME? *)
      | `Module (m, s) ->
        begin match rebase_signature_reference new_base m with
          | SStop m' -> if m == m' then MStop t else MStop (`Module (m', s))
          | SContinue (id, new_base) ->
            let id = `Module(id, s) in
            match new_base with
            | Reversed.Module s' :: rest when s = s' ->
              MContinue (id, rest)
            | _ ->
              MStop (`Identifier id)
        end
      | `Canonical (_, `Resolved p) ->
        (* We only care about printing at this point, so let's drop the lhs. *)
        rebase_module_reference new_base p
      | `Canonical (rp, p) ->
        begin match rebase_module_reference new_base (rp) with
          | MStop rp' -> MStop (`Canonical (rp', p))
          | _ ->
            (* We might come back at some point with a resolved rhs? So we don't want to
               drop it. *)
            MStop t
        end

    and rebase_signature_reference :
      Reversed.t -> Paths_types.Resolved_reference.signature -> sig_rebase_result =
      fun new_base t ->
      match t with
      | `Identifier id ->
        let rev = Identifier.(to_reversed id) in
        let new_base = Reversed.remove_prefix rev ~of_:new_base in
        SContinue (id, new_base)
      | `ModuleType (m, s) ->
        begin match rebase_signature_reference new_base m with
          | SStop m' -> if m == m' then SStop t else SStop (`ModuleType(m', s))
          | SContinue (id, new_base) ->
            let id = `ModuleType(id, s) in
            match new_base with
            | Reversed.ModuleType s' :: rest when s = s' ->
              SContinue (id, rest)
            | _ ->
              SStop (`Identifier id)
        end
      | `Module _ | `Canonical _ as x ->
        begin match rebase_module_reference new_base x with
          | MStop rp -> SStop (rp : module_ :> signature)
          | MContinue (id, rev) ->
            SContinue (Identifier.signature_of_module id, rev)
        end
      | `SubstAlias _ -> SStop t (* FIXME? *)

    let rebase_single_module = fun
      new_base t ->
      match t with
      | `Module (mp, s) ->
        begin match rebase_signature_reference new_base mp with
          | SContinue (id, _) ->
            `Identifier (`Module(id, s))
          | SStop mp' -> `Module (mp', s)
        end

    type module_id = [  
      | `Identifier of Identifier.Module.t
    ]

    let rebase_single_canonical : Reversed.t -> s_canonical -> [ s_canonical | module_id ] = fun
      new_base t ->
      match t with
      | `Canonical (p, `Resolved rp) ->
        begin match rebase_module_reference new_base rp with
          | MContinue (id, _) -> `Identifier id
          | MStop rp ->
            (* Easier to reexport a canonical than get the type for rp right... *)
            `Canonical (p, `Resolved rp)
        end
      | `Canonical (rp, p) ->
        begin match rebase_module_reference new_base rp with
          | MStop rp' -> `Canonical (rp', p)
          | _ ->
            (* We might come back at some point with a resolved rhs? So we don't want to
               drop it. *)
            (t :> [s_canonical | module_id ])
        end

    let rebase_single_module_type : Reversed.t -> s_module_type -> module_type = fun
      new_base t ->
      match t with
      | `ModuleType (mp, s) ->
        begin match rebase_signature_reference new_base mp with
          | SContinue (id, _) ->
            `Identifier (`ModuleType (id, s))
          | SStop mp' -> `ModuleType (mp', s)
        end

    let rebase_single_module : Reversed.t -> s_module -> [ s_module | `Identifier of Identifier.Module.t ] = fun
      new_base t ->
      match t with
      | `Module (x,y) -> (rebase_single_module new_base (`Module (x,y)))

    let rebase_single_class : Reversed.t -> s_class -> class_ = fun
      new_base t ->
      match t with
      | `Class (mp, s) ->
        begin match rebase_signature_reference new_base mp with
          | SContinue (id, _) -> `Identifier (`Class (id, s))
          | SStop mp' -> `Class (mp', s)
        end

    let rebase_single_value : Reversed.t -> s_value -> [s_value | `Identifier of Identifier.Value.t] = fun
      new_base t ->
      match t with
      | `Value (mp, s) ->
        begin match rebase_signature_reference new_base mp with
          | SContinue (id, _) -> `Identifier (`Value (id, s))
          | SStop mp' -> `Value (mp', s)
        end

    let rebase_single_class_type : Reversed.t -> s_class_type -> class_type = fun
      new_base t ->
      match t with
      | `ClassType (mp, s) ->
        begin match rebase_signature_reference new_base mp with
          | SContinue (id, _) -> `Identifier (`ClassType (id, s))
          | SStop mp' -> `ClassType (mp', s)
        end

    let rebase_single_type : Reversed.t -> s_type -> [s_type | `Identifier of Paths_types.Identifier.type_] = fun
      new_base t ->
      match t with
      | `Type (mp, s) ->
        begin match rebase_signature_reference new_base mp with
          | SContinue (id, _) -> `Identifier (`Type (id, s))
          | SStop mp' -> `Type (mp', s)
        end

    let rebase_single_extension : Reversed.t -> s_extension -> [s_extension | `Identifier of Paths_types.Identifier.reference_extension] = fun
      new_base t ->
      match t with 
      | `Extension (mp, s) ->
        begin match rebase_signature_reference new_base mp with
        | SContinue (id, _) ->
          `Identifier (`Extension (id, s))
        | SStop mp' -> `Extension (mp', s)
        end

    let rebase_single_exception : Reversed.t -> s_exception -> exception_ = fun
      new_base t ->
      match t with
      | `Exception (mp, s) ->
        begin match rebase_signature_reference new_base mp with
        | SContinue (id, _) ->
          `Identifier (`Exception (id, s))
        | SStop mp' -> `Exception (mp', s)
        end


    module Signature =
    struct
      type t = Paths_types.Resolved_reference.signature

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let rec identifier : t -> Identifier.Signature.t = function
        | `Identifier id -> id
        | `SubstAlias(_, p) -> identifier (p : module_ :> signature)
        | `Module(s, n) -> `Module(parent_signature_identifier s, n)
        | `Canonical(_, `Resolved p) -> identifier (p : module_ :> signature)
        | `Canonical(p, _) -> identifier (p : module_ :> signature)
        | `ModuleType(s, n) -> `ModuleType(parent_signature_identifier s, n)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
        match t with
        | `Identifier _ -> t
        | `SubstAlias _ -> t (* TODO: rewrite necessary? *)
        | `Canonical (x, y) -> (rebase_single_canonical new_base (`Canonical (x,y)) :> t)
        | `Module (x,y) -> (rebase_single_module new_base (`Module (x,y)) :> t)
        | `ModuleType (x,y) -> (rebase_single_module_type new_base (`ModuleType (x,y)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

    module ClassSignature =
    struct
      type t = Paths_types.Resolved_reference.class_signature

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Identifier.ClassSignature.t = function
        | `Identifier id -> id
        | `Class(s, n) -> `Class(parent_signature_identifier s, n)
        | `ClassType(s, n) -> `ClassType(parent_signature_identifier s, n)

      let rebase' : Reversed.t -> t -> t =
        fun new_base t ->
        match t with
        | `Identifier _ -> t
        | `Class (mp, s) -> (rebase_single_class new_base (`Class (mp, s)) :> t)
        | `ClassType (x, y) -> (rebase_single_class_type new_base (`ClassType (x, y)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase' rev t

    end


    module DataType =
    struct
      type t = Paths_types.Resolved_reference.datatype

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Identifier.DataType.t = function 
        | `Identifier id -> id
        | `Type(s, n) -> `Type(parent_signature_identifier s, n)

      let rebase' : Reversed.t -> t -> t =
        fun new_base t ->
        match t with
        | `Identifier _ -> t
        | `Type (s, n) -> (rebase_single_type new_base (`Type (s, n)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase' rev t
    end

    let rebase_single_constructor : Reversed.t -> s_constructor -> s_constructor = fun
      new_base t ->
      match t with
      | `Constructor (parent, s) ->
          `Constructor(DataType.rebase' new_base parent, s)

    module Parent =
    struct
      type t = Paths_types.Resolved_reference.parent

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let rec identifier : t -> Identifier.Parent.t = function
        | `Identifier id -> id
        | `SubstAlias(_, p) -> identifier (p : module_ :> t)
        | `Module(s, n) -> `Module(parent_signature_identifier s, n)
        | `Canonical(_, `Resolved p) -> identifier (p : module_ :> t)
        | `Canonical(p, _) -> identifier (p : module_ :> t)
        | `ModuleType(s, n) -> `ModuleType(parent_signature_identifier s, n)
        | `Class(s, n) -> `Class(parent_signature_identifier s, n)
        | `ClassType(s, n) -> `ClassType(parent_signature_identifier s, n)
        | `Type(s, n) -> `Type(parent_signature_identifier s, n)

      let rebase' : Reversed.t -> t -> t =
        fun new_base t ->
        match t with
        | `Identifier _ -> t
        | `SubstAlias _ -> t (* TODO: rewrite necessary? *)
        | `Canonical (x, y) -> (rebase_single_canonical new_base (`Canonical (x,y)) :> t)
        | `Module (x,y) -> (rebase_single_module new_base (`Module (x,y)) :> t)
        | `ModuleType (x,y) -> (rebase_single_module_type new_base (`ModuleType (x,y)) :> t)
        | `Type (mp, s) -> (rebase_single_type new_base (`Type (mp, s)) :> t)
        | `Class (mp, s) -> (rebase_single_class new_base (`Class (mp, s)) :> t)
        | `ClassType (x, y) -> (rebase_single_class_type new_base (`ClassType (x, y)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase' rev t
    end

    let rebase_single_field : Reversed.t -> s_field -> s_field = fun
      new_base t ->
      match t with
      | `Field (parent, s) ->
          `Field(Parent.rebase' new_base parent, s)

    module LabelParent =
    struct
      type t = Paths_types.Resolved_reference.label_parent

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let rec identifier : t -> Identifier.LabelParent.t = function
        | `Identifier id -> id
        | `SubstAlias(_, p) -> identifier (p : module_ :> t)
        | `Module(s, n) -> `Module(parent_signature_identifier s, n)
        | `Canonical(_, `Resolved p) -> identifier (p : module_ :> t)
        | `Canonical(p, _) -> identifier (p : module_ :> t)
        | `ModuleType(s, n) -> `ModuleType(parent_signature_identifier s, n)
        | `Class(s, n) -> `Class(parent_signature_identifier s, n)
        | `ClassType(s, n) -> `ClassType(parent_signature_identifier s, n)
        | `Type(s, n) -> `Type(parent_signature_identifier s, n)

      let rebase' : Reversed.t -> t -> t =
        fun new_base t ->
        match t with
        | `Identifier _ -> t
        | `SubstAlias _ -> t (* TODO: rewrite necessary? *)
        | `Canonical (x, y) -> (rebase_single_canonical new_base (`Canonical (x,y)) :> t)
        | `Module (x,y) -> (rebase_single_module new_base (`Module (x,y)) :> t)
        | `ModuleType (x,y) -> (rebase_single_module_type new_base (`ModuleType (x,y)) :> t)
        | `Type (mp, s) -> (rebase_single_type new_base (`Type (mp, s)) :> t)
        | `Class (mp, s) -> (rebase_single_class new_base (`Class (mp, s)) :> t)
        | `ClassType (x, y) -> (rebase_single_class_type new_base (`ClassType (x, y)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase' rev t
    end

    module Module =
    struct
      type t = Paths_types.Resolved_reference.module_

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let rec identifier : t -> Identifier.Module.t = function
        | `Identifier id -> id
        | `SubstAlias(_, p) -> identifier (p : module_ :> t)
        | `Module(s, n) -> `Module(parent_signature_identifier s, n)
        | `Canonical(_, `Resolved p) -> identifier (p : module_ :> t)
        | `Canonical(p, _) -> identifier (p : module_ :> t)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
        match t with
        | `Identifier _ -> t
        | `SubstAlias _ -> t (* TODO: rewrite necessary? *)
        | `Canonical (x, y) -> (rebase_single_canonical new_base (`Canonical (x,y)) :> t)
        | `Module (x,y) -> (rebase_single_module new_base (`Module (x,y)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

  module ModuleType =
    struct
      type t = Paths_types.Resolved_reference.module_type

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Identifier.ModuleType.t = function
        | `Identifier id -> id
        | `ModuleType(s, n) -> `ModuleType(parent_signature_identifier s, n)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
        match t with
        | `Identifier _ -> t
        | `ModuleType (x,y) -> (rebase_single_module_type new_base (`ModuleType (x,y)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

    module Type =
    struct
      type t = Paths_types.Resolved_reference.type_

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Identifier.Path.Type.t = function
        | `Identifier id -> id
        | `Type(s, n) -> `Type(parent_signature_identifier s, n)
        | `Class(s,n) -> `Class(parent_signature_identifier s, n)
        | `ClassType(s,n) -> `ClassType(parent_signature_identifier s, n)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
        match t with
        | `Identifier _ -> t
        | `Type (mp, s) -> (rebase_single_type new_base (`Type (mp, s)) :> t)
        | `Class (s, n) -> (rebase_single_class new_base (`Class (s, n)) :> t)
        | `ClassType (s, n) -> (rebase_single_class_type new_base (`ClassType (s, n)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

    module Constructor =
    struct
      type t = Paths_types.Resolved_reference.constructor

      let equal t1 t2 =
        resolved_equal (t1 : t :> any) (t2 : t :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_constructor = function
        | `Identifier id -> id
        | `Constructor(s, n) -> `Constructor(parent_type_identifier s, n)
        | `Extension(s, n) -> `Extension(parent_signature_identifier s, n)
        | `Exception(s, n) -> `Exception(parent_signature_identifier s, n)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
        match t with
        | `Identifier _ -> t
        | `Constructor (p,q) -> (rebase_single_constructor new_base (`Constructor (p,q)) :> t)
        | `Extension (p,q) -> (rebase_single_extension new_base (`Extension (p,q)) :> t)
        | `Exception (p,q) -> (rebase_single_exception new_base (`Exception (p,q)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

    module Field =
    struct
      type t = Paths_types.Resolved_reference.field

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Identifier.Field.t = function
        | `Identifier id -> id
        | `Field(p, n) -> `Field(parent_identifier p, n)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
          match t with
          | `Identifier _ -> t
          | `Field (p,q) -> (rebase_single_field new_base (`Field (p,q)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

    module Extension =
    struct
      type t = Paths_types.Resolved_reference.extension

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_extension  = function
        | `Identifier id -> id
        | `Extension(p,q) -> `Extension(parent_signature_identifier p, q)
        | `Exception(p,q) -> `Exception(parent_signature_identifier p, q)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
          match t with
          | `Identifier _ -> t
          | `Extension(p,q) -> (rebase_single_extension new_base (`Extension (p,q)) :> t)
          | `Exception(p,q) -> (rebase_single_exception new_base (`Exception (p,q)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

    module Exception =
    struct
      type t = Paths_types.Resolved_reference.exception_

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_exception  = function
        | `Identifier id -> id
        | `Exception(p,q) -> `Exception(parent_signature_identifier p, q)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
          match t with
          | `Identifier _ -> t
          | `Exception(p,q) -> (rebase_single_exception new_base (`Exception (p,q)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

    module Value =
    struct
      type t = Paths_types.Resolved_reference.value

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_value  = function
        | `Identifier id -> id
        | `Value(p,q) -> `Value(parent_signature_identifier p, q)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
          match t with
          | `Identifier _ -> t
          | `Value(p,q) -> (rebase_single_value new_base (`Value (p,q)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

    module Class =
    struct
      type t = Paths_types.Resolved_reference.class_

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_class  = function
        | `Identifier id -> id
        | `Class(p,q) -> `Class(parent_signature_identifier p, q)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
          match t with
          | `Identifier _ -> t
          | `Class(p,q) -> (rebase_single_class new_base (`Class (p,q)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

    module ClassType =
    struct
      type t = Paths_types.Resolved_reference.class_type

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_class_type  = function
        | `Identifier id -> id
        | `Class(p,q) -> `Class(parent_signature_identifier p, q)
        | `ClassType(p,q) -> `ClassType(parent_signature_identifier p, q)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
          match t with
          | `Identifier _ -> t
          | `Class(p,q) -> (rebase_single_class new_base (`Class (p,q)) :> t)
          | `ClassType(p,q) -> (rebase_single_class_type new_base (`ClassType(p,q)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

    let rebase_single_method : Reversed.t -> s_method -> s_method = fun
      new_base t ->
      match t with
      | `Method (parent, s) ->
          `Method(ClassSignature.rebase' new_base parent, s)

    let rebase_single_instance_variable : Reversed.t -> s_instance_variable -> s_instance_variable = fun
      new_base t ->
      match t with
      | `InstanceVariable (parent, s) ->
          `InstanceVariable(ClassSignature.rebase' new_base parent, s)

    module Method =
    struct
      type t = Paths_types.Resolved_reference.method_

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_method  = function
        | `Identifier id -> id
        | `Method(p,q) -> `Method(parent_class_signature_identifier p, q)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
          match t with
          | `Identifier _ -> t
          | `Method(p,q) -> (rebase_single_method new_base (`Method (p,q)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

    module InstanceVariable =
    struct
      type t = Paths_types.Resolved_reference.instance_variable

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_instance_variable  = function
        | `Identifier id -> id
        | `InstanceVariable(p,q) -> `InstanceVariable(parent_class_signature_identifier p, q)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
          match t with
          | `Identifier _ -> t
          | `InstanceVariable(p,q) -> (rebase_single_instance_variable new_base (`InstanceVariable(p,q)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

    let rebase_single_label : Reversed.t -> s_label -> s_label = fun
      new_base t ->
      match t with
      | `Label (parent, s) ->
          `Label(LabelParent.rebase' new_base parent, s)

    module Label =
    struct
      type t = Paths_types.Resolved_reference.label

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_label  = function
        | `Identifier id -> id
        | `Label(p,q) -> `Label(label_parent_identifier p, q)

      let rebase : Reversed.t -> t -> t =
        fun new_base t ->
          match t with
          | `Identifier _ -> t
          | `Label(p,q) -> (rebase_single_label new_base (`Label(p,q)) :> t)

      let rebase id t =
        let rev = Identifier.to_reversed id in
        rebase rev t
    end

    module Page =
    struct
      type t = Paths_types.Resolved_reference.page

      let equal t1 t2 =
        resolved_equal (t1 :> any) (t2 :> any)

      let hash t = hash_resolved (t : t :> any)

      let identifier : t -> Paths_types.Identifier.reference_page  = function
        | `Identifier id -> id

      let rebase _id t = t
    end

    type t = Paths_types.Resolved_reference.any

    let rec identifier : t -> Identifier.t = function
      | `Identifier id -> id
      | `SubstAlias(_, p) -> identifier (p :> t)
      | `Module(s, n) -> `Module(parent_signature_identifier s, n)
      | `Canonical(_, `Resolved p) -> identifier (p :> t)
      | `Canonical(p, _) -> identifier (p :> t)
      | `ModuleType(s, n) -> `ModuleType(parent_signature_identifier s, n)
      | `Field(p, n) -> `Field(parent_identifier p, n)
      | `Type(s, n) -> `Type(parent_signature_identifier s, n)
      | `Constructor(s, n) -> `Constructor(parent_type_identifier s, n)
      | `Extension(p,q) -> `Extension(parent_signature_identifier p, q)
      | `Exception(p,q) -> `Exception(parent_signature_identifier p, q)
      | `Value(p,q) -> `Value(parent_signature_identifier p, q)
      | `Class(p,q) -> `Class(parent_signature_identifier p, q)
      | `ClassType(p,q) -> `ClassType(parent_signature_identifier p, q)
      | `Method(p,q) -> `Method(parent_class_signature_identifier p, q)
      | `InstanceVariable(p,q) -> `InstanceVariable(parent_class_signature_identifier p, q)
      | `Label(p,q) -> `Label(label_parent_identifier p, q)

    let module_of_t : t -> Module.t = function
      | `Identifier (#Identifier.Module.t)
      | #Paths_types.Resolved_reference.module_no_id as x -> x
      | _ -> assert false

    let module_type_of_t : t -> ModuleType.t = function
      | `Identifier (#Identifier.ModuleType.t)
      | #Paths_types.Resolved_reference.s_module_type as x -> x
      | _ -> assert false

    let signature_of_t : t -> Signature.t = function
      | `Identifier (#Identifier.Signature.t)
      | #Paths_types.Resolved_reference.signature_no_id as x -> x
      | _ -> assert false

    let class_signature_of_t : t -> ClassSignature.t = function
      | `Identifier (#Identifier.ClassSignature.t)
      | #Paths_types.Resolved_reference.class_signature_no_id as x -> x
      | _ -> assert false

    let parent_of_t : t -> Parent.t = function
      | `Identifier (#Identifier.Parent.t)
      | #Paths_types.Resolved_reference.parent_no_id as x -> x
      | _ -> assert false

    let label_parent_of_t : t -> LabelParent.t = function
      | `Identifier (#Identifier.LabelParent.t)
      | #Paths_types.Resolved_reference.parent_no_id as x -> x
      | _ -> assert false

    let type_of_t : t -> Type.t = function
      | `Identifier (#Identifier.Type.t)
      | #Paths_types.Resolved_reference.s_type as x -> x
      | _ -> assert false

    let datatype_of_t : t -> DataType.t = function
      | `Identifier (#Identifier.DataType.t)
      | #Paths_types.Resolved_reference.s_type as x -> x
      | _ -> assert false

    let constructor_of_t : t -> Constructor.t = function
      | `Identifier (#Identifier.Constructor.t)
      | #Paths_types.Resolved_reference.s_constructor
      | #Paths_types.Resolved_reference.s_extension
      | #Paths_types.Resolved_reference.s_exception as x -> x
      | _ -> assert false

    let field_of_t : t -> Field.t = function
      | `Identifier (#Identifier.Field.t)
      | #Paths_types.Resolved_reference.s_field as x -> x
      | _ -> assert false

    let extension_of_t : t -> Extension.t = function
      | `Identifier (#Paths_types.Identifier.reference_extension)
      | #Paths_types.Resolved_reference.s_extension
      | #Paths_types.Resolved_reference.s_exception as x -> x
      | _ -> assert false

    let exception_of_t : t -> Exception.t = function
      | `Identifier (#Paths_types.Identifier.reference_exception)
      | #Paths_types.Resolved_reference.s_exception as x -> x
      | _ -> assert false

    let value_of_t : t -> Value.t = function
      | `Identifier (#Paths_types.Identifier.reference_value)
      | #Paths_types.Resolved_reference.s_value as x -> x
      | _ -> assert false

    let class_of_t : t -> Class.t = function
      | `Identifier (#Paths_types.Identifier.reference_class)
      | #Paths_types.Resolved_reference.s_class as x -> x
      | _ -> assert false

    let class_type_of_t : t -> ClassType.t = function
      | `Identifier (#Paths_types.Identifier.reference_class_type)
      | #Paths_types.Resolved_reference.s_class_type as x -> x
      | _ -> assert false

    let method_of_t : t -> Method.t = function
      | `Identifier (#Paths_types.Identifier.reference_method)
      | #Paths_types.Resolved_reference.s_method as x -> x
      | _ -> assert false

    let instance_variable_of_t : t -> InstanceVariable.t = function
      | `Identifier (#Paths_types.Identifier.reference_instance_variable)
      | #Paths_types.Resolved_reference.s_instance_variable as x -> x
      | _ -> assert false

    let label_of_t : t -> Label.t = function
      | `Identifier (#Paths_types.Identifier.reference_label)
      | #Paths_types.Resolved_reference.s_label as x -> x
      | _ -> assert false

  end
  type t = Paths_types.Reference.any

  let equal : t -> t -> bool = equal

  let hash p = hash_reference p

  module Signature =
  struct
    type t = Paths_types.Reference.signature
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module ClassSignature =
  struct
    type t = Paths_types.Reference.class_signature
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module DataType =
  struct
    type t = Paths_types.Reference.datatype
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Parent =
  struct
    type t = Paths_types.Reference.parent
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module LabelParent =
  struct
    type t = Paths_types.Reference.label_parent
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Module =
  struct
    type t = Paths_types.Reference.module_
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module ModuleType =
  struct
    type t = Paths_types.Reference.module_type
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Type =
  struct
    type t = Paths_types.Reference.type_
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Constructor =
  struct
    type t = Paths_types.Reference.constructor
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Field =
  struct
    type t = Paths_types.Reference.field
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Extension =
  struct
    type t = Paths_types.Reference.extension
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Exception =
  struct
    type t = Paths_types.Reference.exception_
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Value =
  struct
    type t = Paths_types.Reference.value
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Class =
  struct
    type t = Paths_types.Reference.class_
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module ClassType =
  struct
    type t = Paths_types.Reference.class_type
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Method =
  struct
    type t = Paths_types.Reference.method_
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module InstanceVariable =
  struct
    type t = Paths_types.Reference.instance_variable
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Label =
  struct
    type t = Paths_types.Reference.label
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  module Page =
  struct
    type t = Paths_types.Reference.page
    let equal : t -> t -> bool = fun t1 t2 -> equal (t1 :> Paths_types.Reference.any) (t2 :> Paths_types.Reference.any)
    let hash : t -> int = fun t -> hash (t :> Paths_types.Reference.any)
  end

  let module_of_t : t -> Module.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.module_))
  | `Resolved (#Paths_types.Resolved_reference.module_no_id)
  | `Root (_,#Paths_types.Reference.tag_module)
  | `Dot (_,_)
  | `Module (_, _) as x -> x
  | _ -> assert false

let module_type_of_t : t -> ModuleType.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.module_type))
  | `Resolved (#Paths_types.Resolved_reference.s_module_type)
  | `Root (_,#Paths_types.Reference.tag_module_type)
  | `Dot (_,_)
  | `ModuleType (_, _) as x -> x
  | _ -> assert false

let signature_of_t : t -> Signature.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.signature))
  | `Resolved (#Paths_types.Resolved_reference.signature_no_id)
  | `Root (_,#Paths_types.Reference.tag_signature)
  | `Dot (_,_)
  | `ModuleType (_,_)
  | `Module (_, _) as x -> x
  | _ -> assert false

let class_signature_of_t : t -> ClassSignature.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.class_signature))
  | `Resolved (#Paths_types.Resolved_reference.class_signature_no_id)
  | `Root (_,#Paths_types.Reference.tag_class_signature)
  | `Dot (_,_)
  | `ClassType (_,_)
  | `Class (_, _) as x -> x
  | _ -> assert false

let parent_of_t : t -> Parent.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.parent))
  | `Resolved (#Paths_types.Resolved_reference.parent_no_id)
  | `Root (_,#Paths_types.Reference.tag_parent)
  | `Dot (_,_)
  | `ClassType (_,_)
  | `ModuleType (_,_)
  | `Module (_, _)
  | `Type (_,_)
  | `Class (_, _) as x -> x
  | _ -> assert false

let label_parent_of_t : t -> LabelParent.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.label_parent))
  | `Resolved (#Paths_types.Resolved_reference.parent_no_id) (* Nb, parent_no_id would be equal to label_parent_no_id if it existed! *)
  | `Root (_,#Paths_types.Reference.tag_label_parent)
  | `Dot (_,_)
  | `ClassType (_,_)
  | `ModuleType (_,_)
  | `Module (_, _)
  | `Type (_,_)
  | `Class (_, _) as x -> x
  | _ -> assert false

let type_of_t : t -> Type.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.type_))
  | `Resolved (#Paths_types.Resolved_reference.s_type)
  | `Root (_,#Paths_types.Reference.tag_type)
  | `Dot (_,_)
  | `Type (_,_) as x -> x
  | _ -> assert false

let datatype_of_t : t -> DataType.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.datatype))
  | `Resolved (#Paths_types.Resolved_reference.s_type)
  | `Root (_,#Paths_types.Reference.tag_datatype)
  | `Dot (_,_)
  | `Type (_,_) as x -> x
  | _ -> assert false

let constructor_of_t : t -> Constructor.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.reference_constructor))
  | `Resolved (#Paths_types.Resolved_reference.constructor_no_id)
  | `Root (_,#Paths_types.Reference.tag_constructor)
  | `Dot (_,_)
  | `Constructor (_,_)
  | `Extension (_,_)
  | `Exception (_,_) as x -> x
  | _ -> assert false

let field_of_t : t -> Field.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.reference_field))
  | `Resolved (#Paths_types.Resolved_reference.s_field)
  | `Root (_,#Paths_types.Reference.tag_field)
  | `Dot (_,_)
  | `Field (_,_) as x -> x
  | _ -> assert false

let extension_of_t : t -> Extension.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.reference_extension))
  | `Resolved (#Paths_types.Resolved_reference.extension_no_id)
  | `Root (_,#Paths_types.Reference.tag_extension)
  | `Dot (_,_)
  | `Extension (_,_)
  | `Exception (_,_) as x -> x
  | _ -> assert false

let exception_of_t : t -> Exception.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.reference_exception))
  | `Resolved (#Paths_types.Resolved_reference.s_exception)
  | `Root (_,#Paths_types.Reference.tag_exception)
  | `Dot (_,_)
  | `Exception (_,_) as x -> x
  | _ -> assert false

let value_of_t : t -> Value.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.reference_value))
  | `Resolved (#Paths_types.Resolved_reference.s_value)
  | `Root (_,#Paths_types.Reference.tag_value)
  | `Dot (_,_)
  | `Value (_,_) as x -> x
  | _ -> assert false

let class_of_t : t -> Class.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.reference_class))
  | `Resolved (#Paths_types.Resolved_reference.s_class)
  | `Root (_,#Paths_types.Reference.tag_class)
  | `Dot (_,_)
  | `Class (_,_) as x -> x
  | _ -> assert false

let class_type_of_t : t -> ClassType.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.reference_class_type))
  | `Resolved (#Paths_types.Resolved_reference.class_type_no_id)
  | `Root (_,#Paths_types.Reference.tag_class_type)
  | `Dot (_,_)
  | `Class (_,_) 
  | `ClassType (_,_) as x -> x 
  | _ -> assert false

let method_of_t : t -> Method.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.reference_method))
  | `Resolved (#Paths_types.Resolved_reference.s_method)
  | `Root (_,#Paths_types.Reference.tag_method)
  | `Dot (_,_)
  | `Method (_,_) as x -> x 
  | _ -> assert false

let instance_variable_of_t : t -> InstanceVariable.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.reference_instance_variable))
  | `Resolved (#Paths_types.Resolved_reference.s_instance_variable)
  | `Root (_,#Paths_types.Reference.tag_instance_variable)
  | `Dot (_,_)
  | `InstanceVariable (_,_) as x -> x 
  | _ -> assert false

let label_of_t : t -> Label.t = function
  | `Resolved (`Identifier (#Paths_types.Identifier.reference_label))
  | `Resolved (#Paths_types.Resolved_reference.s_label)
  | `Root (_,#Paths_types.Reference.tag_label)
  | `Dot (_,_)
  | `Label (_,_) as x -> x 
  | _ -> assert false
end
