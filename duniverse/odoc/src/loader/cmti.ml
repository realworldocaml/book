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

open Asttypes
open Typedtree

module OCamlPath = Path

open Odoc_model.Paths
open Odoc_model.Lang
open Odoc_model.Names

module Env = Odoc_model.Ident_env
module Paths = Odoc_model.Paths
module Ident_env = Odoc_model.Ident_env

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let parenthesise name =
  match name with
  | "asr" | "land" | "lnot" | "lor" | "lsl" | "lsr"
  | "lxor" | "mod" -> "(" ^ name ^ ")"
  | _ ->
    if (String.length name > 0) then
      match name.[0] with
      | 'a' .. 'z' | '\223' .. '\246' | '\248' .. '\255' | '_'
      | 'A' .. 'Z' | '\192' .. '\214' | '\216' .. '\222' -> name
      | _ -> "(" ^ name ^ ")"
    else name

let read_label = Cmi.read_label

let rec read_core_type env container ctyp =
  let open TypeExpr in
    match ctyp.ctyp_desc with
    | Ttyp_any -> Any
    | Ttyp_var s -> Var s
    | Ttyp_arrow(lbl, arg, res) ->
        let lbl = read_label lbl in
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
        (* NOTE(@ostera): Unbox the optional value for this optional labelled
           argument since the 4.02.x representation includes it explicitly. *)
        let arg = match lbl with
          | None | Some(Label(_)) -> read_core_type env container arg
          | Some(Optional(_)) ->
              let arg' = match arg.ctyp_desc with
                | Ttyp_constr(_, _, param :: _) -> param
                | _ -> arg
              in
              read_core_type env container arg'
#else
        let arg = read_core_type env container arg
#endif
        in
        let res = read_core_type env container res in
          Arrow(lbl, arg, res)
    | Ttyp_tuple typs ->
        let typs = List.map (read_core_type env container) typs in
          Tuple typs
    | Ttyp_constr(p, _, params) ->
        let p = Env.Path.read_type env p in
        let params = List.map (read_core_type env container) params in
          Constr(p, params)
    | Ttyp_object(methods, closed) ->
        let open TypeExpr.Object in
        let fields =
          List.map
#if OCAML_MAJOR = 4 && OCAML_MINOR < 06
            (fun (name, _, typ) ->
              Method {name; type_ = read_core_type env container typ})
#elif OCAML_MAJOR = 4 && OCAML_MINOR < 08
            (function
              | OTtag (name, _, typ) ->
                Method {
                  name = name.txt;
                  type_ = read_core_type env container typ;
                }
              | OTinherit typ -> Inherit (read_core_type env container typ))
#else
            (function
              | {of_desc=OTtag (name, typ); _} ->
                Method {
                  name = name.txt;
                  type_ = read_core_type env container typ;
                }
              | {of_desc=OTinherit typ; _} -> Inherit (read_core_type env container typ))
#endif
            methods
        in
          Object {fields; open_ = (closed = Asttypes.Open)}
    | Ttyp_class(p, _, params) ->
        let p = Env.Path.read_class_type env p in
        let params = List.map (read_core_type env container) params in
          Class(p, params)
    | Ttyp_alias(typ, var) ->
        let typ = read_core_type env container typ in
          Alias(typ, var)
    | Ttyp_variant(fields, closed, present) ->
        let open TypeExpr.Polymorphic_variant in
        let elements =
          fields |> List.map begin fun field ->
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
            match field.rf_desc with
              | Ttag(name, constant, arguments) ->
                let attributes = field.rf_attributes in
#else
            match field with
              | Ttag(name, attributes, constant, arguments) ->
#endif
                let arguments =
                  List.map (read_core_type env container) arguments in
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 06
                  let name = name.txt in
#endif
                let doc = Doc_attr.attached container attributes in
                Constructor {name; constant; arguments; doc}
              | Tinherit typ -> Type (read_core_type env container typ)
          end
        in
        let kind =
          if closed = Asttypes.Open then Open
          else match present with
            | None -> Fixed
            | Some names -> Closed names
        in
          Polymorphic_variant {kind; elements}
    | Ttyp_poly([], typ) -> read_core_type env container typ
    | Ttyp_poly(vars, typ) -> Poly(vars, read_core_type env container typ)
    | Ttyp_package {pack_path; pack_fields; _} ->
        let open TypeExpr.Package in
        let path = Env.Path.read_module_type env pack_path in
        let substitutions =
          List.map
            (fun (frag, typ) ->
               let frag = Env.Fragment.read_type frag.Location.txt in
               let typ = read_core_type env container typ in
               (frag, typ))
            pack_fields
        in
          Package {path; substitutions}

let read_value_description env parent vd =
  let open Signature in
  let open Odoc_model.Names in
  let name = parenthesise (Ident.name vd.val_id) in
  let id = `Value(parent, ValueName.of_string name) in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container vd.val_attributes in
  let type_ = read_core_type env container vd.val_desc in
  match vd.val_prim with
  | [] -> Value {Value.id; doc; type_}
  | primitives -> External {External.id; doc; type_; primitives}

let read_type_parameter (ctyp, var) =
  let open TypeDecl in
  let desc =
    match ctyp.ctyp_desc with
    | Ttyp_any -> Any
    | Ttyp_var s -> Var s
    | _ -> assert false
  in
  let var =
    match var with
    | Covariant -> Some Pos
    | Contravariant -> Some Neg
    | Invariant -> None
  in
    (desc, var)

let read_label_declaration env parent label_parent ld =
  let open TypeDecl.Field in
  let open Odoc_model.Names in
  let name = parenthesise (Ident.name ld.ld_id) in
  let id = `Field(parent, FieldName.of_string name) in
  let doc = Doc_attr.attached label_parent ld.ld_attributes in
  let mutable_ = (ld.ld_mutable = Mutable) in
  let type_ = read_core_type env label_parent ld.ld_type in
    {id; doc; mutable_; type_}

let read_constructor_declaration_arguments env parent label_parent arg =
  let open TypeDecl.Constructor in
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
  ignore parent;
  Tuple (List.map (read_core_type env label_parent) arg)
#else
  match arg with
  | Cstr_tuple args -> Tuple (List.map (read_core_type env label_parent) args)
  | Cstr_record lds ->
      Record (List.map (read_label_declaration env parent label_parent) lds)
#endif

let read_constructor_declaration env parent cd =
  let open TypeDecl.Constructor in
  let open Odoc_model.Names in
  let name = parenthesise (Ident.name cd.cd_id) in
  let id = `Constructor(parent, ConstructorName.of_string name) in
  let container = (parent : Identifier.DataType.t :> Identifier.Parent.t) in
  let label_container = (container :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached label_container cd.cd_attributes in
  let args =
    read_constructor_declaration_arguments
      env container label_container cd.cd_args
  in
  let res = opt_map (read_core_type env label_container) cd.cd_res in
    {id; doc; args; res}

let read_type_kind env parent =
  let open TypeDecl.Representation in function
    | Ttype_abstract -> None
    | Ttype_variant cstrs ->
        let cstrs = List.map (read_constructor_declaration env parent) cstrs in
          Some (Variant cstrs)
    | Ttype_record lbls ->
        let parent = (parent : Identifier.DataType.t :> Identifier.Parent.t) in
      let label_parent = (parent :> Identifier.LabelParent.t) in
      let lbls =
        List.map (read_label_declaration env parent label_parent) lbls in
          Some (Record lbls)
    | Ttype_open -> Some Extensible

let read_type_equation env container decl =
  let open TypeDecl.Equation in
  let params = List.map read_type_parameter decl.typ_params in
  let private_ = (decl.typ_private = Private) in
  let manifest = opt_map (read_core_type env container) decl.typ_manifest in
  let constraints =
    List.map
      (fun (typ1, typ2, _) ->
         (read_core_type env container typ1,
          read_core_type env container typ2))
      decl.typ_cstrs
  in
    {params; private_; manifest; constraints}

let read_type_declaration env parent decl =
  let open TypeDecl in
  let open Odoc_model.Names in
  let name = parenthesise (Ident.name decl.typ_id) in
  let id = `Type(parent, TypeName.of_string name) in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container decl.typ_attributes in
  let equation = read_type_equation env container decl in
  let representation = read_type_kind env id decl.typ_kind in
    {id; doc; equation; representation}

let read_type_declarations env parent rec_flag decls =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let items =
    let open Signature in
    List.fold_left
      (fun (acc, recursive) decl ->
        let comments =
          Doc_attr.standalone_multiple container decl.typ_attributes in
         let comments = List.map (fun com -> Comment com) comments in
         let decl = read_type_declaration env parent decl in
         ((Type (recursive, decl)) :: (List.rev_append comments acc), And))
      ([], rec_flag) decls
    |> fst
  in
    List.rev items

#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
let read_type_substitutions env parent decls =
  List.map (fun decl -> Odoc_model.Lang.Signature.TypeSubstitution (read_type_declaration env parent decl)) decls
#endif

let read_extension_constructor env parent ext =
  let open Extension.Constructor in
  let open Odoc_model.Names in
  let name = parenthesise (Ident.name ext.ext_id) in
  let id = `Extension(parent, ExtensionName.of_string name) in
  let container = (parent : Identifier.Signature.t :> Identifier.Parent.t) in
  let label_container = (container :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached label_container ext.ext_attributes in
  match ext.ext_kind with
  | Text_rebind _ -> assert false
  | Text_decl(args, res) ->
    let args =
      read_constructor_declaration_arguments
        env container label_container args
    in
    let res = opt_map (read_core_type env label_container) res in
        {id; doc; args; res}

let read_type_extension env parent tyext =
  let open Extension in
  let type_path = Env.Path.read_type env tyext.tyext_path in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container tyext.tyext_attributes in
  let type_params = List.map read_type_parameter tyext.tyext_params in
  let private_ = (tyext.tyext_private = Private) in
  let constructors =
    List.map (read_extension_constructor env parent) tyext.tyext_constructors
  in
    { type_path; doc; type_params; private_; constructors; }

let read_exception env parent (ext : extension_constructor) =
  let open Exception in
  let open Odoc_model.Names in
  let name = parenthesise (Ident.name ext.ext_id) in
  let id = `Exception(parent, ExceptionName.of_string name) in
  let container = (parent : Identifier.Signature.t :> Identifier.Parent.t) in
  let label_container = (container :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached label_container ext.ext_attributes in
  match ext.ext_kind with
  | Text_rebind _ -> assert false
  | Text_decl(args, res) ->
    let args =
      read_constructor_declaration_arguments
        env container label_container args
    in
    let res = opt_map (read_core_type env label_container) res in
        {id; doc; args; res}

let rec read_class_type_field env parent ctf =
  let open ClassSignature in
  let open Odoc_model.Names in

  let container = (parent : Identifier.ClassSignature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container ctf.ctf_attributes in
  match ctf.ctf_desc with
  | Tctf_val(name, mutable_, virtual_, typ) ->
      let open InstanceVariable in
      let name = parenthesise name in
      let id = `InstanceVariable(parent, InstanceVariableName.of_string name) in
      let mutable_ = (mutable_ = Mutable) in
      let virtual_ = (virtual_ = Virtual) in
    let type_ = read_core_type env container typ in
        Some (InstanceVariable {id; doc; mutable_; virtual_; type_})
  | Tctf_method(name, private_, virtual_, typ) ->
      let open Method in
      let name = parenthesise name in
      let id = `Method(parent, MethodName.of_string name) in
      let private_ = (private_ = Private) in
      let virtual_ = (virtual_ = Virtual) in
    let type_ = read_core_type env container typ in
        Some (Method {id; doc; private_; virtual_; type_})
  | Tctf_constraint(typ1, typ2) ->
    let typ1 = read_core_type env container typ1 in
    let typ2 = read_core_type env container typ2 in
        Some (Constraint(typ1, typ2))
  | Tctf_inherit cltyp ->
      Some (Inherit (read_class_signature env parent container cltyp))
  | Tctf_attribute attr ->
      match Doc_attr.standalone container attr with
      | None -> None
      | Some doc -> Some (Comment doc)

and read_self_type env container typ =
  if typ.ctyp_desc = Ttyp_any then None
  else Some (read_core_type env container typ)

and read_class_signature env parent label_parent cltyp =
  let open ClassType in
    match cltyp.cltyp_desc with
    | Tcty_constr(p, _, params) ->
        let p = Env.Path.read_class_type env p in
      let params = List.map (read_core_type env label_parent) params in
          Constr(p, params)
    | Tcty_signature csig ->
        let open ClassSignature in
      let self = read_self_type env label_parent csig.csig_self in
        let items =
          List.fold_left
            (fun rest item ->
               match read_class_type_field env parent item with
               | None -> rest
               | Some item -> item :: rest)
            [] csig.csig_fields
        in
        let items = List.rev items in
          Signature {self; items}
    | Tcty_arrow _ -> assert false
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 06
    | Tcty_open _ -> assert false
#endif

let read_class_type_declaration env parent cltd =
  let open ClassType in
  let name = parenthesise (Ident.name cltd.ci_id_class_type) in
  let id = `ClassType(parent, Odoc_model.Names.ClassTypeName.of_string name) in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container cltd.ci_attributes in
  let virtual_ = (cltd.ci_virt = Virtual) in
  let params = List.map read_type_parameter cltd.ci_params in
  let expr = read_class_signature env id container cltd.ci_expr in
  { id; doc; virtual_; params; expr; expansion = None }

let read_class_type_declarations env parent cltds =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let open Signature in
  List.fold_left begin fun (acc,recursive) cltd ->
    let comments = Doc_attr.standalone_multiple container cltd.ci_attributes in
    let comments = List.map (fun com -> Comment com) comments in
    let cltd = read_class_type_declaration env parent cltd in
    ((ClassType (recursive, cltd))::(List.rev_append comments acc), And)
  end ([], Ordinary) cltds
  |> fst
  |> List.rev

let rec read_class_type env parent label_parent cty =
  let open Class in
  match cty.cltyp_desc with
  | Tcty_constr _ | Tcty_signature _ ->
    ClassType (read_class_signature env parent label_parent cty)
  | Tcty_arrow(lbl, arg, res) ->
      let lbl = read_label lbl in
    let arg = read_core_type env label_parent arg in
    let res = read_class_type env parent label_parent res in
        Arrow(lbl, arg, res)
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 06 && OCAML_MINOR < 08
  | Tcty_open (_, _, _, _, cty) -> read_class_type env parent label_parent cty
#elif OCAML_MAJOR = 4 && OCAML_MINOR >= 08
  | Tcty_open (_, cty) -> read_class_type env parent label_parent cty
#endif

let read_class_description env parent cld =
  let open Class in
  let name = parenthesise (Ident.name cld.ci_id_class) in
  let id = `Class(parent, Odoc_model.Names.ClassName.of_string name) in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container cld.ci_attributes in
  let virtual_ = (cld.ci_virt = Virtual) in
  let params = List.map read_type_parameter cld.ci_params in
  let type_ = read_class_type env id container cld.ci_expr in
  { id; doc; virtual_; params; type_; expansion = None }

let read_class_descriptions env parent clds =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let open Signature in
  List.fold_left begin fun (acc, recursive) cld ->
    let comments = Doc_attr.standalone_multiple container cld.ci_attributes in
    let comments = List.map (fun com -> Comment com) comments in
    let cld = read_class_description env parent cld in
    ((Class (recursive, cld))::(List.rev_append comments acc), And)
  end ([], Ordinary) clds
  |> fst
  |> List.rev

let rec read_with_constraint env parent (_, frag, constr) =
  let open ModuleType in
    match constr with
    | Twith_type decl ->
        let frag = Env.Fragment.read_type frag.Location.txt in
        let eq = read_type_equation env parent decl in
          TypeEq(frag, eq)
    | Twith_module(p, _) ->
        let frag = Env.Fragment.read_module frag.Location.txt in
        let eq = read_module_equation env p in
          ModuleEq(frag, eq)
    | Twith_typesubst decl ->
        let frag = Env.Fragment.read_type frag.Location.txt in
        let eq = read_type_equation env parent decl in
          TypeSubst(frag, eq)
    | Twith_modsubst(p, _) ->
        let frag = Env.Fragment.read_module frag.Location.txt in
        let p = Env.Path.read_module env p in
          ModuleSubst(frag, p)

and read_module_type env parent label_parent pos mty =
  let open ModuleType in
    match mty.mty_desc with
    | Tmty_ident(p, _) -> Path (Env.Path.read_module_type env p)
    | Tmty_signature sg -> Signature (read_signature env parent sg)
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 10
    | Tmty_functor(parameter, res) ->
        let parameter, env =
          match parameter with
          | Unit -> FunctorParameter.Unit, env
          | Named (id_opt, _, arg) ->
            let name, env =
              match id_opt with
              | Some id ->
                parenthesise (Ident.name id), Env.add_argument parent pos id (ArgumentName.of_ident id) env
              | None -> "_", env
            in
            let id = `Argument(parent, pos, ArgumentName.of_string name) in
            let arg = read_module_type env id label_parent 1 arg in
            let expansion =
                match arg with
                | Signature _ -> Some Module.AlreadyASig
                | _ -> None
              in
            Named { id; expr = arg; expansion }, env
        in
        let res = read_module_type env parent label_parent (pos + 1) res in
        Functor(parameter, res)
#else
    | Tmty_functor(id, _, arg, res) ->
        let arg =
          match arg with
          | None -> Odoc_model.Lang.FunctorParameter.Unit
          | Some arg ->
              let name = parenthesise (Ident.name id) in
              let id = `Argument(parent, pos, Odoc_model.Names.ArgumentName.of_string name) in
              let arg = read_module_type env id label_parent 1 arg in
              let expansion =
                match arg with
                | Signature _ -> Some Module.AlreadyASig
                | _ -> None
              in
              Named { FunctorParameter. id; expr = arg; expansion }
        in
        let env = Env.add_argument parent pos id (ArgumentName.of_ident id) env in
        let res = read_module_type env parent label_parent (pos + 1) res in
        Functor(arg, res)
#endif
    | Tmty_with(body, subs) ->
      let body = read_module_type env parent label_parent pos body in
      let subs = List.map (read_with_constraint env label_parent) subs in
          With(body, subs)
    | Tmty_typeof mexpr ->
        let decl =
          let open Module in
          match mexpr.mod_desc with
          | Tmod_ident(p, _) -> Alias (Env.Path.read_module env p)
          | _ ->
              let mty =
                Cmi.read_module_type env parent pos (Odoc_model.Compat.module_type mexpr.mod_type)
              in
                ModuleType mty
        in
          TypeOf decl
    | Tmty_alias _ -> assert false

and read_module_type_declaration env parent mtd =
  let open ModuleType in
  let name = parenthesise (Ident.name mtd.mtd_id) in
  let id = `ModuleType(parent, (Odoc_model.Names.ModuleTypeName.of_string name)) in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container mtd.mtd_attributes in
  let expr = opt_map (read_module_type env id container 1) mtd.mtd_type in
  let expansion =
    match expr with
    | Some (Signature _) -> Some Module.AlreadyASig
    | _ -> None
  in
    {id; doc; expr; expansion}

and read_module_declaration env parent md =
  let open Module in
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 10
  match md.md_id with
  | None -> None
  | Some id ->
    let name = parenthesise (Ident.name id) in
    let id = `Module(parent, Odoc_model.Names.ModuleName.of_string name) in
#else
  let name = parenthesise (Ident.name md.md_id) in
  let id = `Module(parent, Odoc_model.Names.ModuleName.of_string name) in
#endif

  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container md.md_attributes in
  let canonical =
    let doc = List.map Odoc_model.Location_.value doc in
    match List.find (function `Tag (`Canonical _) -> true | _ -> false) doc with
    | exception Not_found -> None
    | `Tag (`Canonical (p, r)) -> Some (p, r)
    | _ -> None
  in
  let type_ =
    match md.md_type.mty_desc with
    | Tmty_alias(p, _) -> Alias (Env.Path.read_module env p)
    | _ -> ModuleType (read_module_type env id container 1 md.md_type)
  in
  let hidden =
#if OCAML_MAJOR=4 && OCAML_MINOR >= 10
    match canonical, md.md_id with
    | None, Some id -> Odoc_model.Root.contains_double_underscore (Ident.name id)
    | _,_ -> false
#else
    match canonical with
    | None -> Odoc_model.Root.contains_double_underscore (Ident.name md.md_id)
    | _ -> false
#endif
  in
  let expansion =
    match type_ with
    | ModuleType (ModuleType.Signature _) -> Some AlreadyASig
    | _ -> None
  in
  Some {id; doc; type_; expansion; canonical; hidden; display_type = None}

and read_module_declarations env parent mds =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let open Signature in
  List.fold_left
    (fun (acc, recursive) md ->
      let comments = Doc_attr.standalone_multiple container md.md_attributes in
      let comments = List.map (fun com -> Comment com) comments in
      match read_module_declaration env parent md with
      | Some md -> ((Module (recursive, md))::(List.rev_append comments acc), And)
      | None -> acc, recursive)
    ([], Rec) mds
  |> fst
  |> List.rev

and read_module_equation env p =
  let open Module in
    Alias (Env.Path.read_module env p)

#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
and module_of_extended_open env parent o =
  let open Module in
  let id = `Module (parent, Odoc_model.Names.ModuleName.internal_of_string (Env.module_name_of_open o)) in
  let (p,_) = o.Typedtree.open_expr in
  let type_ = Alias (Env.Path.read_module env p) in
  { id
  ; doc = []
  ; type_
  ; canonical = None
  ; hidden = true
  ; display_type = None
  ; expansion = None }
#endif

and read_signature_item env parent item =
  let open Signature in
    match item.sig_desc with
    | Tsig_value vd ->
        [read_value_description env parent vd]
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
    | Tsig_type decls ->
      let rec_flag = Ordinary in
#else
    | Tsig_type (rec_flag, decls) ->
      let rec_flag =
        match rec_flag with
        | Recursive -> Ordinary
        | Nonrecursive -> Nonrec
      in
#endif
      read_type_declarations env parent rec_flag decls
    | Tsig_typext tyext ->
        [TypExt (read_type_extension env parent tyext)]
    | Tsig_exception ext ->
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
        [Exception (read_exception env parent ext.tyexn_constructor)]
#else
        [Exception (read_exception env parent ext)]
#endif
    | Tsig_module md -> begin
        match read_module_declaration env parent md with
        | Some m -> [Module (Ordinary, m)]
        | None -> []
        end
    | Tsig_recmodule mds ->
        read_module_declarations env parent mds
    | Tsig_modtype mtd ->
        [ModuleType (read_module_type_declaration env parent mtd)]
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
    | Tsig_open o ->
        [Comment `Stop; Module (Ordinary, module_of_extended_open env parent o); Comment `Stop]
#else
    | Tsig_open _ -> []
#endif
    | Tsig_include incl ->
        [Include (read_include env parent incl)]
    | Tsig_class cls ->
        read_class_descriptions env parent cls
    | Tsig_class_type cltyps ->
        read_class_type_declarations env parent cltyps
    | Tsig_attribute attr -> begin
        let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
          match Doc_attr.standalone container attr with
          | None -> []
          | Some doc -> [Comment doc]
      end
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
    | Tsig_typesubst tst ->
        read_type_substitutions env parent tst
    | Tsig_modsubst mst ->
        [ModuleSubstitution (read_module_substitution env parent mst)]

and read_module_substitution env parent ms =
  let open ModuleSubstitution in
  let name = parenthesise (Ident.name ms.ms_id) in
  let id = `Module(parent, (Odoc_model.Names.ModuleName.of_string name)) in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container ms.ms_attributes in
  let manifest = Env.Path.read_module env ms.ms_manifest in
  { id; doc; manifest }
#endif

and read_include env parent incl =
  let open Include in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container incl.incl_attributes in
  let expr = read_module_type env parent container 1 incl.incl_mod in
  let decl = Module.ModuleType expr in
  let content = Cmi.read_signature env parent (Odoc_model.Compat.signature incl.incl_type) in
  let expansion = { content; resolved = false} in
    {parent; doc; decl; expansion}

and read_signature env parent sg =
  let env =
    Env.add_signature_tree_items parent sg env
  in
  let items =
    List.fold_left
      (fun items item ->
         List.rev_append (read_signature_item env parent item) items)
      [] sg.sig_items
  in
    List.rev items

let read_interface root name intf =
  let id = `Root(root, Odoc_model.Names.UnitName.of_string name) in
  let items = read_signature Env.empty id intf in
  let doc, items =
    let open Signature in
    match items with
    | Comment (`Docs doc) :: items -> doc, items
    | _ -> Doc_attr.empty, items
  in
    (id, doc, items)
