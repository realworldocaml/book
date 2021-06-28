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
open Types

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

let opt_iter f = function
  | None -> ()
  | Some x -> f x

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

let read_label lbl =
  let open TypeExpr in
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
  (* NOTE(@ostera): 4.02 does not have an Asttypes variant for whether the
   * label exists, and is an optional label or not, so I went back to string
   * manipulation *)
  if String.length lbl == 0
  then None
  else match String.get lbl 0 with
      | '?' -> Some (Optional (String.sub lbl 1 (String.length lbl - 1)))
      | _ -> Some (Label lbl)
#else
  match lbl with
  | Asttypes.Nolabel -> None
  | Asttypes.Labelled s -> Some (Label s)
  | Asttypes.Optional s -> Some (Optional s)
#endif

(* Handle type variable names *)

let used_names = ref []
let name_counter = ref 0
let reserved_names = ref []

let reset_names () = used_names := []; name_counter := 0; reserved_names := []

let reserve_name = function
  | Some name ->
      if not (List.mem name !reserved_names) then
        reserved_names := name :: !reserved_names
  | None -> ()

let rec next_name () =
  let name =
    if !name_counter < 26
    then String.make 1 (Char.chr(97 + !name_counter))
    else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
           string_of_int(!name_counter / 26)
  in
    incr name_counter;
    if List.mem name !reserved_names then next_name ()
    else name

let fresh_name base =
  let current_name = ref base in
  let i = ref 0 in
  while List.exists (fun (_, name') -> !current_name = name') !used_names do
    current_name := base ^ (string_of_int !i);
    i := !i + 1;
  done;
  !current_name

let name_of_type (ty : Types.type_expr) =
  try
    List.assq ty !used_names
  with Not_found ->
    let base =
      match ty.desc with
      | Tvar (Some name) | Tunivar (Some name) -> name
      | _ -> next_name ()
    in
    let name = fresh_name base in
    if name <> "_" then used_names := (ty, name) :: !used_names;
    name

let remove_names tyl =
  used_names := List.filter (fun (ty,_) -> not (List.memq ty tyl)) !used_names

(* Handle recursive types and shared row variables *)

let aliased = ref []
let used_aliases = ref []

let reset_aliased () = aliased := []; used_aliases := []

let is_aliased px = List.memq px !aliased

let aliasable (ty : Types.type_expr) =
  match ty.desc with
  | Tvar _ | Tunivar _ | Tpoly _ -> false
  | _ -> true

let add_alias ty =
  let px = Btype.proxy ty in
  if not (List.memq px !aliased) then begin
    aliased := px :: !aliased;
    match px.desc with
    | Tvar name | Tunivar name -> reserve_name name
    | _ -> ()
  end

let used_alias (px : Types.type_expr) = List.memq px !used_aliases

let use_alias (px : Types.type_expr) = used_aliases := px :: !used_aliases

let visited_rows = ref []

let reset_visited_rows () = visited_rows := []

let is_row_visited px = List.memq px !visited_rows

let visit_row px =
  visited_rows := px :: !visited_rows

let visit_object ty px =
  if Ctype.opened_object ty then
    visited_rows := px :: !visited_rows

let namable_row row =
  row.row_name <> None &&
  List.for_all
    (fun (_, f) ->
       match Btype.row_field_repr f with
       | Reither(c, l, _, _) ->
           row.row_closed && if c then l = [] else List.length l = 1
       | _ -> true)
    row.row_fields

let mark_type ty =
  let rec loop visited ty =
    let ty = Btype.repr ty in
    let px = Btype.proxy ty in
    if List.memq px visited && aliasable ty then add_alias px else
      let visited = px :: visited in
      match ty.desc with
      | Tvar name -> reserve_name name
      | Tarrow(_, ty1, ty2, _) ->
          loop visited ty1;
          loop visited ty2
      | Ttuple tyl -> List.iter (loop visited) tyl
      | Tconstr(_, tyl, _) ->
          List.iter (loop visited) tyl
      | Tvariant row ->
          if is_row_visited px then add_alias px else
           begin
            let row = Btype.row_repr row in
            if not (Btype.static_row row) then visit_row px;
            match row.row_name with
            | Some(_, tyl) when namable_row row ->
                List.iter (loop visited) tyl
            | _ ->
                Btype.iter_row (loop visited) row
           end
      | Tobject (fi, nm) ->
          if is_row_visited px then add_alias px else
           begin
            visit_object ty px;
            match !nm with
            | None ->
                let fields, _ = Ctype.flatten_fields fi in
                List.iter
                  (fun (_, kind, ty) ->
                    if Btype.field_kind_repr kind = Fpresent then
                      loop visited ty)
                  fields
            | Some (_, l) ->
                List.iter (loop visited) (List.tl l)
          end
      | Tfield(_, kind, ty1, ty2) when Btype.field_kind_repr kind = Fpresent ->
          loop visited ty1;
          loop visited ty2
      | Tfield(_, _, _, ty2) ->
          loop visited ty2
      | Tnil -> ()
      | Tpoly (ty, tyl) ->
          List.iter (fun t -> add_alias t) tyl;
          loop visited ty
      | Tunivar name -> reserve_name name
      | Tpackage(_, _, tyl) ->
          List.iter (loop visited) tyl
      | Tsubst ty -> loop visited ty
      | Tlink _ -> assert false
  in
  loop [] ty

let reset_context () =
  reset_names ();
  reset_aliased ();
  reset_visited_rows ()

let mark_type_expr t =
  reset_context ();
  mark_type t

let mark_value_description vd =
  reset_context ();
  mark_type vd.val_type

let mark_type_parameter param =
  add_alias param;
  mark_type param;
  if aliasable param then use_alias (Btype.proxy param)

let prepare_type_parameters params manifest =
  let params =
    List.fold_left
      (fun params param ->
        let param = Btype.repr param in
        if List.memq param params then Btype.newgenty (Tsubst param) :: params
        else param :: params)
      [] params
  in
  let params = List.rev params in
  begin match manifest with
    | Some ty ->
        let vars = Ctype.free_variables ty in
          List.iter
            (function {desc = Tvar (Some "_"); _} as ty ->
              if List.memq ty vars then ty.desc <- Tvar None
                    | _ -> ())
            params
    | None -> ()
  end;
  params

(* NOTE(@ostera): constructor with inlined records were introduced post 4.02 *)
let mark_constructor_args =
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
  List.iter mark_type
#else
  function
   | Cstr_tuple args -> List.iter mark_type args
   | Cstr_record lds -> List.iter (fun ld -> mark_type ld.ld_type) lds
#endif

let mark_type_kind = function
  | Type_abstract -> ()
  | Type_variant cds ->
      List.iter
        (fun cd ->
           mark_constructor_args cd.cd_args;
           opt_iter mark_type cd.cd_res)
        cds
  | Type_record(lds, _) ->
      List.iter (fun ld -> mark_type ld.ld_type) lds
  | Type_open -> ()

let mark_type_declaration decl =
  let params = prepare_type_parameters decl.type_params decl.type_manifest in
    reset_context ();
    List.iter mark_type_parameter params;
    opt_iter mark_type decl.type_manifest;
    mark_type_kind decl.type_kind;
    params

let mark_extension_constructor ext =
  mark_constructor_args ext.ext_args;
  opt_iter mark_type ext.ext_ret_type

let mark_type_extension type_params exts =
  let type_params = prepare_type_parameters type_params None in
    reset_context ();
    List.iter mark_type_parameter type_params;
    List.iter mark_extension_constructor exts;
    type_params

let mark_type_extension' ext rest =
  let type_params = ext.ext_type_params in
  let exts = ext :: (List.map snd rest) in
    mark_type_extension type_params exts

let mark_exception ext =
  reset_context ();
  mark_extension_constructor ext

let rec mark_class_type params = function
  | Cty_constr (_, tyl, cty) ->
      let sty = Ctype.self_type cty in
      if is_row_visited (Btype.proxy sty)
      || List.exists aliasable params
      || List.exists (Ctype.deep_occur sty) tyl
      then mark_class_type params cty
      else List.iter mark_type tyl
  | Cty_signature sign ->
      let sty = Btype.repr sign.csig_self in
      let px = Btype.proxy sty in
      if is_row_visited px then add_alias sty
      else visit_row px;
      let (fields, _) =
        Ctype.flatten_fields (Ctype.object_fields sign.csig_self)
      in
      List.iter (fun (_, _, ty) -> mark_type ty) fields;
      Vars.iter (fun _ (_, _, ty) -> mark_type ty) sign.csig_vars;
      if is_aliased sty && aliasable sty then use_alias px
  | Cty_arrow (_, ty, cty) ->
      mark_type ty;
      mark_class_type params cty

let mark_class_type_declaration cltd =
  reset_context ();
  List.iter mark_type_parameter cltd.clty_params;
  mark_class_type cltd.clty_params cltd.clty_type

let mark_class_declaration cld =
  reset_context ();
  List.iter mark_type_parameter cld.cty_params;
  mark_class_type cld.cty_params cld.cty_type

let rec read_type_expr env typ =
  let open TypeExpr in
  let typ = Btype.repr typ in
  let px = Btype.proxy typ in
  if used_alias px then Var (name_of_type typ)
  else begin
    let alias =
      if not (is_aliased px && aliasable typ) then None
      else begin
        use_alias px;
        Some (name_of_type typ)
      end
    in
    let typ =
      match typ.desc with
      | Tvar _ ->
          let name = name_of_type typ in
            if name = "_" then Any
            else Var name
      | Tarrow(lbl, arg, res, _) ->
          let arg =
            if Btype.is_optional lbl then
              match (Btype.repr arg).desc with
              | Tconstr(_option, [arg], _) -> read_type_expr env arg
              | _ -> assert false
            else read_type_expr env arg
          in
          let lbl = read_label lbl in
          let res = read_type_expr env res in
            Arrow(lbl, arg, res)
      | Ttuple typs ->
          let typs = List.map (read_type_expr env) typs in
            Tuple typs
      | Tconstr(p, params, _) ->
          let p = Env.Path.read_type env p in
          let params = List.map (read_type_expr env) params in
            Constr(p, params)
      | Tvariant row -> read_row env px row
      | Tobject (fi, nm) -> read_object env fi !nm
      | Tnil | Tfield _ -> read_object env typ None
      | Tpoly (typ, []) -> read_type_expr env typ
      | Tpoly (typ, tyl) ->
          let tyl = List.map Btype.repr tyl in
          let vars = List.map name_of_type tyl in
          let typ = read_type_expr env typ in
            remove_names tyl;
            Poly(vars, typ)
      | Tunivar _ -> Var (name_of_type typ)
      | Tpackage(p, frags, tyl) ->
          let open TypeExpr.Package in
          let path = Env.Path.read_module_type env p in
          let substitutions =
            List.map2
              (fun frag typ ->
                 let frag = Env.Fragment.read_type frag in
                 let typ = read_type_expr env typ in
                   (frag, typ))
              frags tyl
          in
            Package {path; substitutions}
      | Tsubst typ -> read_type_expr env typ
      | Tlink _ -> assert false
    in
      match alias with
      | None -> typ
      | Some name -> Alias(typ, name)
  end

and read_row env _px row =
  let open TypeExpr in
  let open TypeExpr.Polymorphic_variant in
  let row = Btype.row_repr row in
  let fields =
    if row.row_closed then
      List.filter (fun (_, f) -> Btype.row_field_repr f <> Rabsent)
        row.row_fields
    else row.row_fields in
  let sorted_fields = List.sort (fun (p,_) (q,_) -> compare p q) fields in
  let present =
    List.filter
      (fun (_, f) ->
         match Btype.row_field_repr f with
         | Rpresent _ -> true
         | _ -> false)
      sorted_fields in
  let all_present = List.length present = List.length sorted_fields in
  match row.row_name with
  | Some(p, params) when namable_row row ->
      let p = Env.Path.read_type env p in
      let params = List.map (read_type_expr env) params in
      if row.row_closed && all_present then
        Constr (p, params)
      else
        let kind =
          if all_present then Open else Closed (List.map fst present)
        in
        Polymorphic_variant {kind; elements = [Type (Constr (p, params))]}
  | _ ->
      let elements =
        List.map
          (fun (name, f) ->
            match Btype.row_field_repr f with
              | Rpresent None ->
                Constructor {name; constant = true; arguments = []; doc = []}
              | Rpresent (Some typ) ->
                Constructor {
                  name;
                  constant = false;
                  arguments = [read_type_expr env typ];
                  doc = [];
                }
              | Reither(constant, typs, _, _) ->
                  let arguments =
                    List.map (read_type_expr env) typs
                  in
                Constructor {name; constant; arguments; doc = []}
              | Rabsent -> assert false)
          sorted_fields
      in
      let kind =
        if all_present then
          if row.row_closed then Fixed
          else Open
        else Closed (List.map fst present)
      in
      Polymorphic_variant {kind; elements}

and read_object env fi nm =
  let open TypeExpr in
  let open TypeExpr.Object in
  match nm with
  | None ->
      let (fields, rest) = Ctype.flatten_fields fi in
      let present_fields =
        List.fold_right
          (fun (n, k, t) l ->
             match Btype.field_kind_repr k with
             | Fpresent -> (n, t) :: l
             | _ -> l)
          fields []
      in
      let sorted_fields =
        List.sort (fun (n, _) (n', _) -> compare n n') present_fields
      in
      let methods =
        List.map
          (fun (name, typ) -> Method {name; type_ = read_type_expr env typ})
          sorted_fields
      in
      let open_ =
        match rest.desc with
        | Tvar _ | Tunivar _ -> true
        | Tconstr _ -> true
        | Tnil -> false
        | _ -> assert false
      in
      Object {fields = methods; open_}
  | Some (p, _ :: params) ->
      let p = Env.Path.read_class_type env p in
      let params = List.map (read_type_expr env) params in
      Class (p, params)
  | _ -> assert false

let read_value_description env parent id vd =
  let open Signature in
  let name = parenthesise (Ident.name id) in
  let id = `Value(parent, Odoc_model.Names.ValueName.of_string name) in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container vd.val_attributes in
    mark_value_description vd;
    let type_ = read_type_expr env vd.val_type in
    match vd.val_kind with
    | Val_reg -> Value {Value.id; doc; type_}
    | Val_prim desc ->
        let primitives =
          let open Primitive in
          desc.prim_name :: (
            match desc.prim_native_name with
            | "" -> []
            | name -> [ name ]
          )
        in
          External {External.id; doc; type_; primitives}
    | _ -> assert false

let read_label_declaration env parent ld =
  let open TypeDecl.Field in
  let name = parenthesise (Ident.name ld.ld_id) in
  let id = `Field(parent, Odoc_model.Names.FieldName.of_string name) in
  let doc =
    Doc_attr.attached
      (parent :> Identifier.LabelParent.t) ld.ld_attributes
  in
  let mutable_ = (ld.ld_mutable = Mutable) in
  let type_ = read_type_expr env ld.ld_type in
    {id; doc; mutable_; type_}

let read_constructor_declaration_arguments env parent arg =
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
  (* NOTE(@ostera): constructor with inlined records were introduced post 4.02
     so it's safe to use Tuple here *)
  ignore parent;
  TypeDecl.Constructor.Tuple(List.map (read_type_expr env) arg)
#else
  let open TypeDecl.Constructor in
    match arg with
    | Cstr_tuple args -> Tuple (List.map (read_type_expr env) args)
    | Cstr_record lds ->
        Record (List.map (read_label_declaration env parent) lds)
#endif

let read_constructor_declaration env parent cd =
  let open TypeDecl.Constructor in
  let name = parenthesise (Ident.name cd.cd_id) in
  let id = `Constructor(parent, Odoc_model.Names.ConstructorName.of_string name) in
  let container = (parent : Identifier.DataType.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container cd.cd_attributes in
  let args =
    read_constructor_declaration_arguments env
      (parent :> Identifier.Parent.t) cd.cd_args
  in
  let res = opt_map (read_type_expr env) cd.cd_res in
    {id; doc; args; res}

let read_type_kind env parent =
  let open TypeDecl.Representation in function
    | Type_abstract -> None
    | Type_variant cstrs ->
        let cstrs =
          List.map (read_constructor_declaration env parent) cstrs
        in
          Some (Variant cstrs)
    | Type_record(lbls, _) ->
        let lbls =
          List.map
            (read_label_declaration env (parent :> Identifier.Parent.t))
            lbls
        in
          Some (Record lbls)
    | Type_open ->  Some Extensible

let read_type_parameter abstr var param =
  let open TypeDecl in
  let name = name_of_type param in
  let desc =
    if name = "_" then Any
    else Var name
  in
  let var =
    if not (abstr || aliasable param) then None
    else begin
      let co, cn = Variance.get_upper var in
        if not cn then Some Pos
        else if not co then Some Neg
        else None
    end
  in
    (desc, var)

let read_type_constraints env params =
  List.fold_right
    (fun typ1 acc ->
       let typ2 = Ctype.unalias typ1 in
       if Btype.proxy typ1 != Btype.proxy typ2 then
         let typ1 = read_type_expr env typ1 in
         let typ2 = read_type_expr env typ2 in
           (typ1, typ2) :: acc
       else acc)
    params []

let read_type_declaration env parent id decl =
  let open TypeDecl in
  let name = parenthesise (Ident.name id) in
  let id = `Type(parent, Odoc_model.Names.TypeName.of_string name) in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container decl.type_attributes in
  let params = mark_type_declaration decl in
  let manifest = opt_map (read_type_expr env) decl.type_manifest in
  let constraints = read_type_constraints env params in
  let representation = read_type_kind env id decl.type_kind in
  let abstr =
    match decl.type_kind with
      Type_abstract ->
        decl.type_manifest = None || decl.type_private = Private
    | Type_record _ ->
        decl.type_private = Private
    | Type_variant tll ->
        decl.type_private = Private ||
        List.exists (fun cd -> cd.cd_res <> None) tll
    | Type_open ->
        decl.type_manifest = None
  in
  let params =
    List.map2 (read_type_parameter abstr) decl.type_variance params
  in
  let private_ = (decl.type_private = Private) in
  let equation = Equation.{params; manifest; constraints; private_} in
    {id; doc; equation; representation}

let read_extension_constructor env parent id ext =
  let open Extension.Constructor in
  let name = parenthesise (Ident.name id) in
  let id = `Extension(parent, Odoc_model.Names.ExtensionName.of_string name) in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container ext.ext_attributes in
  let args =
    read_constructor_declaration_arguments env
      (parent : Identifier.Signature.t :> Identifier.Parent.t) ext.ext_args
  in
  let res = opt_map (read_type_expr env) ext.ext_ret_type in
    {id; doc; args; res}

let read_type_extension env parent id ext rest =
  let open Extension in
  let type_path = Env.Path.read_type env ext.ext_type_path in
  let doc = Doc_attr.empty in
  let type_params = mark_type_extension' ext rest in
  let first = read_extension_constructor env parent id ext in
  let rest =
    List.map
      (fun (id, ext) -> read_extension_constructor env parent id ext)
      rest
  in
  let constructors = first :: rest in
  let type_params =
    List.map (read_type_parameter false Variance.null) type_params
  in
  let private_ = (ext.ext_private = Private) in
    { type_path; type_params;
      doc; private_;
      constructors; }

let read_exception env parent id ext =
  let open Exception in
  let name = parenthesise (Ident.name id) in
  let id = `Exception(parent, Odoc_model.Names.ExceptionName.of_string name) in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container ext.ext_attributes in
    mark_exception ext;
    let args =
      read_constructor_declaration_arguments env
        (parent : Identifier.Signature.t :> Identifier.Parent.t) ext.ext_args
    in
    let res = opt_map (read_type_expr env) ext.ext_ret_type in
      {id; doc; args; res}

let read_method env parent concrete (name, kind, typ) =
  let open Method in
  let name = parenthesise name in
  let id = `Method(parent, Odoc_model.Names.MethodName.of_string name) in
  let doc = Doc_attr.empty in
  let private_ = (Btype.field_kind_repr kind) <> Fpresent in
  let virtual_ = not (Concr.mem name concrete) in
  let type_ = read_type_expr env typ in
    ClassSignature.Method {id; doc; private_; virtual_; type_}

let read_instance_variable env parent (name, mutable_, virtual_, typ) =
  let open InstanceVariable in
  let name = parenthesise name in
  let id = `InstanceVariable(parent, Odoc_model.Names.InstanceVariableName.of_string name) in
  let doc = Doc_attr.empty in
  let mutable_ = (mutable_ = Mutable) in
  let virtual_ = (virtual_ = Virtual) in
  let type_ = read_type_expr env typ in
    ClassSignature.InstanceVariable {id; doc; mutable_; virtual_; type_}

let read_self_type sty =
  let sty = Btype.repr sty in
    if not (is_aliased sty) then None
    else Some (TypeExpr.Var (name_of_type (Btype.proxy sty)))

let rec read_class_signature env parent params =
  let open ClassType in function
  | Cty_constr(p, _, cty) ->
      if is_row_visited (Btype.proxy (Ctype.self_type cty))
      || List.exists aliasable params
      then read_class_signature env parent params cty
      else begin
        let p = Env.Path.read_class_type env p in
        let params = List.map (read_type_expr env) params in
          Constr(p, params)
      end
  | Cty_signature csig ->
      let open ClassSignature in
      let self = read_self_type csig.csig_self in
      let constraints = read_type_constraints env params in
      let constraints =
        List.map
          (fun (typ1, typ2) -> Constraint(typ1, typ2))
          constraints
      in
      let instance_variables =
        Vars.fold
          (fun name (mutable_, virtual_, typ) acc ->
             (name, mutable_, virtual_, typ) :: acc)
          csig.csig_vars []
      in
      let methods, _ =
        Ctype.flatten_fields (Ctype.object_fields csig.csig_self)
      in
      let methods =
        List.filter (fun (name, _, _) -> name <> Btype.dummy_method) methods
      in
      let instance_variables =
        List.map (read_instance_variable env parent) instance_variables
      in
      let methods =
        List.map (read_method env parent csig.csig_concr) methods
      in
      let items = constraints @ instance_variables @ methods in
        Signature {self; items}
  | Cty_arrow _ -> assert false

let rec read_virtual = function
  | Cty_constr(_, _, cty) | Cty_arrow(_, _, cty) -> read_virtual cty
  | Cty_signature csig ->
      let methods, _ =
        Ctype.flatten_fields (Ctype.object_fields csig.csig_self)
      in
      let virtual_method =
        List.exists
          (fun (name, _, _) ->
             not (name = Btype.dummy_method
                 || Concr.mem name csig.csig_concr))
          methods
      in
      let virtual_instance_variable =
        Vars.exists
          (fun _ (_, virtual_, _) -> virtual_ = Virtual)
          csig.csig_vars
      in
        virtual_method || virtual_instance_variable

let read_class_type_declaration env parent id cltd =
  let open ClassType in
  let name = parenthesise (Ident.name id) in
  let id = `ClassType(parent, Odoc_model.Names.ClassTypeName.of_string name) in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container cltd.clty_attributes in
    mark_class_type_declaration cltd;
    let params =
      List.map2
        (read_type_parameter false)
        cltd.clty_variance cltd.clty_params
    in
    let expr =
      read_class_signature env id cltd.clty_params cltd.clty_type
    in
    let virtual_ = read_virtual cltd.clty_type in
    { id; doc; virtual_; params; expr; expansion = None }

let rec read_class_type env parent params =
  let open Class in function
  | Cty_constr _ | Cty_signature _ as cty ->
      ClassType (read_class_signature env parent params cty)
  | Cty_arrow(lbl, arg, cty) ->
      let arg =
        if Btype.is_optional lbl then
          match (Btype.repr arg).desc with
          | Tconstr(path, [arg], _)
            when OCamlPath.same path Predef.path_option ->
              read_type_expr env arg
          | _ -> assert false
        else read_type_expr env arg
      in
      let lbl = read_label lbl in
      let cty = read_class_type env parent params cty in
        Arrow(lbl, arg, cty)

let read_class_declaration env parent id cld =
  let open Class in
  let name = parenthesise (Ident.name id) in
  let id = `Class(parent, Odoc_model.Names.ClassName.of_string name) in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container cld.cty_attributes in
    mark_class_declaration cld;
    let params =
      List.map2
        (read_type_parameter false)
        cld.cty_variance cld.cty_params
    in
    let type_ =
      read_class_type env id cld.cty_params cld.cty_type
    in
    let virtual_ = cld.cty_new = None in
    { id; doc; virtual_; params; type_; expansion = None }

let rec read_module_type env parent pos (mty : Odoc_model.Compat.module_type) =
  let open ModuleType in
    match mty with
    | Mty_ident p -> Path (Env.Path.read_module_type env p)
    | Mty_signature sg -> Signature (read_signature env parent sg)
    | Mty_functor(parameter, res) ->
        let parameter, env =
          match parameter with
          | Unit -> Odoc_model.Lang.FunctorParameter.Unit, env
          | Named (id_opt, arg) ->
              let name, env = match id_opt with
                | Some id -> parenthesise (Ident.name id), Env.add_argument parent pos id (ArgumentName.of_ident id) env
                | None -> "_", env
              in
              let id = `Argument(parent, pos, Odoc_model.Names.ArgumentName.of_string name) in
              let arg = read_module_type env id 1 arg in
              let expansion =
                match arg with
                | Signature _ -> Some Module.AlreadyASig
                | _ -> None
              in
              Odoc_model.Lang.FunctorParameter.Named ({ FunctorParameter. id; expr = arg; expansion }), env
        in
        let res = read_module_type env parent (pos+1) res in
        Functor(parameter, res)
    | Mty_alias _ -> assert false

and read_module_type_declaration env parent id (mtd : Odoc_model.Compat.modtype_declaration) =
  let open ModuleType in
  let name = parenthesise (Ident.name id) in
  let id = `ModuleType(parent, Odoc_model.Names.ModuleTypeName.of_string name) in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached container mtd.mtd_attributes in
  let expr = opt_map (read_module_type env id 1) mtd.mtd_type in
  let expansion =
    match expr with
    | Some (Signature _) -> Some Module.AlreadyASig
    | _ -> None
  in
    {id; doc; expr; expansion}

and read_module_declaration env parent ident (md : Odoc_model.Compat.module_declaration) =
  let open Module in
  let name = parenthesise (Ident.name ident) in
  let id = `Module(parent, Odoc_model.Names.ModuleName.of_string name) in
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
    match md.md_type with
    | Mty_alias p -> Alias (Env.Path.read_module env p)
    | _ -> ModuleType (read_module_type env id 1 md.md_type)
  in
  let hidden =
    match canonical with
    | Some _ -> false
    | None -> Odoc_model.Root.contains_double_underscore (Ident.name ident)
  in
  let expansion =
    match type_ with
    | ModuleType (ModuleType.Signature _) -> Some AlreadyASig
    | _ -> None
  in
    {id; doc; type_; expansion; canonical; hidden; display_type = None}

and read_type_rec_status rec_status =
  let open Signature in
  match rec_status with
  | Trec_first -> Ordinary
  | Trec_next -> And
  | Trec_not -> Nonrec

and read_module_rec_status rec_status =
  let open Signature in
  match rec_status with
  | Trec_not -> Ordinary
  | Trec_first -> Rec
  | Trec_next -> And

and read_signature env parent (items : Odoc_model.Compat.signature) =
  let env = Env.add_signature_type_items parent items env in
  let rec loop acc items =
    let open Signature in
    let open Odoc_model.Compat in
    match items with
    | Sig_value(id, v, Exported) :: rest ->
        let vd = read_value_description env parent id v in
          loop (vd :: acc) rest
    | Sig_type(id, _, _, Exported) :: rest
        when Btype.is_row_name (Ident.name id) ->
        loop acc rest
    | Sig_type(id, decl, rec_status, Exported)::rest ->
        let decl = read_type_declaration env parent id decl in
      loop (Type (read_type_rec_status rec_status, decl)::acc) rest
    | Sig_typext (id, ext, Text_first, Exported) :: rest ->
        let rec inner_loop inner_acc = function
          | Sig_typext(id, ext, Text_next, _) :: rest ->
              inner_loop ((id, ext) :: inner_acc) rest
          | rest ->
              let ext =
                read_type_extension env parent id ext (List.rev inner_acc)
              in
                loop (TypExt ext :: acc) rest
        in
          inner_loop [] rest
    | Sig_typext (id, ext, Text_next, Exported) :: rest ->
        let ext = read_type_extension env parent id ext [] in
          loop (TypExt ext :: acc) rest
    | Sig_typext (id, ext, Text_exception, Exported) :: rest ->
        let exn = read_exception env parent id ext in
          loop (Exception exn :: acc) rest
    | Sig_module (id, _, md, rec_status, Exported)::rest ->
          let md = read_module_declaration env parent id md in
          loop (Module (read_module_rec_status rec_status, md)::acc) rest
    | Sig_modtype(id, mtd, Exported) :: rest ->
          let mtd = read_module_type_declaration env parent id mtd in
          loop (ModuleType mtd :: acc) rest
    | Sig_class(id, cl, rec_status, Exported) :: Sig_class_type _
      :: Sig_type _ :: Sig_type _ :: rest ->
          let cl = read_class_declaration env parent id cl in
          loop (Class (read_type_rec_status rec_status, cl)::acc) rest
    | Sig_class_type(id, cltyp, rec_status, Exported)::Sig_type _::Sig_type _::rest ->
        let cltyp = read_class_type_declaration env parent id cltyp in
        loop (ClassType (read_type_rec_status rec_status, cltyp)::acc) rest

(* Skip all of the hidden sig items *)
    | Sig_class_type(_, _, _, Hidden)::Sig_type _::Sig_type _::rest
    | Sig_class(_, _, _, Hidden) :: Sig_class_type _
      :: Sig_type _ :: Sig_type _ :: rest
    | Sig_modtype(_, _, Hidden) :: rest
    | Sig_module (_, _, _, _, Hidden)::rest
    | Sig_typext (_, _, Text_exception, Hidden) :: rest
    | Sig_typext (_, _, _, Hidden) :: rest
    | Sig_type(_, _, _, Hidden) :: rest
    | Sig_value(_, _, Hidden) :: rest ->
      loop acc rest

(* Bad - we expect Sig_class and Sig_class_type to be matched above
   with subsequent Sig_type items *)
    | Sig_class_type _ :: _
    | Sig_class _ :: _ -> assert false

    | [] -> List.rev acc
  in
    loop [] items

let read_interface root name intf =
  let id = `Root(root, Odoc_model.Names.UnitName.of_string name) in
  let doc = Doc_attr.empty in
  let items = read_signature Env.empty id intf in
    (id, doc, items)
