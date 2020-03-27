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
open Names
open Paths
open Components
module CTbl = Component_table

let lpop x = (x : Reference.Parent.t :> Reference.LabelParent.t)
let rlpop x = (x : Reference.Resolved.Parent.t :> Reference.Resolved.LabelParent.t)

type parent_module_path =
  | Resolved of Path.Resolved.Module.t * Sig.t
  | Unresolved of Path.Module.t

type parent_module_type_path =
  | Resolved of Path.Resolved.ModuleType.t * Sig.t
  | Unresolved of Path.ModuleType.t

let rec add_subst_alias :
  Path.Resolved.Module.t ->
  subp:Path.Resolved.Module.t ->
  Path.Resolved.Module.t = fun orig ~subp ->
  if Path.Resolved.Module.equal orig subp then orig else
  match orig with
  | `Canonical (_, `Resolved rp) ->
    add_subst_alias rp ~subp
  | `Canonical (rp, p) ->
    let rp' = add_subst_alias rp ~subp in
    if rp == rp' then orig else `Canonical (rp', p)
  | `Module (p1, s1) -> begin
      match subp with
      | `Module (subp, s2) when s1 = s2 ->
        let p1' = add_subst_alias p1 ~subp in
        if p1 == p1' then orig else `Module (p1', s1)
      | _ ->
        `SubstAlias(subp, orig)
    end
  | `SubstAlias (subst, printing) ->
    if Path.Resolved.Module.equal subst subp then orig else
      `SubstAlias (subp, printing)
  (* Not trying to be smart in these cases, let's just subst... *)
  | _ ->
    `SubstAlias(subp, orig)

let rec find_with_path_substs
  : 'b 'c. (Sig.t -> 'b) -> (Path.Resolved.Module.t -> 'b -> 'c)
    -> (Path.Resolved.Module.t -> 'c) -> Identifier.Signature.t option
    -> CTbl.t -> Path.Resolved.Module.t -> Sig.t
    -> 'c =
  fun find resolved unresolved ident tbl pr parent ->
    try
      resolved pr (find parent)
    with Not_found ->
      try
        match Sig.find_parent_subst parent with
        | Parent.Subst subp -> begin
            match resolve_parent_module_type_path ident tbl subp with
            | Unresolved _ -> unresolved pr
            | Resolved(subpr, parent) ->
              let pr' =
                match pr with
                | `Canonical (_, `Resolved _) ->
                  `Subst(subpr, pr)
                | `Canonical (p1, p2) ->
                  `Canonical(`Subst(subpr, p1), p2)
                | _ ->
                  `Subst(subpr, pr)
              in
                find_with_path_substs
                  find resolved unresolved ident tbl
                  pr' parent
          end
        | Parent.SubstAlias subp -> begin
            match resolve_parent_module_path ident tbl subp with
            | Unresolved _ -> unresolved pr
            | Resolved(subpr, parent) ->
              let pr' = add_subst_alias pr ~subp:subpr in
              find_with_path_substs
                find resolved unresolved ident tbl
                pr' parent
          end
      with Not_found -> unresolved pr

and resolve_canonical_path_module :
  Identifier.Signature.t option -> _ -> Path.Resolved.Module.t -> Path.Resolved.Module.t =
  fun ident tbl p ->
    match p with
    | `Canonical (orig, cano) ->
      let orig' = resolve_resolved_module_path ident tbl orig in
      let cano' = resolve_module_path ident tbl cano in
      if orig == orig' && cano == cano' then (p :> Path.Resolved.Module.t)
      else (
        let cano' =
          match ident, cano' with
          | Some ident, `Resolved rp ->
            let rp' = Path.Resolved.Module.rebase ident rp in
            if rp != rp' then `Resolved rp' else cano'
          | _ -> cano'
        in
        let c = `Canonical(orig, cano') in
        match ident, cano' with
        | Some ident, `Resolved (`Identifier id)
          when Identifier.Signature.equal ident (id :> Identifier.Signature.t) ->
          `Hidden c
        | _, _ -> c
      )
    | _ -> p

and resolve_parent_module_path ident tbl (p : Path.Module.t) : parent_module_path =
    match p with
    | `Root s -> begin
        match CTbl.base tbl s with
        | CTbl.Not_found -> Unresolved p
        | CTbl.Forward_reference ->
            (* We can't have a forward ref as parent. *)
            Unresolved p
        | CTbl.Found { root ; hidden } ->
            let p = `Identifier (`Root(root, UnitName.of_string s)) in
            let p = if hidden then `Hidden p else p in
              Resolved(p, CTbl.resolved_module_path tbl p)
      end
    | `Forward s -> begin
        match CTbl.base tbl s with
        | CTbl.Not_found -> Unresolved p
        | CTbl.Forward_reference ->
            (* We can't have a forward ref as parent. *)
            Unresolved p
        | CTbl.Found { root ; hidden } ->
            let p = `Identifier (`Root(root, UnitName.of_string s)) in
            let p = if hidden then `Hidden p else p in
              Resolved(p, CTbl.resolved_module_path tbl p)
      end
    | `Resolved r -> Resolved(r, CTbl.resolved_module_path tbl r)
    | `Dot(pr, name) -> begin
        match resolve_parent_module_path ident tbl pr with
        | Unresolved pr -> Unresolved(`Dot(pr, name))
        | Resolved(pr, parent) ->
            let resolved pr (`Module md : Parent.module_) =
              let pr = `Module (pr, ModuleName.of_string name) in
              let pr = if Sig.get_hidden md then `Hidden pr else pr in
              let pr =
                match Sig.get_canonical md with
                | None -> pr
                | Some (p, _) ->
                  resolve_canonical_path_module ident tbl (`Canonical(pr, p))
              in
              (Resolved(pr, md) : parent_module_path)
            in
            let unresolved pr =
              (Unresolved(`Dot(`Resolved pr, name)) : parent_module_path)
            in
              find_with_path_substs
                (Sig.find_parent_module name)
                resolved unresolved ident tbl pr parent
      end
    | `Apply(pr, arg) -> begin
        let arg = resolve_module_path ident tbl arg in
        match resolve_parent_module_path ident tbl pr with
        | Unresolved pr -> Unresolved(`Apply(pr, arg))
        | Resolved(pr, parent) ->
            let resolved pr (`Module md : Parent.module_) =
              let pr = `Apply(pr, arg) in
              let pr = if Sig.get_hidden md then `Hidden pr else pr in
              let pr =
                match Sig.get_canonical md with
                | None -> pr
                | Some (p, _) ->
                  resolve_canonical_path_module ident tbl (`Canonical(pr, p))
              in
              (Resolved(pr, md) : parent_module_path)
            in
            let unresolved pr =
              (Unresolved(`Apply(`Resolved pr, arg)) : parent_module_path)
            in
              find_with_path_substs
                (Sig.find_parent_apply (CTbl.module_path tbl) arg)
                resolved unresolved ident tbl pr parent
      end

and resolve_parent_module_type_path ident tbl p : parent_module_type_path =
    match p with
    | `Resolved r -> Resolved(r, CTbl.resolved_module_type_path tbl r)
    | `Dot(pr, name) -> begin
        match resolve_parent_module_path ident tbl pr with
        | Unresolved pr -> Unresolved(`Dot(pr, name))
        | Resolved(pr, parent) ->
            let resolved pr (`ModuleType md : Parent.module_type) =
              (Resolved(`ModuleType(pr, ModuleTypeName.of_string name), md) : parent_module_type_path)
            in
            let unresolved pr =
              Unresolved(`Dot(`Resolved pr, name))
            in
              find_with_path_substs
                (Sig.find_parent_module_type name)
                resolved unresolved ident tbl pr parent
      end

and resolve_module_path ident tbl : Path.Module.t -> Path.Module.t =
  function
  | `Root s as p -> begin
      match CTbl.base tbl s with
      | CTbl.Not_found -> p
      | CTbl.Forward_reference -> `Forward s
      | CTbl.Found { root ; hidden } ->
        let p = `Identifier (`Root(root, UnitName.of_string s)) in
        `Resolved (if hidden then `Hidden p else p)
    end
  | `Forward s as p -> begin
      match CTbl.base tbl s with
      | CTbl.Not_found -> p
      | CTbl.Forward_reference -> `Forward s
      | CTbl.Found { root ; hidden } ->
        let p = `Identifier (`Root(root, UnitName.of_string s)) in
        `Resolved (if hidden then `Hidden p else p)
    end
  | `Resolved r as p ->
    let r' = resolve_resolved_path ident tbl (r :> Path.Resolved.t) |> Path.Resolved.module_of_t in
    if r != r' then `Resolved r' else p
  | `Dot(p, name) -> begin
      match resolve_parent_module_path ident tbl p with
      | Unresolved p -> `Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p (elem : Element.signature_module) =
            let `Module {Element.canonical; hidden} = elem in
            let pr = `Module(p, ModuleName.of_string name) in
            let pr = if hidden then `Hidden pr else pr in
            let pr =
              match canonical with
              | None -> pr
              | Some (p, _) -> `Canonical(pr, p)
            in
            `Resolved pr
          in
          let unresolved p =
            `Dot(`Resolved p, name)
          in
            find_with_path_substs
              (Sig.find_module_element name)
              resolved unresolved ident tbl p parent
    end
  | `Apply(p, arg) -> begin
      let arg = resolve_module_path ident tbl arg in
        match resolve_parent_module_path ident tbl p with
        | Unresolved p -> `Apply(p, arg)
        | Resolved(p, parent) ->
          let resolved p (elem : Element.signature_module) =
            let `Module {Element.canonical; hidden} = elem in
            let pr = `Apply (p, arg) in
            let pr = if hidden then `Hidden pr else pr in
            let pr =
              match canonical with
              | None -> pr
              | Some (p, _) -> `Canonical(pr, p)
            in
            `Resolved pr
          in
          let unresolved p =
            `Apply(`Resolved p, arg)
          in
            find_with_path_substs
              Sig.find_apply_element
              resolved unresolved ident tbl p parent
    end

and resolve_resolved_module_type_path_no_id : _ -> _ ->
  Paths_types.Resolved_path.module_type_no_id ->
  Paths_types.Resolved_path.module_type =
  fun ident tbl p ->
  match p with
  | `ModuleType(parent, name) ->
    let parent' = resolve_resolved_path ident tbl (parent :> Path.Resolved.t) |> Path.Resolved.module_of_t in
    if parent != parent' then
      `ModuleType(parent', name)
    else (p :> Path.Resolved.ModuleType.t)

and resolve_resolved_module_type_path : _ -> _ -> Path.Resolved.ModuleType.t -> Path.Resolved.ModuleType.t =
  fun ident tbl p ->
  match p with
  | `Identifier _ -> p
  | `ModuleType(_parent, _name) as p' -> resolve_resolved_module_type_path_no_id ident tbl p'

and resolve_resolved_module_path_no_id : _ -> _ -> Paths_types.Resolved_path.module_no_id -> Paths_types.Resolved_path.module_ =
  fun ident tbl p ->
  match p with
  | `Subst(sub, orig) ->
    let sub' = resolve_resolved_module_type_path ident tbl sub in
    let orig' = resolve_resolved_module_path ident tbl orig in
    if sub != sub' || orig != orig' then `Subst(sub', orig')
    else (p :> Path.Resolved.Module.t)
  | `SubstAlias(sub, orig) ->
    let sub'  = resolve_resolved_module_path ident tbl sub in 
    let orig' = resolve_resolved_module_path ident tbl orig in 
    if sub != sub' || orig != orig' then `SubstAlias(sub', orig')
    else (p :> Path.Resolved.Module.t)
  | `Hidden hp ->
    let hp'  = resolve_resolved_module_path ident tbl hp in
    if hp != hp' then `Hidden hp'
    else (p :> Path.Resolved.Module.t)
  | `Module(parent, name) ->
    let parent' = resolve_resolved_module_path ident tbl parent in
    if parent != parent' then
      `Module(parent', name)
    else (p :> Path.Resolved.Module.t)
  | `Canonical(_, _) as p ->
    resolve_canonical_path_module ident tbl p
  | `Apply(fn, arg) ->
    let fn' = resolve_resolved_module_path ident tbl fn in
    if fn != fn' then `Apply(fn', arg)
    else (p :> Path.Resolved.Module.t)

and resolve_resolved_module_path : _ -> _ ->
  Path.Resolved.Module.t -> Path.Resolved.Module.t =
  fun ident tbl p ->
  match p with
  | `Identifier _ -> p
  | `Subst(_,_)
  | `SubstAlias(_,_)
  | `Hidden(_)
  | `Module(_,_)
  | `Canonical(_,_)
  | `Apply(_,_) as p' -> resolve_resolved_module_path_no_id ident tbl p'

and resolve_resolved_type_class_path_no_id : _ -> _ ->
  Paths_types.Resolved_path.class_type_no_id -> Path.Resolved.ClassType.t =
  fun ident tbl p ->
  match p with
  | `Class(parent, name) ->
    let parent' = resolve_resolved_module_path ident tbl parent in
    if parent != parent' then `Class(parent', name)
    else (p :> Path.Resolved.ClassType.t)
  | `ClassType(parent, name) ->
    let parent' = resolve_resolved_module_path ident tbl parent in
    if parent != parent' then `ClassType(parent', name)
    else (p :> Path.Resolved.ClassType.t)

and resolve_resolved_type_class_path : _ -> _ ->
  Path.Resolved.ClassType.t -> Path.Resolved.ClassType.t =
  fun ident tbl p ->
  match p with
  | `Identifier _ -> p
  | `Class _ | `ClassType _ as p' -> resolve_resolved_type_class_path_no_id ident tbl p'

and resolve_resolved_type_path_no_id : _ -> _ ->
  Paths_types.Resolved_path.type_no_id -> Path.Resolved.Type.t =
  fun ident tbl p ->
  match p with
  | `Type(parent, name) ->
    let parent' = resolve_resolved_module_path ident tbl parent in
    if parent != parent' then `Type(parent', name)
    else (p :> Path.Resolved.Type.t)
  | `Class _ | `ClassType _ as p' ->
    (resolve_resolved_type_class_path_no_id ident tbl p' :> Path.Resolved.Type.t)

and resolve_resolved_type_path : _ -> _ ->
  Path.Resolved.Type.t -> Path.Resolved.Type.t =
  fun ident tbl p ->
  match p with
  | `Identifier _ -> p
  | `Type _ | `Class _ | `ClassType _ as p' -> resolve_resolved_type_path_no_id ident tbl p'

and resolve_resolved_path :
  _ -> _ -> Path.Resolved.t -> Path.Resolved.t =
  fun ident tbl p ->
  match p with
  | `Identifier _ -> p
  | `Subst _ | `SubstAlias _ | `Hidden _
  | `Module _ | `Canonical _ | `Apply _ as p' ->
    (resolve_resolved_module_path_no_id ident tbl p' :> Path.Resolved.t)
  | `ModuleType _ as p' ->
    (resolve_resolved_module_type_path_no_id ident tbl p' :> Path.Resolved.t)
  | `Type _ | `Class _ | `ClassType _ as p' ->
    (resolve_resolved_type_path_no_id ident tbl p' :> Path.Resolved.t)

and resolve_path_module_type ident tbl (p : Path.ModuleType.t) =
  match p with
  | `Resolved r ->
    let r' = resolve_resolved_module_type_path ident tbl r in
    if r != r' then `Resolved r' else p
  | `Dot(p, name) -> begin
      match resolve_parent_module_path ident tbl p with
      | Unresolved p -> `Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p (`ModuleType : Element.signature_module_type) =
            `Resolved (`ModuleType(p, ModuleTypeName.of_string name))
          in
          let unresolved p =
            `Dot(`Resolved p, name)
          in
            find_with_path_substs
              (Sig.find_module_type_element name)
              resolved unresolved ident tbl p parent
    end

and resolve_type_path ident tbl (p : Path.Type.t) =
  match p with
  | `Resolved r ->
    let r' = resolve_resolved_type_path ident tbl r in
    if r != r' then `Resolved r' else p
  | `Dot(p, name) -> begin
      match resolve_parent_module_path ident tbl p with
      | Unresolved p -> `Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p : Element.signature_type -> _ = function
            | `Type -> `Resolved (`Type(p, TypeName.of_string name))
            | `Class -> `Resolved (`Class(p, ClassName.of_string name))
            | `ClassType -> `Resolved (`ClassType(p, ClassTypeName.of_string name))
          in
          let unresolved p =
            `Dot(`Resolved p, name)
          in
            find_with_path_substs
              (Sig.find_type_element name)
              resolved unresolved ident tbl p parent
    end

and resolve_class_type_path ident tbl (p : Path.ClassType.t) =
  match p with
  | `Resolved r ->
    let r' = resolve_resolved_type_class_path ident tbl r in
    if r != r' then `Resolved r' else p
  | `Dot(p, name) -> begin
      match resolve_parent_module_path ident tbl p with
      | Unresolved p -> `Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p : Element.signature_class_type -> _ = function
            | `Class -> `Resolved (`Class(p, ClassName.of_string name))
            | `ClassType -> `Resolved (`ClassType(p, ClassTypeName.of_string name))
          in
          let unresolved p =
            `Dot(`Resolved p, name)
          in
            find_with_path_substs
              (Sig.find_class_type_element name)
              resolved unresolved ident tbl p parent
    end


type parent_fragment =
  | Resolved of Fragment.Resolved.Signature.t * Sig.t
  | Unresolved of Fragment.Signature.t

let rec find_with_fragment_substs
        : 'b 'c. (Sig.t -> 'b) ->
                   (Fragment.Resolved.Signature.t -> 'b -> 'c) ->
                     (Fragment.Resolved.Signature.t -> 'c) ->
                       _ -> CTbl.t ->
                         Fragment.Resolved.Signature.t -> Sig.t -> 'c =
  fun find resolved unresolved ident tbl pr parent ->
    try
      resolved pr (find parent)
    with Not_found ->
      match pr with
      | `Subst _ | `SubstAlias _ | `Module _ as pr -> begin
          try
            match Sig.find_parent_subst parent with
            | Parent.Subst subp -> begin
                match resolve_parent_module_type_path ident tbl subp with
                | Unresolved _ -> unresolved pr
                | Resolved(subpr, parent) ->
                  find_with_fragment_substs
                    find resolved unresolved ident tbl
                    (`Subst(subpr, pr)) parent
              end
            | Parent.SubstAlias subp -> begin
                match resolve_parent_module_path ident tbl subp with
                | Unresolved _ -> unresolved pr
                | Resolved(subpr, parent) ->
                  find_with_fragment_substs
                    find resolved unresolved ident tbl
                    (`SubstAlias(subpr, pr)) parent
              end
          with Not_found -> unresolved pr
        end
      | _ -> unresolved pr (* we can find substs only for modules. *)

let rec resolve_parent_fragment ident tbl base (p : Fragment.Signature.t) : parent_fragment =
    match p with
    | `Resolved r ->
          Resolved(r, CTbl.resolved_signature_fragment base r)
    | `Dot(pr, name) -> begin
        match resolve_parent_fragment ident tbl base pr with
        | Unresolved pr -> Unresolved(`Dot(pr, name))
        | Resolved(pr, parent) ->
            let resolved pr (`Module md : Parent.module_) =
              (Resolved(`Module(pr, ModuleName.of_string name), md) : parent_fragment)
            in
            let unresolved pr =
              (Unresolved(`Dot(`Resolved pr, name)) : parent_fragment)
            in
              find_with_fragment_substs
                (Sig.find_parent_module name)
                resolved unresolved ident tbl pr parent
      end

and resolve_module_fragment ident tbl base (p : Fragment.Module.t) =
  match p with
  | `Resolved _ as p -> p
  | `Dot(p, name) -> begin
      match resolve_parent_fragment ident tbl base p with
      | Unresolved p -> `Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p (`Module _ : Element.signature_module) =
            (* Handle canonical path here? *)
            `Resolved (`Module(p, ModuleName.of_string name))
          in
          let unresolved p =
            `Dot(`Resolved p, name)
          in
            find_with_fragment_substs
              (Sig.find_module_element name)
              resolved unresolved ident tbl p parent
    end

and resolve_type_fragment ident tbl base (p : Fragment.Type.t) =
  match p with
  | `Resolved _ -> p
  | `Dot(p, name) -> begin
      match resolve_parent_fragment ident tbl base p with
      | Unresolved p -> `Dot(p, name)
      | Resolved(p, parent) ->
          let resolved p : Element.signature_type -> _ = function
            | `Type -> `Resolved (`Type(p, TypeName.of_string name))
            | `Class -> `Resolved (`Class(p, ClassName.of_string name))
            | `ClassType -> `Resolved (`ClassType(p, ClassTypeName.of_string name))
          in
          let unresolved p =
            `Dot(`Resolved p, name)
          in
            find_with_fragment_substs
              (Sig.find_type_element name)
              resolved unresolved ident tbl p parent
    end

type parent_reference =
  | ResolvedSig of Reference.Resolved.Signature.t * Sig.t
  | ResolvedDatatype of Reference.Resolved.DataType.t * Datatype.t
  | ResolvedClassSig of Reference.Resolved.ClassSignature.t * ClassSig.t
  | ResolvedPage of Reference.Resolved.Page.t * Page.t
  | Unresolved of Reference.LabelParent.t

type parent_kind =
  | PParent
  | PSig
  | PClassSig
  | PSigOrType
  | PLabelParent

let find_parent_reference (kind : parent_kind) r name parent
    : parent_reference =
    match kind with
    | PParent -> begin
        match Sig.find_parent name parent with
        | `Module md ->
          let pr = `Module (r, ModuleName.of_string name) in
          let pr =
            match Sig.get_canonical md with
            | None -> pr
            | Some (_, r) -> `Canonical(pr, r)
          in
          ResolvedSig(pr, md)
        | `ModuleType md -> ResolvedSig(`ModuleType(r, ModuleTypeName.of_string name), md)
        | `Datatype t -> ResolvedDatatype(`Type(r, TypeName.of_string name), t)
        | `Class cls -> ResolvedClassSig(`Class(r, ClassName.of_string name), cls)
        | `ClassType cls -> ResolvedClassSig(`ClassType(r, ClassTypeName.of_string name), cls)
      end
    | PSig -> begin
        match Sig.find_parent_signature name parent with
        | `Module md ->
          let pr = `Module (r, ModuleName.of_string name) in
          let pr =
            match Sig.get_canonical md with
            | None -> pr
            | Some (_, r) -> `Canonical(pr, r)
          in
          ResolvedSig(pr, md)
        | `ModuleType md -> ResolvedSig(`ModuleType(r, ModuleTypeName.of_string name), md)
      end
    | PClassSig -> begin
        match Sig.find_parent_class_signature name parent with
        | `Class cls -> ResolvedClassSig(`Class(r, ClassName.of_string name), cls)
        | `ClassType cls -> ResolvedClassSig(`ClassType(r, ClassTypeName.of_string name), cls)
      end
    | PSigOrType -> begin
        match Sig.find_parent_sig_or_type name parent with
        | `Module md ->
          let pr = `Module (r, ModuleName.of_string name) in
          let pr =
            match Sig.get_canonical md with
            | None -> pr
            | Some (_, r) -> `Canonical(pr, r)
          in
          ResolvedSig(pr, md)
        | `ModuleType md -> ResolvedSig(`ModuleType(r, ModuleTypeName.of_string name), md)
        | `Datatype t -> ResolvedDatatype(`Type(r, TypeName.of_string name), t)
      end
    | PLabelParent ->  begin
        match Sig.find_parent name parent with
        | `Module md ->
          let pr = `Module (r, ModuleName.of_string name) in
          let pr =
            match Sig.get_canonical md with
            | None -> pr
            | Some (_, r) -> `Canonical(pr, r)
          in
          ResolvedSig(pr, md)
        | `ModuleType md -> ResolvedSig(`ModuleType(r, ModuleTypeName.of_string name), md)
        | `Datatype t -> ResolvedDatatype(`Type(r, TypeName.of_string name), t)
        | `Class cls -> ResolvedClassSig(`Class(r, ClassName.of_string name), cls)
        | `ClassType cls -> ResolvedClassSig(`ClassType(r, ClassTypeName.of_string name), cls)
      end

let rec find_with_reference_substs
   : 'b 'c. (Sig.t -> 'b)
  -> (Reference.Resolved.Signature.t -> 'b -> 'c)
  -> (Reference.Resolved.Signature.t -> 'c)
  -> Identifier.Signature.t option -> CTbl.t
  -> Reference.Resolved.Signature.t -> Sig.t
  -> 'c
  = fun find resolved unresolved ident tbl pr parent ->
    try resolved pr (find parent)
    with Not_found ->
      match pr with
      | `Identifier (`Root _ | `Module _ | `Argument _)
      | `SubstAlias _ | `Module _ | `Canonical _ as pr -> begin
          match Sig.find_parent_subst parent with
          | Parent.SubstAlias subp -> begin
              match resolve_parent_module_path ident tbl subp with
              | Unresolved _ -> unresolved pr
              | Resolved(subpr, parent) ->
                find_with_reference_substs find resolved unresolved ident tbl
                  (`SubstAlias(subpr, pr)) parent
            end
          | _ -> unresolved pr
          | exception Not_found -> unresolved pr
        end
      | _ -> unresolved pr (* we can find substs only for modules *)

let rec resolve_parent_reference :
  parent_kind -> Identifier.Signature.t option -> CTbl.t ->
       Reference.Parent.t -> parent_reference =
    fun kind ident tbl r ->
        match r with
        | `Root (s, _) -> begin
            match CTbl.base tbl (UnitName.to_string s) with
            | CTbl.Not_found -> Unresolved (lpop r)
            | CTbl.Forward_reference ->
                (* TODO: fail? *)
                Unresolved (lpop r)
            | CTbl.Found {root;_} ->
                let root = `Identifier (`Root(root, s)) in
                  match kind with
                  | PParent ->
                      ResolvedSig(root, CTbl.resolved_signature_reference tbl root)
                  | PSig ->
                      ResolvedSig(root, CTbl.resolved_signature_reference tbl root)
                  | PSigOrType ->
                      ResolvedSig(root, CTbl.resolved_signature_reference tbl root)
                  | PLabelParent ->
                      ResolvedSig(root, CTbl.resolved_signature_reference tbl root)
                  | _ -> Unresolved (lpop r)
          end
        | `Resolved
            (`Identifier (`Root _ | `Module _ | `Argument _ | `ModuleType _)
             | `SubstAlias _ | `Module _ | `Canonical _ | `ModuleType _ as rr) -> begin
            match kind with
            | PParent ->
                ResolvedSig(rr, CTbl.resolved_signature_reference tbl rr)
            | PSig ->
                ResolvedSig(rr, CTbl.resolved_signature_reference tbl rr)
            | PSigOrType ->
                ResolvedSig(rr, CTbl.resolved_signature_reference tbl rr)
            | PLabelParent ->
                ResolvedSig(rr, CTbl.resolved_signature_reference tbl rr)
            | _ -> Unresolved (lpop r)
          end
        | `Resolved
            (`Identifier (`Class _ | `ClassType _)
            | `Class _ | `ClassType _ as rr) -> begin
            match kind with
            | PParent ->
                ResolvedClassSig(rr, CTbl.resolved_class_signature_reference tbl rr)
            | PClassSig ->
                ResolvedClassSig(rr, CTbl.resolved_class_signature_reference tbl rr)
            | PLabelParent ->
                ResolvedClassSig(rr, CTbl.resolved_class_signature_reference tbl rr)
            | _ -> Unresolved (lpop r)
          end
        | `Resolved (`Identifier (`Type _ | `CoreType _) | `Type _ as rr) -> begin
            match kind with
            | PParent ->
                ResolvedDatatype(rr, CTbl.resolved_datatype_reference tbl rr)
            | PSigOrType ->
                ResolvedDatatype(rr, CTbl.resolved_datatype_reference tbl rr)
            | PLabelParent ->
                ResolvedDatatype(rr, CTbl.resolved_datatype_reference tbl rr)
            | _ -> Unresolved (lpop r)
          end
        | `Dot(pr, name) -> begin
            match resolve_label_parent_reference PParent ident tbl pr with
            | Unresolved _ -> Unresolved (lpop r)
            | ResolvedSig(rr, parent) ->
              let resolved _ pr = pr in
              let unresolved pr =
                Unresolved(
                  `Dot(`Resolved (pr :> Reference.Resolved.LabelParent.t), name)
                )
              in
              find_with_reference_substs (find_parent_reference kind rr name)
                resolved unresolved ident tbl rr parent
            | _ ->
              Unresolved (lpop r)
          end
        | `Module(pr, name) -> begin
            match
              resolve_parent_reference PSig ident tbl
                (pr : Reference.Signature.t :> Reference.Parent.t)
            with
            | Unresolved _ -> Unresolved (lpop r)
            | ResolvedSig(rr, parent) ->
              let resolved _ pr = pr in
              let unresolved pr =
                Unresolved(
                  `Module(`Resolved pr, name)
                )
              in
              find_with_reference_substs (find_parent_reference kind rr (ModuleName.to_string name))
                resolved unresolved ident tbl rr parent
            | _ -> assert false
          end
        | `ModuleType(pr, name) -> begin
            match
              resolve_parent_reference PSig ident tbl
                (pr : Reference.Signature.t :> Reference.Parent.t)
            with
            | Unresolved _ -> Unresolved (lpop r)
            | ResolvedSig(rr, parent) ->
              let resolved _ pr = pr in
              let unresolved pr =
                Unresolved(
                  `ModuleType(`Resolved pr, name)
                )
              in
              find_with_reference_substs (find_parent_reference kind rr (ModuleTypeName.to_string name))
                resolved unresolved ident tbl rr parent
            | _ -> assert false
          end
        | `Type(pr, name) -> begin
            match
              resolve_parent_reference PSig ident tbl
                (pr : Reference.Signature.t :> Reference.Parent.t)
            with
            | Unresolved _ -> Unresolved (lpop r)
            | ResolvedSig(rr, parent) ->
              let resolved _ pr = pr in
              let unresolved pr =
                Unresolved(
                  `Type(`Resolved pr, name)
                )
              in
              find_with_reference_substs (find_parent_reference kind rr (TypeName.to_string name))
                resolved unresolved ident tbl rr parent
            | _ -> assert false
          end
        | `Class(pr, name) -> begin
            match
              resolve_parent_reference PSig ident tbl
                (pr : Reference.Signature.t :> Reference.Parent.t)
            with
            | Unresolved _ -> Unresolved (lpop r)
            | ResolvedSig(rr, parent) ->
              let resolved _ pr = pr in
              let unresolved pr =
                Unresolved(
                  `Class(`Resolved pr, name)
                )
              in
              find_with_reference_substs (find_parent_reference kind rr (ClassName.to_string name))
                resolved unresolved ident tbl rr parent
            | _ -> assert false
          end
        | `ClassType(pr, name) -> begin
            match
              resolve_parent_reference PSig ident tbl
                (pr : Reference.Signature.t :> Reference.Parent.t)
            with
            | Unresolved _ -> Unresolved (lpop r)
            | ResolvedSig(rr, parent) ->
              let resolved _ pr = pr in
              let unresolved pr =
                Unresolved(
                  `ClassType(`Resolved pr, name)
                )
              in
              find_with_reference_substs (find_parent_reference kind rr (ClassTypeName.to_string name))
                resolved unresolved ident tbl rr parent
            | _ -> assert false
          end

and resolve_label_parent_reference :
  parent_kind -> Identifier.Signature.t option -> CTbl.t ->
       Reference.LabelParent.t -> parent_reference =
  fun kind ident tbl r ->
    let handle_root s : parent_reference =
      match CTbl.page_base tbl s with
      | None -> Unresolved r
      | Some root ->
        let root = `Identifier (`Page(root, PageName.of_string s)) in
        match kind with
        | PLabelParent ->
          ResolvedPage(root, CTbl.resolved_page_reference tbl root)
        | _ -> Unresolved r
    in
      match r with
      | `Root (s, `TPage) -> handle_root (UnitName.to_string s)
      | `Root (s, `TUnknown) as r -> begin
          match resolve_parent_reference kind ident tbl r with
          | Unresolved _ -> handle_root (UnitName.to_string s)
          | otherwise -> otherwise
        end
      | `Resolved (`Identifier `Page _ as root) -> begin
          match kind with
          | PLabelParent ->
            ResolvedPage(root, CTbl.resolved_page_reference tbl root)
          | _ -> Unresolved r
        end
      | `Root (_, (`TModule | `TModuleType | `TType | `TClass | `TClassType))
      | `Resolved (`Identifier ( `Root _ | `Module _
                             | `Argument _ | `ModuleType _
                             | `Type _ | `CoreType _
                             | `Class _ | `ClassType _)
                 | `SubstAlias _ | `Module _ | `Canonical _
                 | `ModuleType _ | `Type _ | `Class _ | `ClassType _)
      | `Dot _ | `Module _ | `ModuleType _ | `Type _ | `Class _ | `ClassType _ as r ->
        resolve_parent_reference kind ident tbl r

and resolve_resolved_reference_signature_no_id : _ -> _ ->
  Paths_types.Resolved_reference.signature_no_id -> Reference.Resolved.Signature.t =
  fun ident tbl r ->
    match r with
    | `SubstAlias _
    | `Module _ 
    | `Canonical _ as r' ->
      (resolve_resolved_reference_module_no_id ident tbl r' :> Reference.Resolved.Signature.t)
    | `ModuleType(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `ModuleType(parent', name)
      else (r :> Reference.Resolved.Signature.t)

and resolve_resolved_reference_signature : _ -> _ ->
  Reference.Resolved.Signature.t -> Reference.Resolved.Signature.t =
  fun ident tbl r ->
    match r with
    | `Identifier _ -> r
    | `SubstAlias _ | `Module _ | `Canonical _ | `ModuleType _ as r' ->
      resolve_resolved_reference_signature_no_id ident tbl r' 

and resolve_resolved_reference_module_no_id : _ -> _ ->
  Paths_types.Resolved_reference.module_no_id -> Reference.Resolved.Module.t =
  fun ident tbl r ->
    match r with
    | `SubstAlias(sub, orig) ->
      let sub' = resolve_resolved_module_path ident tbl sub in
      let orig' = resolve_resolved_reference_module ident tbl orig in
      if sub != sub' || orig != orig' then
        `SubstAlias(sub', orig')
      else (r :> Reference.Resolved.Module.t)
    | `Module(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `Module(parent', name)
      else (r :> Reference.Resolved.Module.t)
    | `Canonical(orig, cano) ->
      let orig' = resolve_resolved_reference_module ident tbl orig in
      let cano' = resolve_module_reference ident tbl cano in
      if orig != orig' || cano != cano' then
        let cano' =
          match ident, cano' with
          | Some ident, `Resolved rp ->
              let rp' = Reference.Resolved.Module.rebase ident rp in
              if rp != rp' then `Resolved rp' else cano'
          | _, _ -> cano'
        in
        `Canonical(orig', cano')
      else (r :> Reference.Resolved.Module.t)

and resolve_resolved_reference_module : _ -> _ ->
  Reference.Resolved.Module.t -> Reference.Resolved.Module.t =
  fun ident tbl r ->
    match r with
    | `Identifier _ -> r
    | `SubstAlias _ | `Module _ | `Canonical _ as r' ->
      resolve_resolved_reference_module_no_id ident tbl r' 

and resolve_resolved_reference_class_signature_no_id : _ -> _ ->
  Paths_types.Resolved_reference.class_signature_no_id -> Reference.Resolved.ClassSignature.t =
  fun ident tbl r ->
    match r with
    | `Class(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `Class(parent', name)
      else (r :> Reference.Resolved.ClassSignature.t)
    | `ClassType(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `ClassType(parent', name)
      else (r :> Reference.Resolved.ClassSignature.t)

and resolve_resolved_reference_class_signature : _ -> _ ->
  Reference.Resolved.ClassSignature.t -> Reference.Resolved.ClassSignature.t =
  fun ident tbl r ->
    match r with
    | `Identifier _ -> r
    | `Class _ | `ClassType _ as r' ->
      resolve_resolved_reference_class_signature_no_id ident tbl r' 

and resolve_resolved_reference_datatype_no_id : _ -> _ ->
  Paths_types.Resolved_reference.datatype_no_id -> Reference.Resolved.DataType.t =
  fun ident tbl r ->
    match r with
    | `Type(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `Type(parent', name)
      else (r :> Reference.Resolved.DataType.t)

and resolve_resolved_reference_datatype : _ -> _ ->
  Reference.Resolved.DataType.t -> Reference.Resolved.DataType.t =
  fun ident tbl r ->
    match r with
    | `Identifier _ -> r
    | `Type _ as r' ->
      resolve_resolved_reference_datatype_no_id ident tbl r'

and resolve_resolved_reference_parent_no_id : _ -> _ ->
  Paths_types.Resolved_reference.parent_no_id -> Reference.Resolved.Parent.t =
  fun ident tbl r ->
    match r with
    | `SubstAlias _ | `Module _ | `Canonical _ | `ModuleType _ as r' ->
      (resolve_resolved_reference_signature_no_id ident tbl r' :> Reference.Resolved.Parent.t)
    | `Class _ | `ClassType _ as r' ->
      (resolve_resolved_reference_class_signature_no_id ident tbl r' :> Reference.Resolved.Parent.t)
    | `Type _ as r' ->
      (resolve_resolved_reference_datatype_no_id ident tbl r' :> Reference.Resolved.Parent.t)

and resolve_resolved_reference_parent : _ -> _ ->
  Reference.Resolved.Parent.t -> Reference.Resolved.Parent.t =
  fun ident tbl r ->
    match r with
    | `Identifier _ -> r
    | `SubstAlias _ | `Module _ | `Canonical _ | `ModuleType _ 
    | `Class _ | `ClassType _ | `Type _  as r' ->
      resolve_resolved_reference_parent_no_id ident tbl r'

and resolve_resolved_reference_label_parent : _ -> _ ->
  Reference.Resolved.LabelParent.t -> Reference.Resolved.LabelParent.t =
  fun ident tbl r ->
    match r with
    | `Identifier _ -> r
    | `SubstAlias _ | `Module _ | `Canonical _ | `ModuleType _ 
    | `Class _ | `ClassType _ | `Type _  as r' ->
      (resolve_resolved_reference_parent_no_id ident tbl r' :> Reference.Resolved.LabelParent.t)

and resolve_resolved_reference_type : _ -> _ ->
  Reference.Resolved.Type.t -> Reference.Resolved.Type.t =
  fun ident tbl r ->
    match r with
    | `Identifier _ -> r
    | `Type _ as r' ->
      (resolve_resolved_reference_datatype_no_id ident tbl r' :> Reference.Resolved.Type.t)
    | `Class _ | `ClassType _ as r' ->
      (resolve_resolved_reference_class_signature_no_id ident tbl r' :> Reference.Resolved.Type.t)

and resolve_resolved_reference_module_type : _ -> _ ->
  Reference.Resolved.ModuleType.t -> Reference.Resolved.ModuleType.t =
  fun ident tbl r ->
    match r with
    | `ModuleType(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `ModuleType(parent', name)
      else (r :> Reference.Resolved.ModuleType.t)
    | `Identifier _ -> r

and resolve_resolved_reference_constructor : _ -> _ ->
  Reference.Resolved.Constructor.t -> Reference.Resolved.Constructor.t =
  fun ident tbl r ->
    match r with
    | `Extension(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `Extension(parent', name)
      else r
    | `Exception(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `Exception(parent', name)
      else r
    | `Constructor(parent, name) ->
      let parent' = resolve_resolved_reference_datatype ident tbl parent in
      if parent != parent' then
        `Constructor(parent', name)
      else (r :> Reference.Resolved.Constructor.t)
    | `Identifier _ -> r

and resolve_resolved_reference_field : _ -> _ ->
  Reference.Resolved.Field.t -> Reference.Resolved.Field.t =
  fun ident tbl r ->
    match r with
    | `Field(parent, name) ->
      let parent' = resolve_resolved_reference_parent ident tbl parent in
      if parent != parent' then
        `Field(parent', name)
      else r
    | `Identifier _ -> r

and resolve_resolved_reference_extension : _ -> _ ->
  Reference.Resolved.Extension.t -> Reference.Resolved.Extension.t =
  fun ident tbl r ->
    match r with
    | `Extension(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `Extension(parent', name)
      else r
    | `Exception(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `Exception(parent', name)
      else r
    | `Identifier _ -> r

and resolve_resolved_reference_exception : _ -> _ ->
  Reference.Resolved.Exception.t -> Reference.Resolved.Exception.t =
  fun ident tbl r ->
    match r with
    | `Exception(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `Exception(parent', name)
      else r
    | `Identifier _ -> r

and resolve_resolved_reference_value : _ -> _ ->
  Reference.Resolved.Value.t -> Reference.Resolved.Value.t =
  fun ident tbl r ->
    match r with
    | `Value(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `Value(parent', name)
      else r
    | `Identifier _ -> r

and resolve_resolved_reference_class : _ -> _ ->
  Reference.Resolved.Class.t -> Reference.Resolved.Class.t =
  fun ident tbl r ->
    match r with
    | `Class(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `Class(parent', name)
      else (r :> Reference.Resolved.Class.t)
    | `Identifier _ -> r

and resolve_resolved_reference_class_type : _ -> _ ->
  Reference.Resolved.ClassType.t -> Reference.Resolved.ClassType.t =
  fun ident tbl r ->
    match r with
    | `Class _ | `ClassType _ as r'->
      (resolve_resolved_reference_class_signature_no_id ident tbl r' :> Reference.Resolved.ClassType.t)
    | `Identifier _ -> r

and resolve_resolved_reference_method : _ -> _ ->
  Reference.Resolved.Method.t -> Reference.Resolved.Method.t =
  fun ident tbl r ->
    match r with
    | `Method (parent, name) ->
      let parent' = resolve_resolved_reference_class_signature ident tbl parent in
      if parent != parent' then
        `Method(parent', name)
      else r
    | `Identifier _ -> r

and resolve_resolved_reference_instance_variable : _ -> _ ->
  Reference.Resolved.InstanceVariable.t -> Reference.Resolved.InstanceVariable.t =
  fun ident tbl r ->
    match r with
    | `InstanceVariable(parent, name) ->
      let parent' = resolve_resolved_reference_class_signature ident tbl parent in
      if parent != parent' then
        `InstanceVariable(parent', name)
      else r
    | `Identifier _ -> r

and resolve_resolved_reference_label : _ -> _ ->
  Reference.Resolved.Label.t -> Reference.Resolved.Label.t =
  fun ident tbl r ->
    match r with
    | `Label(parent, name) ->
      let parent' = resolve_resolved_reference_label_parent ident tbl parent in
      if parent != parent' then
        `Label(parent', name)
      else r
    | `Identifier _ -> r

and resolve_resolved_reference :
   _ -> _ -> Reference.Resolved.t -> Reference.Resolved.t =
  fun ident tbl r ->
    match r with
    | `Identifier _ -> r
    | `SubstAlias _ | `Module _ | `Canonical _ | `ModuleType _ 
    | `Class _ | `ClassType _ | `Type _  as r' ->
      (resolve_resolved_reference_parent_no_id ident tbl r' :> Reference.Resolved.t)
    | `Constructor(parent, name) ->
      let parent' = resolve_resolved_reference_datatype ident tbl parent in
      if parent != parent' then
        `Constructor(parent', name)
      else r
    | `Field(parent, name) ->
      let parent' = resolve_resolved_reference_parent ident tbl parent in
      if parent != parent' then
        `Field(parent', name)
      else r
    | `Extension(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `Extension(parent', name)
      else r
    | `Exception(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `Exception(parent', name)
      else r
    | `Value(parent, name) ->
      let parent' = resolve_resolved_reference_signature ident tbl parent in
      if parent != parent' then
        `Value(parent', name)
      else r
    | `Method(parent, name) ->
      let parent' = resolve_resolved_reference_class_signature ident tbl parent in
      if parent != parent' then
        `Method(parent', name)
      else r
    | `InstanceVariable(parent, name) ->
      let parent' = resolve_resolved_reference_class_signature ident tbl parent in
      if parent != parent' then
        `InstanceVariable(parent', name)
      else r
    | `Label(parent, name) ->
      let parent' = resolve_resolved_reference_label_parent ident tbl parent in
      if parent != parent' then
        `Label(parent', name)
      else r

and resolve_module_reference ident tbl (r : Reference.Module.t)
  : Reference.Module.t =
    match r with
    | `Root (s, _) -> begin
        match CTbl.base tbl (UnitName.to_string s) with
        | CTbl.Not_found -> r
        | CTbl.Forward_reference -> r (* TODO *)
        | CTbl.Found {root; _} -> `Resolved (`Identifier (`Root(root, s)))
      end
    | `Resolved rr as r ->
      let rr' = resolve_resolved_reference_module ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_module) =
            let `Module {Element.canonical; hidden = _} = elem in
            let rr : Reference.Resolved.Module.t = `Module(r, ModuleName.of_string name) in
            let rr : Reference.Resolved.Module.t =
              match canonical with
              | None -> rr
              | Some (_, r) -> `Canonical(rr, r)
            in
              `Resolved rr
          in
          let unresolved r =
            let r = rlpop (r :> Reference.Resolved.Parent.t) in
              `Dot(`Resolved r, name)
          in
            find_with_reference_substs (Sig.find_module_element name)
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end
    | `Module(r, name) -> begin
        match
          resolve_parent_reference PSig ident tbl
            (r : Reference.Signature.t :> Reference.Parent.t)
        with
        | Unresolved _ -> `Module(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_module) =
            let `Module {Element.canonical; hidden = _} = elem in
            let rr : Reference.Resolved.Module.t = `Module(r, name) in
            let rr : Reference.Resolved.Module.t =
              match canonical with
              | None -> rr
              | Some (_, r) -> `Canonical(rr, r)
            in
              `Resolved rr
          in
          let unresolved r = `Module(`Resolved r, name) in
            find_with_reference_substs (Sig.find_module_element (ModuleName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false

        
      end

and resolve_module_type_reference ident tbl (r : Reference.ModuleType.t)
  : Reference.ModuleType.t =
    match r with
    | `Root _ -> r
    | `Resolved rr as r ->
      let rr' = resolve_resolved_reference_module_type ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (`ModuleType : Element.signature_module_type) =
            `Resolved(`ModuleType(r, ModuleTypeName.of_string name))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Dot(`Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_module_type_element name)
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end
    | `ModuleType(r, name) -> begin
        match
          (resolve_parent_reference PSig ident tbl (r :> Reference.Parent.t))
        with
        | Unresolved _ -> `ModuleType(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (`ModuleType : Element.signature_module_type) =
            `Resolved(`ModuleType(r, name))
          in
          let unresolved r = `ModuleType(`Resolved r, name) in
            find_with_reference_substs (Sig.find_module_type_element (ModuleTypeName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end

and resolve_type_reference ident tbl (r : Reference.Type.t)
  : Reference.Type.t =
    match r with
    | `Root _ -> r
    | `Resolved rr as r ->
      let rr' = resolve_resolved_reference_type ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | `Type -> `Resolved (`Type(r, TypeName.of_string name))
            | `Class -> `Resolved (`Class(r, ClassName.of_string name))
            | `ClassType -> `Resolved (`ClassType(r, ClassTypeName.of_string name))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Dot(`Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_type_element name)
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end
    | `Type(r, name) as typ -> begin
        match
          resolve_parent_reference PSig ident tbl (r :> Reference.Parent.t)
        with
        | Unresolved _ -> typ
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | `Type -> `Resolved (`Type(r, name))
            | `Class -> typ
            | `ClassType -> typ
          in
          let unresolved r = `Type(`Resolved r, name) in
            find_with_reference_substs (Sig.find_type_element (TypeName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end
    | `Class(r, name) as cls -> begin
        match
          resolve_parent_reference PSig ident tbl (r :> Reference.Parent.t)
        with
        | Unresolved _ -> cls
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | `Type -> cls
            | `Class -> `Resolved (`Class(r, name))
            | `ClassType -> cls
          in
          let unresolved r = `Class(`Resolved r, name) in
            find_with_reference_substs (Sig.find_type_element (ClassName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end
    | `ClassType(r, name) as cls -> begin
        match
          resolve_parent_reference PSig ident tbl (r :> Reference.Parent.t)
        with
        | Unresolved _ -> cls
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | `Type -> cls
            | `Class -> `Resolved (`Class(r, (ClassName.of_string (ClassTypeName.to_string name))))
            | `ClassType -> `Resolved (`ClassType(r, name))
          in
          let unresolved r = `ClassType(`Resolved r, name) in
            find_with_reference_substs (Sig.find_type_element (ClassTypeName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end

and resolve_constructor_reference ident tbl (r : Reference.Constructor.t)
  : Reference.Constructor.t =
    match r with
    | `Root _ -> r
    | `Resolved rr as r ->
      let rr' = resolve_resolved_reference_constructor ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PSigOrType ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | `Constructor type_name ->
              `Resolved (`Constructor(`Type(r, TypeName.of_string type_name), ConstructorName.of_string name))
            | `Extension -> `Resolved (`Extension(r, ExtensionName.of_string name))
            | `Exception -> `Resolved (`Exception(r, ExceptionName.of_string name))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Dot(`Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_constructor_element name)
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            (* Not calling [find_with_reference_substs] here since the parent is
               not a Sig, i.e. there won't be any subst. *)
            try
              let `Constructor _ =
                Datatype.find_constructor_element name parent
              in
                `Resolved (`Constructor(r, ConstructorName.of_string name))
            with Not_found ->
              let r = (r :> Reference.Resolved.Parent.t) in
                `Dot(`Resolved (rlpop r), name)
          end
        | _ -> assert false
      end
    | `Constructor(r, name) -> begin
        match
          resolve_parent_reference PSigOrType ident tbl
            (r :> Reference.Parent.t)
        with
        | Unresolved _ -> `Constructor(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | `Constructor type_name ->
              `Resolved (`Constructor(`Type(r, TypeName.of_string type_name), name))
            | `Extension -> `Resolved (`Extension(r, (ExtensionName.of_string (ConstructorName.to_string name))))
            | `Exception -> `Resolved (`Exception(r, (ExceptionName.of_string (ConstructorName.to_string name))))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Dot(`Resolved (rlpop r), ConstructorName.to_string name)
          in
            find_with_reference_substs (Sig.find_constructor_element (ConstructorName.to_string name))
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            (* Not calling [find_with_reference_substs] here since the parent is
               not a Sig, i.e. there won't be any subst. *)
            try
              let `Constructor _ =
                Datatype.find_constructor_element (ConstructorName.to_string name) parent
              in
                `Resolved (`Constructor(r, name))
            with Not_found ->
                `Constructor(`Resolved r, name)
          end
        | _ -> assert false
      end
    | `Extension(r, name) as ext -> begin
        match
          resolve_parent_reference PSig ident tbl
            (r :> Reference.Parent.t)
        with
        | Unresolved _ -> ext
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | `Constructor _ -> ext
            | `Extension -> `Resolved (`Extension(r, name))
            | `Exception -> `Resolved (`Exception(r, (ExceptionName.of_string (ExtensionName.to_string name))))
          in
          let unresolved r =
              `Extension(`Resolved r, name)
          in
            find_with_reference_substs (Sig.find_constructor_element (ExtensionName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end
    | `Exception(r, name) as ext -> begin
        match
          resolve_parent_reference PSig ident tbl
            (r :> Reference.Parent.t)
        with
        | Unresolved _ -> ext
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | `Constructor _ -> ext
            | `Extension -> ext
            | `Exception -> `Resolved (`Exception(r, name))
          in
          let unresolved r =
              `Exception(`Resolved r, name)
          in
            find_with_reference_substs (Sig.find_constructor_element (ExceptionName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end

and resolve_field_reference ident tbl (r : Reference.Field.t)
  : Reference.Field.t =
    match r with
    | `Root _ -> r
    | `Resolved rr as r ->
      let rr' = resolve_resolved_reference_field ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PSigOrType ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (`Field type_name : Element.signature_field) =
            `Resolved (`Field(`Type(r, TypeName.of_string type_name), FieldName.of_string name))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Dot(`Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_field_element name)
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            (* Not calling [find_with_reference_substs] here since the parent is
               not a Sig, i.e. there won't be any subst. *)
            try
              let `Field _ =
                Datatype.find_field_element name parent
              in
                `Resolved (`Field((r :> Reference.Resolved.Parent.t), FieldName.of_string name))
            with Not_found ->
              let r = (r :> Reference.Resolved.Parent.t) in
                `Dot(`Resolved (rlpop r), name)
          end
        | _ -> assert false
      end
    | `Field(r, name) -> begin
        match resolve_parent_reference PSigOrType ident tbl r with
        | Unresolved _ -> `Field(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (`Field type_name : Element.signature_field) =
            `Resolved (`Field(`Type(r, TypeName.of_string type_name), name))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Field(`Resolved r, name)
          in
            find_with_reference_substs (Sig.find_field_element (FieldName.to_string name))
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            (* Not calling [find_with_reference_substs] here since the parent is
               not a Sig, i.e. there won't be any subst. *)
            try
              let `Field _ =
                Datatype.find_field_element (FieldName.to_string name) parent
              in
                `Resolved (`Field((r :> Reference.Resolved.Parent.t), name))
            with Not_found ->
              let r = (r :> Reference.Resolved.Parent.t) in
                `Field(`Resolved r, name)
          end
        | _ -> assert false
      end

and resolve_extension_reference ident tbl (r : Reference.Extension.t)
  : Reference.Extension.t =
    match r with
    | `Root _ -> r
    | `Resolved rr as r ->
      let rr' = resolve_resolved_reference_extension ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_extension) =
            match elem with
            | `Extension -> `Resolved (`Extension(r, ExtensionName.of_string name))
            | `Exception -> `Resolved (`Exception(r, ExceptionName.of_string name))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Dot(`Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_extension_element name)
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end
    | `Extension(r, name) as ext -> begin
        match
          resolve_parent_reference PSig ident tbl
            (r :> Reference.Parent.t)
        with
        | Unresolved _ -> ext
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | `Constructor _ -> ext
            | `Extension -> `Resolved (`Extension(r, name))
            | `Exception -> `Resolved (`Exception(r, (ExceptionName.of_string (ExtensionName.to_string name))))
          in
          let unresolved r =
              `Extension(`Resolved r, name)
          in
            find_with_reference_substs (Sig.find_constructor_element (ExtensionName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end
    | `Exception(r, name) as ext -> begin
        match
          resolve_parent_reference PSig ident tbl
            (r :> Reference.Parent.t)
        with
        | Unresolved _ -> ext
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | `Constructor _ -> ext
            | `Extension -> ext
            | `Exception -> `Resolved (`Exception(r, name))
          in
          let unresolved r =
              `Exception(`Resolved r, name)
          in
            find_with_reference_substs (Sig.find_constructor_element (ExceptionName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end

and resolve_exception_reference ident tbl (r : Reference.Exception.t)
  : Reference.Exception.t =
    match r with
    | `Root _ -> r
    | `Resolved rr ->
      let rr' = resolve_resolved_reference_exception ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (`Exception : Element.signature_exception) =
            `Resolved (`Exception(r, ExceptionName.of_string name))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Dot(`Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_exception_element name)
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end
    | `Exception(r, name) as ext -> begin
        match
          resolve_parent_reference PSig ident tbl
            (r :> Reference.Parent.t)
        with
        | Unresolved _ -> ext
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_constructor) =
            match elem with
            | `Constructor _ -> ext
            | `Extension -> ext
            | `Exception -> `Resolved (`Exception(r, name))
          in
          let unresolved r =
              `Exception(`Resolved r, name)
          in
            find_with_reference_substs (Sig.find_constructor_element (ExceptionName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end

and resolve_value_reference ident tbl (r : Reference.Value.t)
  : Reference.Value.t =
    match r with
    | `Root _ -> r
    | `Resolved rr ->
      let rr' = resolve_resolved_reference_value ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (`Value : Element.signature_value) =
            `Resolved (`Value(r, ValueName.of_string name))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Dot(`Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_value_element name)
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end
    | `Value(r, name) -> begin
        match
          resolve_parent_reference PSig ident tbl (r :> Reference.Parent.t)
        with
        | Unresolved _ -> `Value(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (`Value : Element.signature_value) =
            `Resolved (`Value(r, name))
          in
          let unresolved r =
            `Value(`Resolved r, name)
          in
            find_with_reference_substs (Sig.find_value_element (ValueName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end

and resolve_class_reference ident tbl (r : Reference.Class.t)
  : Reference.Class.t =
    match r with
    | `Root _ -> r
    | `Resolved rr ->
      let rr' = resolve_resolved_reference_class ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (`Class : Element.signature_class) =
            `Resolved (`Class(r, ClassName.of_string name))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Dot(`Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_class_element name)
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end
    | `Class(r, name) as cls -> begin
        match
          resolve_parent_reference PSig ident tbl (r :> Reference.Parent.t)
        with
        | Unresolved _ -> cls
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | `Type -> cls
            | `Class -> `Resolved (`Class(r, name))
            | `ClassType -> cls
          in
          let unresolved r = `Class(`Resolved r, name) in
            find_with_reference_substs (Sig.find_type_element (ClassName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end

and resolve_class_type_reference ident tbl (r : Reference.ClassType.t)
  : Reference.ClassType.t =
    match r with
    | `Root _ -> r
    | `Resolved rr ->
      let rr' = resolve_resolved_reference_class_type ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PSig ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r : Element.signature_class_type -> _ = function
            | `Class -> `Resolved (`Class(r, ClassName.of_string name))
            | `ClassType -> `Resolved (`ClassType(r, ClassTypeName.of_string name))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Dot(`Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_class_type_element name)
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end
    | `Class(r, name) as cls -> begin
        match
          resolve_parent_reference PSig ident tbl (r :> Reference.Parent.t)
        with
        | Unresolved _ -> cls
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | `Type -> cls
            | `Class -> `Resolved (`Class(r, name))
            | `ClassType -> cls
          in
          let unresolved r = `Class(`Resolved r, name) in
            find_with_reference_substs (Sig.find_type_element (ClassName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end
    | `ClassType(r, name) as cls -> begin
        match
          resolve_parent_reference PSig ident tbl (r :> Reference.Parent.t)
        with
        | Unresolved _ -> cls
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_type) =
            match elem with
            | `Type -> cls
            | `Class -> `Resolved (`Class(r, (ClassName.of_string (ClassTypeName.to_string name))))
            | `ClassType -> `Resolved (`ClassType(r, name))
          in
          let unresolved r = `ClassType(`Resolved r, name) in
            find_with_reference_substs (Sig.find_type_element (ClassTypeName.to_string name))
              resolved unresolved ident tbl r parent
        | _ -> assert false
      end

and resolve_method_reference ident tbl (r : Reference.Method.t)
  : Reference.Method.t =
    match r with
    | `Root _ -> r
    | `Resolved rr ->
      let rr' = resolve_resolved_reference_method ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PClassSig ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedClassSig(r, parent) -> begin
            try
              let `Method =
                ClassSig.find_method_element name parent
              in
                `Resolved (`Method(r, MethodName.of_string name))
            with Not_found ->
              let r = (r :> Reference.Resolved.Parent.t) in
                `Dot(`Resolved (rlpop r), name)
          end
        | _ -> assert false
      end
    | `Method(r, name) -> begin
        match
          resolve_parent_reference PClassSig ident tbl
            (r :> Reference.Parent.t)
        with
        | Unresolved _ -> `Method(r, name)
        | ResolvedClassSig(r, parent) -> begin
            try
              let `Method =
                ClassSig.find_method_element (MethodName.to_string name) parent
              in
                `Resolved (`Method(r, name))
            with Not_found ->
              `Method(`Resolved r, name)
          end
        | _ -> assert false
      end

and resolve_instance_variable_reference ident tbl
      (r : Reference.InstanceVariable.t) : Reference.InstanceVariable.t =
    match r with
    | `Root _ -> r
    | `Resolved rr ->
      let rr' = resolve_resolved_reference_instance_variable ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PClassSig ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedClassSig(r, parent) -> begin
            try
              let `InstanceVariable =
                ClassSig.find_instance_variable_element name parent
              in
                `Resolved (`InstanceVariable(r, InstanceVariableName.of_string name))
            with Not_found ->
              let r = (r :> Reference.Resolved.Parent.t) in
                `Dot(`Resolved (rlpop r), name)
          end
        | _ -> assert false
      end
    | `InstanceVariable(r, name) -> begin
        match
          resolve_parent_reference PClassSig ident tbl
            (r :> Reference.Parent.t)
        with
        | Unresolved _ -> `InstanceVariable(r, name)
        | ResolvedClassSig(r, parent) -> begin
            try
              let `InstanceVariable =
                ClassSig.find_instance_variable_element (InstanceVariableName.to_string name) parent
              in
                `Resolved (`InstanceVariable(r, name))
            with Not_found ->
              `InstanceVariable(`Resolved r, name)
          end
        | _ -> assert false
      end

and resolve_label_reference ident tbl (r : Reference.Label.t)
  : Reference.Label.t =
    match r with
    | `Root _ -> r
    | `Resolved rr ->
      let rr' = resolve_resolved_reference_label ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PLabelParent ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_label) =
            match elem with
            | `Label (Some type_name) ->
                `Resolved (`Label(`Type(r, TypeName.of_string type_name), LabelName.of_string name))
            | `Label None ->
                `Resolved (`Label((r :> Reference.Resolved.LabelParent.t), LabelName.of_string name))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Dot(`Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_label_element name)
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            let r = (r :> Reference.Resolved.Parent.t) in
              try
                let `Label _ =
                  Datatype.find_label_element name parent
                in
                  `Resolved (`Label(rlpop r, LabelName.of_string name))
              with Not_found ->
                  `Dot(`Resolved (rlpop r), name)
          end
        | ResolvedClassSig(r, parent) -> begin
            let r = (r :> Reference.Resolved.Parent.t) in
              try
                let `Label _ =
                  ClassSig.find_label_element name parent
                in
                  `Resolved (`Label((rlpop r), LabelName.of_string name))
              with Not_found ->
                  `Dot(`Resolved (rlpop r), name)
          end
        | ResolvedPage(r, page) -> begin
            match Page.find_label_element name page with
            | exception Not_found ->
              `Dot(`Resolved (r :> Reference.Resolved.LabelParent.t), name)
            | `Label None ->
              `Resolved (`Label ((r :> Reference.Resolved.LabelParent.t), LabelName.of_string name))
            | `Label (Some _) -> assert false
          end
      end
    | `Label(r, name) -> begin
        match resolve_label_parent_reference PLabelParent ident tbl r with
        | Unresolved r -> `Label(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature_label) =
            match elem with
            | `Label (Some type_name) ->
                `Resolved (`Label(`Type(r, TypeName.of_string type_name), name))
            | `Label None ->
                `Resolved (`Label((r :> Reference.Resolved.LabelParent.t), name))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Label(`Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_label_element (LabelName.to_string name))
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            let r = (r :> Reference.Resolved.Parent.t) in
              try
                let `Label _ =
                  Datatype.find_label_element (LabelName.to_string name) parent
                in
                  `Resolved (`Label(rlpop r, name))
              with Not_found ->
                  `Label(`Resolved (rlpop r), name)
          end
        | ResolvedClassSig(r, parent) -> begin
            let r = (r :> Reference.Resolved.Parent.t) in
              try
                let `Label _ =
                  ClassSig.find_label_element (LabelName.to_string name) parent
                in
                  `Resolved (`Label(rlpop r, name))
              with Not_found ->
                  `Label(`Resolved (rlpop r), name)
          end
        | ResolvedPage(r, page) -> begin
              match Page.find_label_element (LabelName.to_string name) page with
              | exception Not_found ->
                `Label(`Resolved (r :> Reference.Resolved.LabelParent.t), name)
              | `Label None ->
                `Resolved (`Label ((r :> Reference.Resolved.LabelParent.t), name))
              | `Label (Some _) -> assert false
          end
      end

and resolve_element_reference ident tbl (r : Reference.t)
  : Reference.t =
    let find_root_page s =
        match CTbl.page_base tbl s with
        | None -> r
        | Some root -> `Resolved (`Identifier (`Page(root, (PageName.of_string s))))
    in
    match r with
    | `Root (s, `TPage) -> find_root_page (UnitName.to_string s)
    | `Root (s, `TUnknown) -> begin
        match CTbl.base tbl (UnitName.to_string s) with
        | CTbl.Not_found -> find_root_page (UnitName.to_string s)
        | CTbl.Forward_reference ->
          r (* not looking for a page since [s] "exists". *)
        | CTbl.Found {root;_} -> `Resolved (`Identifier (`Root(root, s)))
      end
    | `Root (s, _) -> begin
        match CTbl.base tbl (UnitName.to_string s) with
        | CTbl.Not_found -> r
        | CTbl.Forward_reference -> r
        | CTbl.Found {root;_} -> `Resolved (`Identifier (`Root(root, s)))
      end
    | `Resolved rr ->
      let rr' = resolve_resolved_reference ident tbl rr in
      if rr != rr' then `Resolved rr' else r
    | `Dot(r, name) -> begin
        match resolve_label_parent_reference PLabelParent ident tbl r with
        | Unresolved r -> `Dot(r, name)
        | ResolvedSig(r, parent) ->
          let resolved r (elem : Element.signature) =
            match elem with
            | `Module {canonical; hidden = _} ->
              let rr = `Module(r, ModuleName.of_string name) in
              let rr =
                match canonical with
                | None -> rr
                | Some (_, r) -> `Canonical(rr, r)
              in
              `Resolved rr
            | `ModuleType -> `Resolved (`ModuleType(r, ModuleTypeName.of_string name))
            | `Type -> `Resolved (`Type(r, TypeName.of_string name))
            | `Constructor type_name ->
                `Resolved (`Constructor(`Type(r, TypeName.of_string type_name) , ConstructorName.of_string name))
            | `Field type_name ->
                `Resolved (`Field(`Type(r, TypeName.of_string type_name) , FieldName.of_string name))
            | `Extension -> `Resolved (`Extension(r, ExtensionName.of_string name))
            | `Exception -> `Resolved (`Exception(r, ExceptionName.of_string name))
            | `Value -> `Resolved (`Value(r, ValueName.of_string name))
            | `Class -> `Resolved (`Class(r, ClassName.of_string name))
            | `ClassType -> `Resolved (`ClassType(r, ClassTypeName.of_string name))
            | `Label (Some type_name) ->
                `Resolved (`Label(`Type(r, TypeName.of_string type_name), LabelName.of_string name))
            | `Label None ->
                `Resolved (`Label((r :> Reference.Resolved.LabelParent.t), LabelName.of_string name))
          in
          let unresolved r =
            let r = (r :> Reference.Resolved.Parent.t) in
              `Dot(`Resolved (rlpop r), name)
          in
            find_with_reference_substs (Sig.find_element name)
              resolved unresolved ident tbl r parent
        | ResolvedDatatype(r, parent) -> begin
            try
              match Datatype.find_element name parent with
              | `Constructor _ -> `Resolved (`Constructor(r , ConstructorName.of_string name))
              | `Field _ ->
                  `Resolved (`Field((r :> Reference.Resolved.Parent.t) , FieldName.of_string name))
              | `Label _ ->
                  `Resolved (`Label((r :> Reference.Resolved.LabelParent.t), LabelName.of_string name))
            with Not_found ->
              let r = (r :> Reference.Resolved.Parent.t) in
                `Dot(`Resolved (rlpop r), name)
          end
        | ResolvedClassSig(r, parent) -> begin
            try
              match ClassSig.find_element name parent with
              | `Method -> `Resolved (`Method(r, MethodName.of_string name))
              | `InstanceVariable -> `Resolved (`InstanceVariable(r, InstanceVariableName.of_string name))
              | `Label _ ->
                  `Resolved (`Label((r :> Reference.Resolved.LabelParent.t), LabelName.of_string name))
            with Not_found ->
              let r = (r :> Reference.Resolved.Parent.t) in
                `Dot(`Resolved (rlpop r), name)
          end
        | ResolvedPage(r, page) -> begin
              match Page.find_label_element name page with
              | exception Not_found ->
                `Dot(`Resolved (r :> Reference.Resolved.LabelParent.t), name)
              | `Label None ->
                `Resolved (`Label ((r :> Reference.Resolved.LabelParent.t), LabelName.of_string name))
              | `Label (Some _) -> assert false
          end
      end
    | `Module _ as r -> (resolve_module_reference ident tbl r :> Reference.t)
    | `ModuleType _ as r -> (resolve_module_type_reference ident tbl r :> Reference.t)
    | `Type _ as r -> (resolve_type_reference ident tbl r :> Reference.t)
    | `Constructor _ as r -> (resolve_constructor_reference ident tbl r :> Reference.t)
    | `Field _ as r -> (resolve_field_reference ident tbl r :> Reference.t)
    | `Extension _ as r -> (resolve_extension_reference ident tbl r :> Reference.t)
    | `Exception _ as r -> (resolve_exception_reference ident tbl r :> Reference.t)
    | `Value _ as r -> (resolve_value_reference ident tbl r :> Reference.t)
    | `Class _ as r -> (resolve_class_reference ident tbl r :> Reference.t)
    | `ClassType _ as r -> (resolve_class_type_reference ident tbl r :> Reference.t)
    | `Method _ as r -> (resolve_method_reference ident tbl r :> Reference.t)
    | `InstanceVariable _ as r -> (resolve_instance_variable_reference ident tbl r :> Reference.t)
    | `Label _ as r -> (resolve_label_reference ident tbl r :> Reference.t)

let splice_section_title tbl path elements =
  let title_of_parent =
    fun name parent_ref ->
      match parent_ref with
      | (`Identifier (`Root _ | `Module _ | `Argument _ | `ModuleType _)
        | `SubstAlias _ | `Module _ | `Canonical _ | `ModuleType _ as rr) ->
          Some (Sig.find_section_title name
                  (CTbl.resolved_signature_reference tbl rr))
      | `Identifier `Page _ as rr ->
        Some (Page.find_section_title name
                (CTbl.resolved_page_reference tbl rr))
      | _ -> None
  in
  let find_section_title :
    Reference.Resolved.Label.t -> Odoc_model.Comment.link_content option =
    function
    | `Identifier `Label (parent, str) ->
      let parent_ref = `Identifier parent in
      title_of_parent (LabelName.to_string str) parent_ref
    | `Label (parent_ref, str) ->
      title_of_parent (LabelName.to_string str) parent_ref
  in
  match (path, elements) with
  | (r, []) ->
    begin match r with
    | `Resolved (`Label _
                       | `Identifier (`Label _) as rr) ->
      begin match find_section_title rr with
      | None -> (path, elements)
      | Some txt -> (r, txt)
      end
    | _ -> (path, elements)
    end
  | otherwise -> otherwise

class resolver ?equal ?hash lookup_unit fetch_unit lookup_page fetch_page =
  object (self)
    val tbl =
      CTbl.create ?equal ?hash lookup_unit fetch_unit lookup_page fetch_page
    val where_am_i : Identifier.Signature.t option = None

    inherit Maps.types as super
    method root x = x

    method identifier_module x = x
    method identifier_module_type x = x
    method identifier_type x = x
    method identifier_constructor x = x
    method identifier_field x = x
    method identifier_extension x = x
    method identifier_exception x = x
    method identifier_value x = x
    method identifier_class x = x
    method identifier_class_type x = x
    method identifier_method x = x
    method identifier_instance_variable x = x
    method identifier_label x = x
    method identifier_page x = x
    method identifier_signature x = x
    method identifier x = x

    method path_module x = resolve_module_path where_am_i tbl x
    method path_module_type x = resolve_path_module_type where_am_i tbl x
    method path_type x = resolve_type_path where_am_i tbl x
    method path_class_type x = resolve_class_type_path where_am_i tbl x

    method! module_ md =
      let open Lang.Module in
      let {id; doc; type_; expansion; canonical;display_type;hidden} = md in
      let id' = self#identifier_module id in
      let sig_id = (id' :> Identifier.Signature.t) in
      let self = {< where_am_i = Some sig_id >} in
      let doc' = self#documentation doc in
      let type' = self#module_decl_with_id sig_id type_ in
      let expansion' = Maps.option_map self#module_expansion expansion in
      let canonical' =
        Maps.option_map
          (Maps.pair_map self#path_module self#reference_module)
          canonical
      in
      let display_type' =
        Maps.option_map (self#module_decl_with_id sig_id) display_type
      in
      let hidden' = self#module_hidden hidden in
        if id != id' || doc != doc' || type_ != type'
          || expansion != expansion' || canonical != canonical'
          || display_type != display_type'
        then
          {id = id'; doc = doc'; type_ = type';
          expansion = expansion'; canonical = canonical';
          display_type = display_type'; hidden = hidden'}
        else md

    method! module_type mty =
      let open Lang.ModuleType in
      let {id; doc; expr; expansion} = mty in
      let id' = self#identifier_module_type id in
      let sig_id = (id' :> Identifier.Signature.t) in
      let self = {< where_am_i = Some sig_id >} in
      let doc' = self#documentation doc in
      let expr' =
        match expr with
        | None -> expr
        | Some body ->
            let body' = self#module_type_expr_with_id sig_id body in
            if body != body' then Some body'
            else expr
      in
      let expansion' = Maps.option_map self#module_expansion expansion in
        if id != id' || doc != doc' || expr != expr' || expansion != expansion' then
          {id = id'; doc = doc'; expr = expr'; expansion = expansion'}
        else mty

    method! include_ incl =
      let open Lang.Include in
      let {parent; doc; decl; expansion} = incl in
      let parent' = self#identifier_signature parent in
      let doc' = self#documentation doc in
      let decl' = self#module_decl_with_id parent decl in
      let expansion' = self#include_expansion expansion in
        if parent != parent' || doc != doc' || decl != decl' || expansion != expansion' then
          {parent = parent'; doc = doc'; decl = decl'; expansion = expansion'}
        else incl

    method! module_type_functor_param arg =
      let open Lang.FunctorParameter in
      match arg with
      | Unit -> arg
      | Named { id; expr; expansion } ->
          let id' = self#identifier_module id in
          let sig_id = (id' :> Identifier.Signature.t) in
          let expr' = self#module_type_expr_with_id sig_id expr in
          let expansion' =
            Maps.option_map self#module_expansion expansion
          in
            if id != id' || expr != expr' || expansion != expansion' then
              Named {id = id'; expr = expr'; expansion = expansion'}
            else arg

    method module_type_expr_with_id id expr =
      let open Lang.ModuleType in
        match expr with
        | With(body, substs) ->
          let body = self#module_type_expr_with_id id body in
          let base = CTbl.module_type_expr_with tbl id body in
          let substs =
            List.map (function
              | ModuleEq(frag, eq) ->
                let frag =
                  resolve_module_fragment where_am_i tbl base frag
                in
                let eq = self#module_equation eq in
                ModuleEq(frag, eq)
              | TypeEq(frag, eq) ->
                let frag =
                  resolve_type_fragment where_am_i tbl base frag
                in
                let eq = self#type_decl_equation eq in
                TypeEq(frag, eq)
              | ModuleSubst(frag, p) ->
                let frag =
                  resolve_module_fragment where_am_i tbl base frag
                in
                let p = self#path_module p in
                ModuleSubst(frag, p)
              | TypeSubst(frag, eq) ->
                let frag =
                  resolve_type_fragment where_am_i tbl base frag
                in
                let eq = self#type_decl_equation eq in
                TypeSubst(frag, eq)
            ) substs
          in
          With(body, substs)
        | Functor(arg, res) ->
          let arg' = self#module_type_functor_param arg in
          let res' = self#module_type_expr_with_id id res in
          if res != res' || arg != arg' then Functor(arg', res')
          else expr
        | TypeOf decl ->
          let decl' = self#module_decl_with_id id decl in
          if decl != decl' then TypeOf decl'
          else expr
        | Path _ | Signature _ -> self#module_type_expr expr

    method module_decl_with_id id decl =
      let open Lang.Module in
        match decl with
        | ModuleType expr ->
            let expr' = self#module_type_expr_with_id id expr in
              if expr != expr' then ModuleType expr'
              else decl
        | Alias _ -> self#module_decl decl

    method! type_expr_package pkg =
      let open Lang.TypeExpr.Package in
      let path = resolve_path_module_type where_am_i tbl pkg.path in
      let base = CTbl.module_type_path_with tbl path in
      let substitutions =
        List.map
          (fun (frag, eq) ->
            let frag = resolve_type_fragment where_am_i tbl base frag in
            let eq = self#type_expr eq in
              (frag, eq))
          pkg.substitutions
      in
        {path; substitutions}

    method fragment_type x = x
    method fragment_module x = x

    method reference_module x =
      resolve_module_reference where_am_i tbl x
    method reference_module_type x =
      resolve_module_type_reference where_am_i tbl
        x
    method reference_type x =
      resolve_type_reference where_am_i tbl x
    method reference_constructor x =
      resolve_constructor_reference where_am_i tbl
        x
    method reference_field x =
      resolve_field_reference where_am_i tbl x
    method reference_extension x =
      resolve_extension_reference where_am_i tbl
        x
    method reference_exception x =
      resolve_exception_reference where_am_i tbl
        x
    method reference_value x =
      resolve_value_reference where_am_i tbl x
    method reference_class x =
      resolve_class_reference where_am_i tbl x
    method reference_class_type x =
      resolve_class_type_reference where_am_i tbl
        x
    method reference_method x =
      resolve_method_reference where_am_i tbl x
    method reference_instance_variable x =
      resolve_instance_variable_reference where_am_i tbl
        x
    method reference_label x =
      resolve_label_reference where_am_i tbl x
    method reference_any x =
      resolve_element_reference where_am_i tbl x

    method! documentation_reference r =
      let (path, elements) = super#documentation_reference r in
      splice_section_title tbl path elements

    method! unit_import import =
      let open Lang.Compilation_unit.Import in
        match import with
        | Resolved _ -> import
        | Unresolved(name, _) ->
            match CTbl.base tbl name with
            | CTbl.Found {root; _} -> Resolved root
            | _ -> import

    method resolve_unit unit =
      let this =
        {< where_am_i =
          Some (unit.Lang.Compilation_unit.id :> Identifier.Signature.t) >}
      in
        this#unit unit

    method resolve_page page =
      let this = {< where_am_i = None >} in
        this#page page
end

let build_resolver ?equal ?hash lookup_unit fetch_unit lookup_page fetch_page =
  new resolver ?equal ?hash lookup_unit fetch_unit lookup_page fetch_page

let resolve r u = r#resolve_unit u

let resolve_page r p = r#resolve_page p
