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
open Odoc_model.Lang
open Names

type partial_expansion =
  | Signature of Signature.t
  | Functor of FunctorParameter.t *
               Identifier.Signature.t * int *
               ModuleType.expr

let subst_signature sub = function
  | None -> None
  | Some sg -> Some (Subst.signature sub sg)

let subst_arg sub arg =
  let open FunctorParameter in
  match arg with
  | Unit -> Unit
  | Named {id; expr; expansion} ->
      let id' = Subst.identifier_module sub id in
      let expr' = Subst.module_type_expr sub expr in
      let expansion' =
        Maps.option_map (Subst.module_expansion sub) expansion
      in
        Named {id = id'; expr = expr'; expansion = expansion'}

let subst_expansion sub = function
  | None -> None
  | Some (Signature sg) ->
      let sg' = Subst.signature sub sg in
        Some (Signature sg')
  | Some (Functor(arg, id, offset, expr)) ->
      let arg' = subst_arg sub arg in
      let id', offset' =
        Subst.offset_identifier_signature sub (id, offset)
      in
      let expr' = Subst.module_type_expr sub expr in
        Some (Functor(arg', id', offset', expr'))

let subst_class_expansion sub = function
  | None -> None
  | Some sg -> Some (Subst.class_signature sub sg)

let map_module name ex f =
  let rec loop name items f acc =
    let open Signature in
    let open Module in
      match items with
      | [] ->
        List.rev acc
        (* raise Not_found *)
      | Module (recursive, md)::rest when Identifier.name md.id = name ->
        let md' = f md in
        List.rev_append acc ((Module (recursive, md'))::rest)
      | item :: rest -> loop name rest f (item :: acc)
  in
    match ex with
    | None -> raise Not_found
    | Some (Signature items) -> Some (Signature (loop name items f []))
    | Some (Functor _) -> raise Not_found

let map_type name ex f =
  let rec loop name items f acc =
    let open Signature in
    let open TypeDecl in
      match items with
      | [] ->
        List.rev acc
        (* raise Not_found *)
      | Type (recursive, decl) :: rest when Identifier.name decl.id = name ->
        let decl' = f decl in
        List.rev_append acc ((Type (recursive, decl')) :: rest)
      | item :: rest -> loop name rest f (item :: acc)
  in
    match ex with
    | None -> raise Not_found
    | Some (Signature items) -> Some (Signature (loop name items f []))
    | Some (Functor _) -> raise Not_found

let add_module_with subst md =
  let open Module in
  let open ModuleType in
  let type_ =
    match md.type_ with
    | Alias _ as decl ->
        ModuleType(With(TypeOf decl, [subst]))
    | ModuleType(With(expr, substs)) ->
        ModuleType(With(expr, substs @ [subst]))
    | ModuleType expr ->
        ModuleType(With(expr, [subst]))
  in
    { md with type_; expansion = None }

let refine_type ex (frag : Fragment.Type.t) equation =
  let open Fragment in
  match frag with
  | `Dot _ -> None
  | `Resolved frag ->
    let open Resolved in
    let name, rest = Type.split frag in
      match rest with
      | None -> begin
          try
            map_type name ex
              (fun decl -> TypeDecl.{ decl with equation })
          with Not_found -> None (* TODO should be an error *)
        end
      | Some frag -> begin
          let subst = ModuleType.TypeEq(`Resolved frag, equation) in
          try
            map_module name ex (add_module_with subst)
          with Not_found -> None (* TODO should be an error *)
        end

let refine_module ex (frag : Fragment.Module.t) equation =
  let open Fragment in
  match frag with
  | `Dot _ -> None
  | `Resolved frag ->
    let open Resolved in
    let name, rest = Module.split frag in
      match rest with
      | None -> begin
          try
            map_module name ex
              (fun md -> { md with type_ = equation
                         ; expansion = None })
              (* TODO Fix this to not produce an alias (needs strengthening)
                      or fix OCaml to do the correct thing. *)
          with Not_found -> None (* TODO should be an error *)
        end
      | Some frag -> begin
          let subst = ModuleType.ModuleEq(`Resolved frag, equation) in
          try
            map_module name ex (add_module_with subst)
          with Not_found -> None (* TODO should be an error *)
        end

type intermediate_module_expansion =
  Identifier.Module.t * Odoc_model.Comment.docs
  * (Path.Module.t * Reference.Module.t) option
  * partial_expansion option * Subst.t list

type intermediate_module_type_expansion =
  Identifier.ModuleType.t * Odoc_model.Comment.docs
  * partial_expansion option * Subst.t list

type intermediate_class_type_expansion =
  Identifier.ClassType.t * Odoc_model.Comment.docs
  * ClassSignature.t option * Subst.t list

type expander =
  { equal: Root.t -> Root.t -> bool;
    hash: Root.t -> int;
    expand_root: root:Root.t -> Root.t -> intermediate_module_expansion;
    expand_forward_ref : root:Root.t -> string -> intermediate_module_expansion;
    expand_module_identifier: root:Root.t -> Identifier.Module.t ->
                              intermediate_module_expansion;
    expand_module_type_identifier: root:Root.t -> Identifier.ModuleType.t ->
                                   intermediate_module_type_expansion;
    expand_class_signature_identifier: root:Root.t -> Identifier.ClassSignature.t ->
      intermediate_class_type_expansion;
    expand_signature_identifier: root:Root.t -> Identifier.Signature.t ->
                                 partial_expansion option;
    expand_module_resolved_path: root:Root.t -> Path.Resolved.Module.t ->
                                 intermediate_module_expansion;
    expand_module_path: root:Root.t -> Path.Module.t ->
                                 intermediate_module_expansion;
    expand_module_type_resolved_path: root:Root.t ->
                                      Path.Resolved.ModuleType.t ->
      intermediate_module_type_expansion;
    expand_class_type_path: root:Root.t -> Path.ClassType.t ->
      intermediate_class_type_expansion;
    expand_class_type_resolved_path: root:Root.t -> Path.Resolved.ClassType.t ->
      intermediate_class_type_expansion;
    fetch_unit_from_ref: Reference.Module.t -> Compilation_unit.t option; }

let add_doc_to_class_expansion_opt doc =
  let open ClassSignature in
  function
  | Some ({ items; _ } as sg) ->
      let doc = Comment (`Docs doc) in
      Some { sg with items = doc :: items }
  | otherwise -> otherwise

let add_doc_to_expansion_opt doc = function
  | Some (Signature sg) ->
      let doc = Signature.Comment (`Docs doc) in
      Some (Signature (doc :: sg))
  | otherwise -> otherwise

let rec expand_class_decl t root dest decl =
  let open Class in
  match decl with
  | Arrow (_, _, decl) -> expand_class_decl t root dest decl
  | ClassType expr -> expand_class_type_expr t root dest expr

and expand_class_type_expr ({equal; _} as t) root dest expr =
  let open ClassType in
  match expr with
  | Constr (`Resolved p, _) -> begin
      match t.expand_class_type_resolved_path ~root p with
      | src, doc, ex, subs ->
        let ex = add_doc_to_class_expansion_opt doc ex in
        let ex =
          List.fold_left
            (fun acc sub -> subst_class_expansion sub acc)
            ex subs
        in
        let src = (src : Identifier.ClassType.t :> Identifier.ClassSignature.t) in
        let sub = Subst.rename_class_signature ~equal src dest in
        subst_class_expansion sub ex
      | exception Not_found -> None
    end
  | Constr (p, _) -> begin
      match t.expand_class_type_path ~root p with
      | src, doc, ex, subs ->
        let ex = add_doc_to_class_expansion_opt doc ex in
        let ex =
          List.fold_left
            (fun acc sub -> subst_class_expansion sub acc)
            ex subs
        in
        let src = (src : Identifier.ClassType.t :> Identifier.ClassSignature.t) in
        let sub = Subst.rename_class_signature ~equal src dest in
        subst_class_expansion sub ex
      | exception Not_found -> None
    end
  | Signature csig -> Some csig

let rec expand_module_decl ({equal; _} as t) root dest offset decl =
  let open Module in
    match decl with
    | Alias (`Resolved p) -> begin (* TODO Should have strengthening *)
        match t.expand_module_resolved_path ~root p with
        | src, doc, _, ex, subs ->
          let ex = add_doc_to_expansion_opt doc ex in
          let ex =
            List.fold_left
              (fun acc sub -> subst_expansion sub acc)
              ex subs
          in
          let src = (src : Identifier.Module.t :> Identifier.Signature.t) in
          let sub1 = Subst.rename_signature ~equal src dest offset in
          let ex = subst_expansion sub1 ex in
          let sub2 = Subst.strengthen p in
          subst_expansion sub2 ex
        | exception Not_found -> None (* TODO: Should be an error *)
      end
    | Alias p -> begin
        match t.expand_module_path ~root p with
        | src, doc, _, ex, subs ->
          let ex = add_doc_to_expansion_opt doc ex in
          let ex =
            List.fold_left
              (fun acc sub -> subst_expansion sub acc)
              ex subs
          in
          let src = (src : Identifier.Module.t :> Identifier.Signature.t) in
          let sub = Subst.rename_signature ~equal src dest offset in
          subst_expansion sub ex
        | exception Not_found -> None (* TODO: Should be an error *)
      end
    | ModuleType expr -> expand_module_type_expr t root dest offset expr

and expand_module_type_expr ({equal; _ } as t) root dest offset expr =
  let open ModuleType in
    match expr with
    | Path (`Resolved p) -> begin
        match t.expand_module_type_resolved_path ~root p with
        | src, _, ex, subs ->
          let ex =
            List.fold_left
              (fun acc sub -> subst_expansion sub acc)
              ex subs
          in
          let src = (src : Identifier.ModuleType.t :> Identifier.Signature.t)  in
          let sub = Subst.rename_signature ~equal src dest offset in
            subst_expansion sub ex
        | exception Not_found -> None (* TODO: Should be an error *)
      end
    | Path _ -> None
    | Signature sg -> Some (Signature sg)
    | Functor(arg, expr) -> Some (Functor(arg, dest, (offset + 1), expr))
    | With(expr, substs) ->
        let ex = expand_module_type_expr t root dest offset expr in
          List.fold_left
            (fun ex subst ->
               match subst with
               | TypeEq(frag, eq) -> refine_type ex frag eq
               | ModuleEq(frag, eq) -> refine_module ex frag eq
               | TypeSubst _ -> ex (* TODO perform substitution *)
               | ModuleSubst _ -> ex (* TODO perform substitution *))
            ex substs
    | TypeOf decl ->
        expand_module_decl t root dest offset decl (* TODO perform weakening *)

let expand_module t root md =
  let open Module in
  let id = (md.id : Identifier.Module.t :> Identifier.Signature.t)  in
  expand_module_decl t root id 0 md.type_

let expand_class t root c =
  let open Class in
  let id = (c.id : Identifier.Class.t :> Identifier.ClassSignature.t)  in
  expand_class_decl t root id c.type_

let expand_class_type t root c =
  let open ClassType in
  let id = (c.id : Identifier.ClassType.t :> Identifier.ClassSignature.t) in
  expand_class_type_expr t root id c.expr

let expand_module_type t root mty =
  let open ModuleType in
  match mty.expr with
  | Some expr ->
      let id = (mty.id : Identifier.ModuleType.t :> Identifier.Signature.t) in
        expand_module_type_expr t root id 0 expr
  | None -> Some (Signature [])

type include_expansion_result =
  | Failed of Signature.t
  | Expanded of Signature.t
  | To_functor

let expand_include t root incl =
  let open Include in
    if incl.expansion.resolved then Expanded incl.expansion.content
    else begin
      match expand_module_decl t root incl.parent 0 incl.decl with
      | None -> Failed incl.expansion.content
      | Some (Signature sg) -> Expanded sg
      | Some (Functor _) -> To_functor (* TODO: Should be an error *)
    end

let expand_argument_ t root {FunctorParameter. id; expr; expansion} =
  match expansion with
  | None ->
      let id = (id : Identifier.Module.t :> Identifier.Signature.t) in
      expand_module_type_expr t root id 0 expr
  | Some Module.AlreadyASig -> begin
      match expr with
      | ModuleType.Signature sg -> Some (Signature sg)
      | _ -> assert false
    end
  | Some (Module.Signature sg) -> Some (Signature sg)
  | Some (Module.Functor _) ->
      (* TODO: This is for cases where the module argument is itself a functor.
         It *should* be handled, but latter. *)
      None

let find_module t root name ex =
  let rec inner_loop name items =
    let open Signature in
    let open Module in
      match items with
      | [] -> raise Not_found
      | Module (_, md) :: _ when Identifier.name md.id = name -> md
      | Include incl :: rest -> begin
          match expand_include t root incl with
          | To_functor -> inner_loop name rest
          | Failed sg | Expanded sg -> inner_loop name (sg @ rest)
        end
      | _ :: rest -> inner_loop name rest
  in
  let rec loop t root name ex =
    match ex with
    | None -> raise Not_found
    | Some (Signature items) -> inner_loop name items
    | Some (Functor(_, dest, offset, expr)) ->
        loop t root name (expand_module_type_expr t root dest offset expr)
  in
    loop t root name ex

let find_class_type t root name ex =
  let rec inner_loop name items =
    let open Signature in
    let open ClassType in
      match items with
      | [] -> raise Not_found
      | ClassType (_, cd) :: _ when Identifier.name cd.id = name -> cd
      | Include incl :: rest -> begin
          match expand_include t root incl with
          | To_functor -> inner_loop name rest
          | Failed sg | Expanded sg -> inner_loop name (sg @ rest)
        end
      | _ :: rest -> inner_loop name rest
  in
  let rec loop t root name ex =
    match ex with
    | None -> raise Not_found
    | Some (Signature items) -> inner_loop name items
    | Some (Functor(_, dest, offset, expr)) ->
        loop t root name (expand_module_type_expr t root dest offset expr)
  in
    loop t root name ex

let find_argument t root pos ex =
  let rec loop t root pos ex =
    match ex with
    | None -> raise Not_found
    | Some (Signature _) -> raise Not_found
    | Some (Functor(Unit, _, _, _)) when pos = 1 -> raise Not_found
    | Some (Functor(Named arg, _, _, _)) when pos = 1 -> arg
    | Some (Functor(_, dest, offset, expr)) ->
        loop t root (pos - 1) (expand_module_type_expr t root dest offset expr)
  in
    loop t root pos ex

let find_module_type t root name ex =
  let rec inner_loop name items =
    let open Signature in
    let open ModuleType in
      match items with
      | [] -> raise Not_found
      | ModuleType mty :: _ when Identifier.name mty.id = name -> mty
      | Include incl :: rest -> begin
          match expand_include t root incl with
          | To_functor -> inner_loop name rest
          | Failed sg | Expanded sg -> inner_loop name (sg @ rest)
        end
      | _ :: rest -> inner_loop name rest
  in
  let rec loop t root name ex =
    match ex with
    | None -> raise Not_found
    | Some (Signature items) -> inner_loop name items
    | Some (Functor(_, dest, offset, expr)) ->
        loop t root name (expand_module_type_expr t root dest offset expr)
  in
    loop t root name ex

let expand_signature_identifier' t root (id : Identifier.Signature.t) =
  match id with
  | `Root(root', _name) ->
      let _, _, _, ex, subs = t.expand_root ~root root' in
      let ex =
        List.fold_left
          (fun acc sub -> subst_expansion sub acc)
          ex subs
      in
        ex
  | `Module(parent, name) ->
      let ex = t.expand_signature_identifier ~root parent in
      let md = find_module t root (ModuleName.to_string_unsafe name) ex in
        expand_module t root md
  | `Argument(parent, pos, _name) ->
      let ex = t.expand_signature_identifier ~root parent in
      let arg = find_argument t root pos ex in
        expand_argument_ t root arg
  | `ModuleType(parent, name) ->
      let ex = t.expand_signature_identifier ~root parent in
      let mty = find_module_type t root (ModuleTypeName.to_string name) ex in
        expand_module_type t root mty

and expand_module_identifier' t root (id : Identifier.Module.t) =
  match id with
  | `Root(root', _name) -> t.expand_root ~root root'
  | `Module(parent, name) ->
      let open Module in
      let ex = t.expand_signature_identifier ~root parent in
      let md = find_module t root (ModuleName.to_string_unsafe name) ex in
        md.id, md.doc, md.canonical, expand_module t root md, []
  | `Argument(parent, pos, _name) ->
      let ex = t.expand_signature_identifier ~root parent in
      let {FunctorParameter. id; _} as arg = find_argument t root pos ex in
      let doc = [] in
      id, doc, None, expand_argument_ t root arg, []

and expand_module_type_identifier' t root (id : Identifier.ModuleType.t) =
  match id with
  | `ModuleType(parent, name) ->
      let open ModuleType in
      let ex = t.expand_signature_identifier ~root parent in
      let mty = find_module_type t root (ModuleTypeName.to_string name) ex in
        mty.id, mty.doc, expand_module_type t root mty, []

and expand_class_signature_identifier' t root (id : Identifier.ClassSignature.t) =
  match id with
  | `Class(parent, name) ->
    let ex = t.expand_signature_identifier ~root parent in
    let ct = find_class_type t root (ClassName.to_string name) ex in
    ct.id, ct.doc, expand_class_type t root ct, []
  | `ClassType(parent, name) ->
    let ex = t.expand_signature_identifier ~root parent in
    let ct = find_class_type t root (ClassTypeName.to_string name) ex in
    ct.id, ct.doc, expand_class_type t root ct, []

and expand_module_resolved_path' ({equal = eq; _ } as t) root (p : Path.Resolved.Module.t) =
  match p with
  | `Identifier id -> t.expand_module_identifier ~root id
  | `Subst(_, p) -> t.expand_module_resolved_path ~root p
  | `SubstAlias(_, p) -> t.expand_module_resolved_path ~root p
  | `Hidden p -> t.expand_module_resolved_path ~root p
  | `Module(parent, name) ->
    let open Module in
    let id, _, canonical, ex, subs =
      t.expand_module_resolved_path ~root parent
    in
    let md = find_module t root (ModuleName.to_string name) ex in
    let sub = Subst.prefix ~equal:eq ~canonical id in
    md.id, md.doc, md.canonical, expand_module t root md, sub :: subs
  | `Canonical (p, _) -> t.expand_module_resolved_path ~root p
  | `Apply _ -> raise Not_found (* TODO support functor application *)

and expand_module_path' ({equal = eq; _ } as t) root (p : Path.Module.t) =
  match p with
  | `Forward s -> t.expand_forward_ref ~root s
  | `Dot(parent, name) ->
      let open Module in
      let id, _, canonical, ex, subs =
        t.expand_module_path ~root parent
      in
      let md = find_module t root name ex in
      let sub = Subst.prefix ~equal:eq ~canonical id in
        md.id, md.doc, md.canonical, expand_module t root md, sub :: subs
  | `Root _ | `Apply _ | `Resolved _ -> raise Not_found (* TODO: assert false? *)

and expand_class_type_path' ({equal = eq ; _ } as t) root
      (p : Path.ClassType.t) =
  match p with
  | `Resolved _ -> raise Not_found (* TODO: assert false? *)
  | `Dot(parent, name) ->
    let open ClassType in
    let id, _, canonical, ex, subs =
      t.expand_module_path ~root parent
    in
    let c = find_class_type t root name ex in
    let sub = Subst.prefix ~equal:eq ~canonical id in
    c.id, c.doc, expand_class_type t root c, sub :: subs

and expand_class_type_resolved_path' ({equal = eq; _} as t) root
      (p : Path.Resolved.ClassType.t) =
  match p with
  | `Identifier id -> t.expand_class_signature_identifier ~root id
  | `Class(parent, name) ->
    let open ClassType in
    let id, _, canonical, ex, subs =
      t.expand_module_resolved_path ~root parent
    in
    let c = find_class_type t root (ClassName.to_string name) ex in
    let sub = Subst.prefix ~equal:eq ~canonical id in
    c.id, c.doc, expand_class_type t root c, sub :: subs
  | `ClassType(parent, name) ->
    let open ClassType in
    let id, _, canonical, ex, subs =
      t.expand_module_resolved_path ~root parent
    in
    let c = find_class_type t root (ClassTypeName.to_string name) ex in
    let sub = Subst.prefix ~equal:eq ~canonical id in
    c.id, c.doc, expand_class_type t root c, sub :: subs

and expand_module_type_resolved_path' ({equal = eq; _} as t) root
                                     (p : Path.Resolved.ModuleType.t) =
  match p with
  | `Identifier id -> t.expand_module_type_identifier ~root id
  | `ModuleType(parent, name) ->
      let open ModuleType in
      let id, _, canonical, ex, subs =
        t.expand_module_resolved_path ~root parent
      in
      let mty = find_module_type t root (ModuleTypeName.to_string name) ex in
      let sub = Subst.prefix ~equal:eq ~canonical id in
        mty.id, mty.doc, expand_module_type t root mty, sub :: subs

and expand_unit ({equal; hash;_} as t) root unit =
  let open Compilation_unit in
    match unit.expansion with
    | Some ex -> Some ex
    | None ->
      match unit.content with
      | Pack items ->
          let open Packed in
          let rec loop ids mds = function
            | [] ->
              let open Signature in
              let sg = List.rev_map (fun md -> Module (Ordinary, md)) mds in
              ids, Some sg
            | item :: rest ->
                match item.path with
                | `Resolved p -> begin
                    match t.expand_module_resolved_path ~root p with
                    | src, doc, _, ex, subs -> begin
                      match ex with
                      | None -> [], None
                      | Some (Functor _) ->
                          [], None (* TODO should be an error *)
                      | Some (Signature sg) ->
                          let sg =
                            List.fold_left
                              (fun acc sub ->
                                 Subst.signature sub acc)
                              sg subs
                          in
                          let doc =
                            List.fold_left
                              (fun acc sub ->
                                 Subst.documentation sub acc)
                              doc subs
                          in
                          let open Module in
                          let id = item.id in
                          let type_ = ModuleType (ModuleType.Signature sg) in
                          let canonical = None in
                          let md = {id; doc; type_; canonical;
                                    expansion = Some (Signature sg);
                                    display_type = None; hidden = false} in
                          loop ((src, item.id) :: ids) (md :: mds) rest
                      end
                    | exception Not_found -> [], None (* TODO: Should be an error *)
                  end
                | _ -> [], None
          in
          let ids, sg = loop [] [] items in
          let sub = Subst.pack ~equal ~hash ids in
            subst_signature sub sg
      | Module sg -> Some sg


let create ?equal ?hash
      (lookup : string -> Component_table.lookup_unit_result)
      (fetch : root:Root.t -> Root.t -> Compilation_unit.t) =
  let equal =
    match equal with
    | None -> (=)
    | Some eq -> eq
  in
  let hash =
    match hash with
    | None -> Hashtbl.hash
    | Some h -> h
  in
  let module RootHash = struct
    type t = Root.t * Root.t
    let equal (a1, b1) (a2, b2) = equal a1 a2 && equal b1 b2
    let hash (a, b) = Hashtbl.hash (hash a, hash b)
  end in
  let module RootTbl = Hashtbl.Make(RootHash) in
  let expand_root_tbl = RootTbl.create 13 in
  let module IdentifierHash = struct
    type t = Root.t * Identifier.t
    let equal (root1, id1) (root2, id2) =
      equal root1 root2 && Identifier.equal id1 id2
    let hash (root, id) =
      Hashtbl.hash (hash root, Identifier.hash id)
  end in
  let module IdentifierTbl = Hashtbl.Make(IdentifierHash) in
  let expand_module_identifier_tbl = IdentifierTbl.create 13 in
  let expand_module_type_identifier_tbl = IdentifierTbl.create 13 in
  let expand_signature_identifier_tbl = IdentifierTbl.create 13 in
  let expand_class_signature_identifier_tbl = IdentifierTbl.create 13 in
  let module RPathHash = struct
    type t = Root.t * Path.Resolved.t
    let equal (root1, p1) (root2, p2) =
      equal root1 root2 && Path.Resolved.equal p1 p2
    let hash (root, _p) =
      Hashtbl.hash (hash root, Path.Resolved.hash)
  end in
  let module RPathTbl = Hashtbl.Make(RPathHash) in
  let module PathHash = struct
    type t = Root.t * Path.t
    let equal (root1, p1) (root2, p2) =
      equal root1 root2 && Path.equal p1 p2
    let hash (root, _p) =
      Hashtbl.hash (hash root, Path.hash)
  end in
  let module PathTbl = Hashtbl.Make(PathHash) in
  let expand_module_resolved_path_tbl = RPathTbl.create 13 in
  let expand_module_path_tbl = PathTbl.create 13 in
  let expand_module_type_resolved_path_tbl = RPathTbl.create 13 in
  let expand_class_type_path_tbl = PathTbl.create 13 in
  let expand_class_type_resolved_path_tbl = RPathTbl.create 13 in
  let rec expand_root ~root root' =
    let key = (root, root') in
    try
      RootTbl.find expand_root_tbl key
    with Not_found ->
      let open Compilation_unit in
      let unit = fetch ~root root' in
      let sg = expand_unit t root unit in
      let ex =
        match sg with
        | None -> None
        | Some sg -> Some (Signature sg)
      in
      let res = (unit.id, unit.doc, None, ex, []) in
      RootTbl.add expand_root_tbl key res;
      res
  and fetch_unit_from_ref ref =
    (* FIXME: this function is not really necessary is it? *)
    match ref with
    | `Resolved (`Identifier (`Root (_, unit_name))) ->
      begin match lookup (UnitName.to_string unit_name) with
      | Component_table.Found { root; _ } ->
        let unit = fetch ~root root in
        Some unit
      | _ -> None
      end
    | _ ->
      None
  and expand_forward_ref ~root str =
    match lookup str with
    | Component_table.Found { root = a; _ } -> expand_root ~root a
    | _ -> raise Not_found
  and expand_module_identifier ~root (id : Identifier.Module.t) =
    let key = (root, (id :> Identifier.t)) in
    try
      IdentifierTbl.find expand_module_identifier_tbl key
    with Not_found ->
      let res = expand_module_identifier' t root id in
      IdentifierTbl.add expand_module_identifier_tbl key res;
      res
  and expand_module_type_identifier ~root (id : Identifier.ModuleType.t) =
    let key = (root, (id :> Identifier.t)) in
    try
      IdentifierTbl.find expand_module_type_identifier_tbl key
    with Not_found ->
      let res = expand_module_type_identifier' t root id in
      IdentifierTbl.add expand_module_type_identifier_tbl key res;
      res
  and expand_signature_identifier ~root (id : Identifier.Signature.t) =
    let key = (root, (id :> Identifier.t)) in
    try
      IdentifierTbl.find expand_signature_identifier_tbl key
    with Not_found ->
      let res = expand_signature_identifier' t root id in
      IdentifierTbl.add expand_signature_identifier_tbl key res;
      res
  and expand_class_signature_identifier ~root (id : Identifier.ClassSignature.t)  =
    let key = (root, (id :> Identifier.t)) in
    try
      IdentifierTbl.find expand_class_signature_identifier_tbl key
    with Not_found ->
      let res = expand_class_signature_identifier' t root id in
      IdentifierTbl.add expand_class_signature_identifier_tbl key res;
      res
  and expand_module_resolved_path ~root (p : Path.Resolved.Module.t) =
    let key = (root, (p :> Path.Resolved.t)) in
    try
      RPathTbl.find expand_module_resolved_path_tbl key
    with Not_found ->
      let res = expand_module_resolved_path' t root p in
      RPathTbl.add expand_module_resolved_path_tbl key res;
      res
  and expand_module_path ~root (p : Path.Module.t) =
    let key = (root, (p :> Path.t)) in
    try
      PathTbl.find expand_module_path_tbl key
    with Not_found ->
      let res = expand_module_path' t root p in
      PathTbl.add expand_module_path_tbl key res;
      res
  and expand_module_type_resolved_path ~root (p : Path.Resolved.ModuleType.t) =
    let key = (root, (p :> Path.Resolved.t)) in
    try
      RPathTbl.find expand_module_type_resolved_path_tbl key
    with Not_found ->
      let res = expand_module_type_resolved_path' t root p in
      RPathTbl.add expand_module_type_resolved_path_tbl key res;
      res
  and expand_class_type_path ~root (p : Path.ClassType.t) =
    let key = (root, (p :> Path.t)) in
    try
      PathTbl.find expand_class_type_path_tbl key
    with Not_found ->
      let res = expand_class_type_path' t root p in
      PathTbl.add expand_class_type_path_tbl key res;
      res
  and expand_class_type_resolved_path ~root (p : Path.Resolved.ClassType.t) =
    let key = (root, (p :> Path.Resolved.t)) in
    try
      RPathTbl.find expand_class_type_resolved_path_tbl key
    with Not_found ->
      let res = expand_class_type_resolved_path' t root p in
      RPathTbl.add expand_class_type_resolved_path_tbl key res;
      res
  and t =
    { equal; hash;
      expand_root; expand_forward_ref; expand_module_path;
      expand_module_identifier;
      expand_module_type_identifier;
      expand_signature_identifier;
      expand_class_signature_identifier;
      expand_module_resolved_path;
      expand_module_type_resolved_path;
      expand_class_type_path;
      expand_class_type_resolved_path;
      fetch_unit_from_ref; }
  in
    t

let rec force_expansion t root (ex : partial_expansion option) =
  match ex with
  | None -> None
  | Some (Signature sg) -> Some (Module.Signature sg)
  | Some (Functor(arg, dest, offset, expr)) ->
      let arg = expand_argument t arg in
      let ex = expand_module_type_expr t root dest offset expr in
        match force_expansion t root ex with
        | None -> None
        | Some Module.AlreadyASig ->
          (* we are never returning it, so we cannot receive it. *)
          assert false
        | Some (Module.Signature sg) -> Some(Module.Functor([arg], sg))
        | Some (Module.Functor(args, sg)) ->
            Some(Module.Functor(arg :: args, sg))

and expand_argument t arg =
  match arg with
  | Unit -> arg
  | Named ({FunctorParameter. id; expr; expansion} as a) ->
      match expansion with
      | Some _ -> arg
      | None ->
          let root = Identifier.Module.root id in
          let expansion = force_expansion t root (expand_argument_ t root a) in
          Named {FunctorParameter. id; expr; expansion}

(** We will always expand modules which are not aliases. For aliases we only
    expand when the thing they point to should be hidden. *)
let should_expand _t _id decl =
  match decl with
  | Module.Alias p -> Path.is_hidden (p :> Path.t)
  | _ -> true

let is_canonical_tag doc =
  match doc with
  | [{Odoc_model.Location_.value = `Tag (`Canonical _); _}] -> true
  | _ -> false

(** For module aliases where the binding site doesn't have any doc comment
    attached, then we fetch the doc for the thing it aliases. *)
let expand_mod_alias_doc md =
  let open Module in
  match md.type_ with
  | ModuleType _  -> md
  | Alias _ ->
    match md.doc with
    | (_::_) -> md
    | _ ->
      match md.expansion with
      | Some (
          Signature (
            Comment (`Docs c) ::
            Comment (`Docs doc) ::
            expansion
          )
        ) when is_canonical_tag c ->
        { md with doc; expansion = Some (Signature expansion) }
      | _ ->
        md

(** Set display type for aliases to hidden things. *)
let set_display_type md =
  let open Module in
  match md.display_type with
  | Some _ -> md
  | None ->
    match md.type_ with
    | Alias p ->
      begin match p with
      | `Resolved (`Hidden _) ->
        let display_type : Module.decl option =
          match md.expansion with
          | Some AlreadyASig -> assert false (* [md.type_] is [Alias] *)
          | Some (Signature sg) -> Some (ModuleType (ModuleType.Signature sg))
          | Some (Functor (args, sg)) ->
            let expr =
              List.fold_right (fun arg acc -> ModuleType.Functor (arg, acc))
                args (ModuleType.Signature sg)
            in
            Some (ModuleType expr)
          | None -> None
        in
        { md with display_type }
      | _ -> md
      end
    | _ -> md

let expand_module t md =
  let open Module in
    match md.expansion with
    | Some _ -> md
    | None ->
      if should_expand t md.id md.type_ then
        let root = Identifier.Module.root md.id in
        let expansion = force_expansion t root (expand_module t root md) in
        set_display_type (expand_mod_alias_doc { md with expansion })
      else
        md

let expand_module_type t mty =
  let open ModuleType in
    match mty.expansion with
    | Some _ -> mty
    | None ->
        let root = Identifier.ModuleType.root mty.id in
        let expansion = force_expansion t root (expand_module_type t root mty) in
          { mty with expansion }

let remove_docs_from_signature =
  (* Remove bare doc comments from a signature *)
  let open Signature in
  function
  | Comment (`Docs _) :: xs -> xs
  | xs -> xs

let expand_include t incl =
  let open Include in
    if incl.expansion.resolved then incl
    else begin
      let root = Identifier.Signature.root incl.parent in
        match expand_include t root incl with
        | Expanded content' ->
            let content = remove_docs_from_signature content' in
            let expansion = {content;resolved=true} in
              { incl with expansion }
        | _ -> incl
    end

let expand_class t c =
  let open Class in
  match c.expansion with
  | Some _ -> c
  | None ->
    let root = Identifier.(ClassSignature.root @@ (c.id :> ClassSignature.t)) in
    let expansion = expand_class t root c in
    { c with expansion }

let expand_class_type t c =
  let open ClassType in
  match c.expansion with
  | Some _ -> c
  | None ->
    let root = Identifier.(ClassSignature.root @@ (c.id :> ClassSignature.t)) in
    let expansion = expand_class_type t root c in
    { c with expansion }
(*
let expand_unit t unit =
  let open Unit in
    match unit.expansion with
    | Some _ -> unit
    | None ->
        let root = Identifier.module_root unit.id in
        let expansion = expand_unit t root unit in
          { unit with expansion }
*)

class t ?equal ?hash lookup fetch = object
  val t = create ?equal ?hash lookup fetch
  val unit = None

  inherit Maps.types as super
  method root x = x

  (* Define virtual methods. *)
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

  method path_module x = x
  method path_module_type x = x
  method path_type x = x
  method path_class_type x = x
  method fragment_type x = x
  method fragment_module x = x

  method! module_ md =
    let md' = expand_module t md in
      super#module_ md'

  method! module_type mty =
    let mty' = expand_module_type t mty in
      super#module_type mty'

  method! include_ incl =
    let incl' = expand_include t incl in
    super#include_ incl'

  method! module_type_functor_param arg =
    let arg = expand_argument t arg in
      super#module_type_functor_param arg

  method! class_ c =
    let c' = expand_class t c in
    super#class_ c'

  method! class_type c =
    let c' = expand_class_type t c in
    super#class_type c'

  (* method! documentation_special_modules (rf, txt as pair) =
    let rf' = self#reference_module rf in
    let txt' =
      match txt with
      | _ :: _ -> txt
      | [] ->
        match t.fetch_unit_from_ref rf' with
        | None -> txt
        | Some u ->
          let open Odoc_model.Comment in
          match u.Compilation_unit.doc with
          | Ok { text; _ } ->
            begin match text with
            | [] -> txt
            | _ -> text
            end
          | _ -> txt
    in
    let txt' = self#documentation_text txt' in
    if rf != rf' || txt != txt' then (rf', txt')
    else pair *)

  (* CR trefis: TODO *)
  method reference_module x = x
  method reference_module_type x = x
  method reference_type x = x
  method reference_constructor x = x
  method reference_field x = x
  method reference_extension x = x
  method reference_exception x = x
  method reference_value x = x
  method reference_class x = x
  method reference_class_type x = x
  method reference_method x = x
  method reference_instance_variable x = x
  method reference_label x = x
  method reference_any x = x

  method expand unit =
    let this = {< unit = Some unit >} in
      this#unit unit
end

let build_expander ?equal ?hash lookup fetch =
  new t ?equal ?hash lookup fetch

let expand e u = e#expand u
