(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the Lesser GNU Public License version 3.0.                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)


(* - Types - *)

open IndexTypes

(* This type is used to pass the full structure to functions building sub-trees,
   so that they can (lazily) lookup type or module type indirections in scope.
   Values of this type should have the form {[
     [ [Module;Submodule], lazy subtrie_at_Module.Submodule;
       [Module], lazy subtrie_at_Module;
       [], lazy subtrie_at_Root ]
   ]}
*)
type parents = (string list * t Lazy.t) list


(* - Utility functions - *)

open IndexMisc

let orig_file_name = function
  | Cmt f | Cmti f | Cmi f -> f

let equal_kind k1 k2 = match k1,k2 with
  | Type,Type | Value,Value | Exception,Exception
  | OpenType,OpenType
  | Field _,Field _ | Variant _,Variant _ | Method _,Method _
  | Module,Module | ModuleType,ModuleType
  | Class,Class | ClassType,ClassType
  | Keyword,Keyword ->
      true
  | Type,_ | Value,_ | Exception,_
  | OpenType,_
  | Field _,_ | Variant _,_ | Method _,_
  | Module,_ | ModuleType,_
  | Class,_ | ClassType,_
  | Keyword,_ ->
      false

let has_kind k info = equal_kind k info.kind

(* - Trie loading and manipulation functions - *)

(* Used as path separator *)
let dot = char_of_int 0

let fix_path_prefix strip new_pfx =
  let rec tln n = function
    | [] -> []
    | _::tl as l -> if n > 0 then tln (n-1) tl else l
  in
  let rev_pfx = List.rev new_pfx in
  fun id -> {id with path = List.rev_append rev_pfx (tln strip id.path)}

let overriding_merge t1 t2 =
  let f = (Trie.filter_keys ((<>) dot) t2) in
  Trie.fold0
    (fun t path values ->
       let t =
         List.fold_left (fun t v -> Trie.add t path v)
           (Trie.unset t path) values
       in
       if List.exists (function
           | {kind=Module|ModuleType|Class|ClassType} -> true
           | _ -> false)
           values
       then
         let subpath = path @ [dot] in
         Trie.graft_lazy t subpath (lazy (Trie.sub t2 subpath))
       else t)
    f
    t1

let open_module ?(cleanup_path=false) t path =
  let strip_path = fix_path_prefix (List.length path) [] in
  let submodule = Trie.sub t (modpath_to_key path) in
  let submodule =
    if cleanup_path then Trie.map (fun _key -> strip_path) submodule
    else submodule
  in
  (* Trie.merge ~values:(fun _ v -> v) t submodule -- keeps hidden values in subtrees *)
  overriding_merge t submodule

let alias ?(cleanup_path=false) t origin alias =
  let subtree = Trie.sub t (modpath_to_key origin) in
  let subtree =
    let strip_path = fix_path_prefix (List.length origin) alias in
    if cleanup_path then Trie.map (fun _key -> strip_path) subtree
    else subtree
  in
  Trie.graft t (modpath_to_key alias) subtree

(* Pops comments from a list of comments (string * loc) to find the ones that
   are associated to a given location. Also returns the remaining comments after
   the location. *)
let associate_comment ?(after_only=false) comments loc nextloc =
  if loc = Location.none then None, comments else
  let lstart = loc.Location.loc_start.Lexing.pos_lnum
  and lend =  loc.Location.loc_end.Lexing.pos_lnum in
  let isnext c =
    nextloc <> Location.none &&
    nextloc.Location.loc_start.Lexing.pos_cnum <
    c.Location.loc_end.Lexing.pos_cnum
  in
  let rec aux = function
    | [] -> None, []
    | (comment, cloc)::comments ->
        let cstart = cloc.Location.loc_start.Lexing.pos_lnum
        and cend =  cloc.Location.loc_end.Lexing.pos_lnum
        in
        if cend < lstart - 1 || cstart < lend && after_only then
          aux comments
        else if cstart > lend + 1 ||
                isnext cloc ||
                cstart > lstart && cend < lend (* keep inner comments *)
        then
          None, (comment, cloc)::comments
        else if String.length comment < 2 ||
                comment.[0] <> '*' || comment.[1] = '*'
        then
          aux comments
        else
        let comment =
          String.trim (String.sub comment 1 (String.length comment - 1))
        in
        match aux comments with
        | None, comments -> Some comment, comments
        | Some c, comments -> Some (String.concat "\n" [comment; c]), comments
  in
  aux comments

let ty_of_sig_item =
  let open Printtyp in
  function
#if OCAML_VERSION < "4.08"
  | Types.Sig_value(id, decl) -> tree_of_value_description id decl
  | Types.Sig_type(id, decl, rs) -> tree_of_type_declaration id decl rs
  | Types.Sig_typext(id, decl, es) -> tree_of_extension_constructor id decl es
  | Types.Sig_module(id, { Types.md_type }, rs) -> tree_of_module id md_type rs
  | Types.Sig_modtype(id, decl) -> tree_of_modtype_declaration id decl
  | Types.Sig_class(id, decl, rs) -> tree_of_class_declaration id decl rs
  | Types.Sig_class_type(id, decl, rs) -> tree_of_cltype_declaration id decl rs
#else
  | Types.Sig_value(id, decl, _) -> tree_of_value_description id decl
  | Types.Sig_type(id, decl, rs, _) -> tree_of_type_declaration id decl rs
  | Types.Sig_typext(id, decl, es, _) -> tree_of_extension_constructor id decl es
  | Types.Sig_module(id, _, { Types.md_type }, rs, _) -> tree_of_module id md_type rs
  | Types.Sig_modtype(id, decl, _) -> tree_of_modtype_declaration id decl
  | Types.Sig_class(id, decl, rs, _) -> tree_of_class_declaration id decl rs
  | Types.Sig_class_type(id, decl, rs, _) -> tree_of_cltype_declaration id decl rs
#endif


(* -- Qualifying types -- *)

#if OCAML_VERSION >= "4.08"
  let n s = {Outcometree.printed_name = s}
  let nn {Outcometree.printed_name} = printed_name
#else
  let n s = s
  let nn s = s
#endif

(* The types may contain unqualified identifiers.
   We need to do some (lazy) lookup in the trie to qualify them, so that
   for example [M.empty] shows up as [M.t] and not just [t] *)
let qualify_ty (parents:parents) ty =
  let qualify ident =
    let path =
      let rec get_path = function
        | Outcometree.Oide_ident name -> [nn name]
        | Outcometree.Oide_dot (path, s) -> get_path path @ [s]
        | Outcometree.Oide_apply (p1, _p2) -> get_path p1
      in
      get_path ident
    in
    let key = modpath_to_key ~enddot:false path in
    let rec lookup = function
      | [] | ([],_) :: _ -> ident
      | ((path1::pathn), lazy t) :: parents ->
          if not (List.exists (has_kind Type) (Trie.find_all t key))
          then lookup parents
          else
            let rec add_pfx = function
              | Outcometree.Oide_dot (idp, s) ->
                  Outcometree.Oide_dot (add_pfx idp, s)
              | Outcometree.Oide_apply (idp, idp2) ->
                  Outcometree.Oide_apply (add_pfx idp, idp2)
              | Outcometree.Oide_ident s ->
                  let parentpath =
                    List.fold_left
                      (fun acc modl -> Outcometree.Oide_dot (acc, modl))
                      (Outcometree.Oide_ident (n path1)) pathn
                  in
                  Outcometree.Oide_dot (parentpath, nn s)
            in add_pfx ident
    in
    lookup parents
  in
  let rec aux = (* Some kind of Outcometree.map_ty *)
    let open Outcometree in
    function
    | Otyp_abstract -> Otyp_abstract
    | Otyp_alias (ty, str) -> Otyp_alias (aux ty, str)
    | Otyp_arrow (str, ty1, ty2) -> Otyp_arrow (str, aux ty1, aux ty2)
    | Otyp_class (bl, id, tylist) ->
        Otyp_class (bl, qualify id, List.map aux tylist)
    | Otyp_constr (id, tylist) ->
        Otyp_constr (qualify id, List.map aux tylist)
    | Otyp_manifest (ty1, ty2) -> Otyp_manifest (aux ty1, aux ty2)
    | Otyp_object (strtylist, blopt) ->
        Otyp_object (List.map (fun (str,ty) -> str, aux ty) strtylist, blopt)
    | Otyp_record (strbltylist) ->
        Otyp_record (List.map (fun (str,bl,ty) -> str, bl, aux ty) strbltylist)
    | Otyp_stuff str -> Otyp_stuff str
    | Otyp_sum (strtylisttyoptlist) ->
        Otyp_sum
          (List.map (fun (str,tylist,tyopt) ->
               str, List.map aux tylist,
               match tyopt with Some ty -> Some (aux ty)
                              | None -> None)
              strtylisttyoptlist)
    | Otyp_tuple (tylist) -> Otyp_tuple (List.map aux tylist)
    | Otyp_var (bl, str) -> Otyp_var (bl, str)
    | Otyp_variant (bl, var, bl2, strlistopt) ->
        Otyp_variant (bl, var, bl2, strlistopt)
    | Otyp_poly (str, ty) -> Otyp_poly (str, aux ty)
    | Otyp_module (str, strl, tylist) ->
        Otyp_module (str, strl, List.map aux tylist)
    | Otyp_open -> Otyp_open
#if OCAML_VERSION >= "4.03"
    | Otyp_attribute (ty,attr) -> Otyp_attribute (aux ty, attr)
#endif
  in
  aux ty

let qualify_ty_in_sig_item (parents:parents) =
  let qual = qualify_ty parents in
  let open Outcometree in
  function

  | Osig_type (out_type_decl, rc) ->
      Osig_type ({ out_type_decl with
        otype_type  = qual out_type_decl.otype_type;
        otype_cstrs = List.map (fun (ty1,ty2) -> qual ty1, qual ty2)
            out_type_decl.otype_cstrs }, rc)

#if OCAML_VERSION >= "4.03"
  | Osig_value o -> Osig_value {o with oval_type = qual o.oval_type}
#else
  | Osig_value (str, ty, str2) -> Osig_value (str, qual ty, str2)
#endif

  | Osig_typext (constr, es) ->
      Osig_typext ({ constr with
                     oext_args = List.map qual constr.oext_args }, es)

  | out_sig -> out_sig (* don't get down in modules, classes and their types *)

(* -- end -- *)

let with_path_loc ?srcpath loc =
  match srcpath with
  | None -> loc
  | Some path ->
      let path =
        (* Some magic to get the real source when using jbuilder or
           ocamlbuild *)
        let lpath = string_split Filename.dir_sep.[0] path in
        let rec aux = function
          | "_build" :: "default" :: r -> r
          | "_build" :: r -> r
          | p :: r -> p :: aux r
          | [] -> []
        in
        String.concat Filename.dir_sep (aux lpath)
      in
      let with_path_pos pos =
        let open Lexing in
        if not (Filename.is_relative pos.pos_fname) then pos
        else {pos with pos_fname = Filename.concat path pos.pos_fname}
      in
      let open Location in
      {loc with loc_start = with_path_pos loc.loc_start;
                loc_end = with_path_pos loc.loc_end }

let loc_of_sig_item = function
#if OCAML_VERSION < "4.08"
  | Types.Sig_value (_,descr) -> descr.Types.val_loc
  | Types.Sig_type (_,descr,_) -> descr.Types.type_loc
  | Types.Sig_typext (_,descr,_) -> descr.Types.ext_loc
  | Types.Sig_module (_,descr,_) -> descr.Types.md_loc
  | Types.Sig_modtype (_,descr) -> descr.Types.mtd_loc
  | Types.Sig_class (_,descr,_) -> descr.Types.cty_loc
  | Types.Sig_class_type (_,descr,_) -> descr.Types.clty_loc
#else
  | Types.Sig_value (_,descr,_) -> descr.Types.val_loc
  | Types.Sig_type (_,descr,_,_) -> descr.Types.type_loc
  | Types.Sig_typext (_,descr,_,_) -> descr.Types.ext_loc
  | Types.Sig_module (_,_,descr,_, _) -> descr.Types.md_loc
  | Types.Sig_modtype (_,descr,_) -> descr.Types.mtd_loc
  | Types.Sig_class (_,descr,_,_) -> descr.Types.cty_loc
  | Types.Sig_class_type (_,descr,_,_) -> descr.Types.clty_loc
#endif

let id_of_sig_item = function
#if OCAML_VERSION < "4.08"
  | Types.Sig_value (id,_)
  | Types.Sig_type (id,_,_)
  | Types.Sig_typext (id,_,_)
  | Types.Sig_module (id,_,_)
  | Types.Sig_modtype (id,_)
  | Types.Sig_class (id,_,_)
  | Types.Sig_class_type (id,_,_)
#else
  | Types.Sig_value (id,_,_)
  | Types.Sig_type (id,_,_,_)
  | Types.Sig_typext (id,_,_,_)
  | Types.Sig_module (id,_,_,_,_)
  | Types.Sig_modtype (id,_,_)
  | Types.Sig_class (id,_,_,_)
  | Types.Sig_class_type (id,_,_,_)
#endif
    -> id

let kind_of_sig_item = function
  | Types.Sig_value _ -> Value
  | Types.Sig_type _ -> Type
#if OCAML_VERSION < "4.08"
  | Types.Sig_typext (_, _, Types.Text_exception) -> Exception
#else
  | Types.Sig_typext (_, _, Types.Text_exception, _) -> Exception
#endif
  | Types.Sig_typext _ -> OpenType
  | Types.Sig_module _ -> Module
  | Types.Sig_modtype _ -> ModuleType
  | Types.Sig_class _ -> Class
  | Types.Sig_class_type _ -> ClassType

let attrs_of_sig_item = function
#if OCAML_VERSION < "4.08"
  | Types.Sig_value (_,descr) -> descr.Types.val_attributes
  | Types.Sig_type (_,descr,_) -> descr.Types.type_attributes
  | Types.Sig_typext (_,descr,_) -> descr.Types.ext_attributes
  | Types.Sig_module (_,descr,_) -> descr.Types.md_attributes
  | Types.Sig_modtype (_,descr) -> descr.Types.mtd_attributes
  | Types.Sig_class (_,descr,_) -> descr.Types.cty_attributes
  | Types.Sig_class_type (_,descr,_) -> descr.Types.clty_attributes
#else
  | Types.Sig_value (_,descr,_) -> descr.Types.val_attributes
  | Types.Sig_type (_,descr,_,_) -> descr.Types.type_attributes
  | Types.Sig_typext (_,descr,_,_) -> descr.Types.ext_attributes
  | Types.Sig_module (_,_,descr,_,_) -> descr.Types.md_attributes
  | Types.Sig_modtype (_,descr,_) -> descr.Types.mtd_attributes
  | Types.Sig_class (_,descr,_,_) -> descr.Types.cty_attributes
  | Types.Sig_class_type (_,descr,_,_) -> descr.Types.clty_attributes
#endif

let doc_of_attributes attrs =
  let doc_loc_id = "ocaml.doc" in (* not exported ! *)
  let open Parsetree in
  try
#if OCAML_VERSION >= "4.08"
  match List.find (fun {attr_name = {Location.txt}} -> txt = doc_loc_id) attrs with
  | {attr_payload = PStr [{pstr_desc = Pstr_eval ({pexp_desc},_)}]} ->
#else
  match List.find (fun ({Location.txt},_) -> txt = doc_loc_id) attrs with
  | _, PStr [{pstr_desc = Pstr_eval ({pexp_desc},_)}] ->
#endif
      (match pexp_desc with
#if OCAML_VERSION >= "4.03"
       | Pexp_constant (Pconst_string (s,_)) -> Some s
#else
       | Pexp_constant (Const_string (s,_)) -> Some s
#endif
       | _ -> debug "Unexpected ocaml.doc docstring format"; None)
  | _ -> None
  with Not_found -> None

let trie_of_type_decl ?comments info ty_decl =
  match ty_decl.Types.type_kind with
  | Types.Type_abstract -> [], comments
  | Types.Type_open -> [], comments
  | Types.Type_record (fields,_repr) ->
      List.map
        (fun { Types.ld_id; ld_type; ld_attributes } ->
          let ty = Printtyp.tree_of_typexp false ld_type in
          let ty =
            Outcometree.Osig_type (Outcometree.({
                otype_name    = "";
                otype_params  = [];
                otype_type    = ty;
                otype_private = Asttypes.Public;
  #if OCAML_VERSION >= "4.03"
                otype_immediate = false;
    #if OCAML_VERSION >= "4.04"
                otype_unboxed = false;
    #endif
  #endif
                otype_cstrs   = []; }), Outcometree.Orec_not)
          in
          let doc = doc_of_attributes ld_attributes in
          let id_name = Ident.name ld_id in
          string_to_key id_name,
          Trie.create ~value:{
            path = info.path;
            orig_path = info.path;
            kind = Field info;
            name = id_name;
            ty = Some ty;
            loc_sig = info.loc_sig;
            loc_impl = info.loc_impl;
            doc = lazy doc;
            file = info.file;
          } ())
        fields,
      comments
  | Types.Type_variant variants ->
      List.map
        (fun { Types.cd_id; cd_args; cd_attributes } ->
          let ty =
            let params = match cd_args with
#if OCAML_VERSION >= "4.03"
              | Cstr_tuple [] -> Outcometree.Otyp_sum []
              | Cstr_tuple (param::_ as l) ->
                     Printtyp.tree_of_typexp false
                       { Types. desc = Types.Ttuple l;
                         level = param.Types.level;
#if OCAML_VERSION >= "4.08"
                         scope = 0;
#elif OCAML_VERSION >= "4.07"
                         scope = None;
#endif
                         id = param.Types.id }
              | Cstr_record params ->
                  Outcometree.Otyp_record (
                    List.map
                      (fun l ->
                         (Ident.name l.Types.ld_id,
                          l.ld_mutable = Mutable,
                          Printtyp.tree_of_typexp false l.ld_type)
                      )
                      params)
#else
              | [] -> Outcometree.Otyp_sum []
              | param::_ as l ->
                     Printtyp.tree_of_typexp false
                       { Types. desc = Types.Ttuple l;
                         level = param.Types.level;
                         id = param.Types.id }
#endif
            in
            Outcometree.Osig_type (Outcometree.({
                otype_name    = "";
                otype_params  = [];
                otype_type    = params;
                otype_private = Asttypes.Public;
  #if OCAML_VERSION >= "4.03"
                otype_immediate = false;
    #if OCAML_VERSION >= "4.04"
                otype_unboxed = false;
    #endif
  #endif
                otype_cstrs   = []; }), Outcometree.Orec_not)
          in
          let doc = doc_of_attributes cd_attributes in
          let id_name = Ident.name cd_id in
          string_to_key id_name,
          Trie.create ~value:{
            path = info.path;
            orig_path = info.path;
            kind = Variant info;
            name = id_name;
            ty = Some ty;
            loc_sig = info.loc_sig;
            loc_impl = info.loc_impl;
            doc = lazy doc;
            file = info.file;
          } ())
        variants,
      comments

(** Implements looking up a module path in the parents list *)
let lookup_parents (parents:parents) path sig_path =
  let sig_key, path_key = match sig_path with
    | hd::tl ->
        modpath_to_key [hd], modpath_to_key tl
    | [] -> assert false
  in
  let rec lookup = function
    | [] ->
        if debug_enabled then
          debug "WARN: Module or sig reference %s not found a %s\n"
            (modpath_to_string sig_path)
            (modpath_to_string path);
        Trie.empty
    | (parentpath, lazy t) :: parents ->
        let s = Trie.sub t sig_key in
        if s = Trie.empty then lookup parents else
          let s = Trie.sub s path_key in
          let rewrite_path =
            fix_path_prefix (List.length parentpath + List.length sig_path) path
          in
          Trie.map (fun _k v -> rewrite_path v) s
  in
  lookup parents

let rec path_of_ocaml = function
  | Path.Pident id -> [Ident.name id]
#if OCAML_VERSION >= "4.08"
  | Path.Pdot (path, s) -> path_of_ocaml path @ [s]
#else
  | Path.Pdot (path, s, _) -> path_of_ocaml path @ [s]
#endif
  | Path.Papply (p1, _p2) -> path_of_ocaml p1

let rec trie_of_sig_item
    ?comments ?srcpath implloc_trie (parents:parents) (orig_file:orig_file)
    path sig_item next
  =
  let id = id_of_sig_item sig_item in
  let loc = with_path_loc ?srcpath (loc_of_sig_item sig_item) in
  let nextloc = match next with
    | None -> Location.none
    | Some n -> with_path_loc ?srcpath (loc_of_sig_item n)
  in
  let doc, comments =
    match doc_of_attributes (attrs_of_sig_item sig_item), comments with
    | Some s, _ -> lazy (Some s), comments
    | None, None -> lazy None, None
    | None, Some comments ->
        let assoc = lazy (
          associate_comment (Lazy.force comments) loc nextloc
        ) in
        lazy (fst (Lazy.force assoc)),
        Some (lazy (snd (Lazy.force assoc)))
  in
  let ty = Some (ty_of_sig_item sig_item) in
  let kind = kind_of_sig_item sig_item in
  let loc_sig = lazy loc in
  let loc_impl = lazy (match implloc_trie with
      | lazy None -> loc
      | lazy (Some t) ->
          try
            let path = List.tl path @ [Ident.name id] in
            let key = modpath_to_key ~enddot:false path in
            let c =
              List.find (has_kind kind) (Trie.find_all t key)
            in
            Lazy.force c.loc_impl
          with Not_found -> Location.none
    ) in
  let info = {path; orig_path = path; kind; name = Ident.name id; ty;
              loc_sig; loc_impl; doc; file = orig_file}
  in
  let siblings, comments = (* read fields / variants ... *)
    match sig_item with
#if OCAML_VERSION >= "4.08"
    | Types.Sig_type (_id,descr,_is_rec, _) ->
#else
    | Types.Sig_type (_id,descr,_is_rec) ->
#endif
        trie_of_type_decl ?comments info descr
    | _ -> [], comments
  in
  (* ignore functor arguments *)
  let rec sig_item_contents = function
    | Types.Sig_module
#if OCAML_VERSION >= "4.08"
        (id, presence,
         ({Types.md_type = Types.Mty_functor (_,_,s)} as funct),
         is_rec, visibility)
      ->
        let funct = {funct with Types.md_type = s} in
        sig_item_contents (Types.Sig_module (id, presence, funct, is_rec, visibility))
#else
        (id,
         ({Types.md_type = Types.Mty_functor (_,_,s)} as funct),
         is_rec)
      ->
        let funct = {funct with Types.md_type = s} in
        sig_item_contents (Types.Sig_module (id, funct, is_rec))
#endif
    | Types.Sig_modtype
#if OCAML_VERSION >= "4.08"
      (id, ({Types.mtd_type = Some (Types.Mty_functor (_,_,s))} as funct), visibility)
      ->
        let funct = {funct with Types.mtd_type = Some s} in
        sig_item_contents (Types.Sig_modtype (id, funct, visibility))
#else
      (id, ({Types.mtd_type = Some (Types.Mty_functor (_,_,s))} as funct))
      ->
        let funct = {funct with Types.mtd_type = Some s} in
        sig_item_contents (Types.Sig_modtype (id, funct))
#endif
    | si -> si
  in
  (* read module / class contents *)
  let children, comments =
    match sig_item_contents sig_item with
  #if OCAML_VERSION >= "4.08"
    | Types.Sig_module (id,_,{ Types.md_type = Types.Mty_signature sign },_,_)
    | Types.Sig_modtype (id,{ Types.mtd_type = Some (Types.Mty_signature sign) },_)
  #else
    | Types.Sig_module (id,{ Types.md_type = Types.Mty_signature sign },_)
    | Types.Sig_modtype (id,{ Types.mtd_type = Some (Types.Mty_signature sign) })
  #endif
      ->
        let path = path @ [Ident.name id] in
        let children_comments = lazy (
          foldl_next
            (fun (t,comments) sign next ->
               let chlds,comments =
                 trie_of_sig_item ?comments ?srcpath implloc_trie
                   ((path, lazy t) :: parents) orig_file path sign next
               in
               List.fold_left Trie.append t chlds, comments)
            (Trie.empty,comments)
            sign
        ) in
        let children = lazy (fst (Lazy.force children_comments)) in
        let comments = match comments, children_comments with
          | None, _ -> None
          | Some _, lazy (_, comments) -> comments
        in
        children, comments
    | Types.Sig_module (
        _,
  #if OCAML_VERSION >= "4.08"
        _,
  #endif
        { Types.md_type =
            Types.Mty_ident sig_ident
  #if OCAML_VERSION >= "4.04" && OCAML_VERSION < "4.08"
        | Types.Mty_alias (_, sig_ident)
  #else
        | Types.Mty_alias sig_ident
  #endif
        },_
  #if OCAML_VERSION >= "4.08"
      ,_
  #endif
    )
  | Types.Sig_modtype (_,{ Types.mtd_type =
                               Some ( Types.Mty_ident sig_ident
  #if OCAML_VERSION >= "4.04" && OCAML_VERSION < "4.08"
                                    | Types.Mty_alias (_, sig_ident)
  #else
                                    | Types.Mty_alias sig_ident
  #endif
                                    ) }
  #if OCAML_VERSION >= "4.08"
        ,_
  #endif
    ) ->
        let sig_path = path_of_ocaml sig_ident in
        let children = lazy (
          (* Only keep the children, don't override the module reference *)
          Trie.graft_lazy Trie.empty []
            (lazy (lookup_parents parents (path@[Ident.name id]) sig_path))
        ) in
        children, comments
  #if OCAML_VERSION >= "4.08"
    | Types.Sig_class (id,{Types.cty_type=cty},_,_)
    | Types.Sig_class_type (id,{Types.clty_type=cty},_,_)
  #else
    | Types.Sig_class (id,{Types.cty_type=cty},_)
    | Types.Sig_class_type (id,{Types.clty_type=cty},_)
  #endif
      ->
        let rec get_clsig = function
          | Types.Cty_constr (_,_,cty) | Types.Cty_arrow (_,_,cty) ->
              get_clsig cty
          | Types.Cty_signature clsig -> clsig
        in
        let clsig = get_clsig cty in
        let path = path@[Ident.name id] in
        let (fields, _) =
          Ctype.flatten_fields (Ctype.object_fields clsig.Types.csig_self)
        in
        lazy (List.fold_left (fun t (lbl,_,ty_expr) ->
            if lbl = "*dummy method*" then t else
              let _ = Printtyp.reset_and_mark_loops ty_expr in
              let ty = Printtyp.tree_of_typexp false ty_expr in
              let ty =
                Outcometree.Osig_type (Outcometree.({
                    otype_name    = "";
                    otype_params  = [];
                    otype_type    = ty;
                    otype_private = Asttypes.Public;
  #if OCAML_VERSION >= "4.03"
                    otype_immediate = false;
    #if OCAML_VERSION >= "4.04"
                    otype_unboxed = false;
    #endif
  #endif
                    otype_cstrs   = []; }), Outcometree.Orec_not)
              in
              Trie.add t (string_to_key lbl)
                { path = path;
                  orig_path = path;
                  kind = Method info;
                  name = lbl;
                  ty = Some ty;
                  loc_sig = loc_sig;
                  loc_impl = loc_impl;
                  doc = lazy None;
                  file = info.file })
          Trie.empty
          fields),
        comments
    | _ ->
        lazy Trie.empty, comments
  in
  let name = Ident.name id in
  if String.length name > 0 && name.[0] = '#' then [], comments
  else
    (string_to_key name,
     Trie.create
       ~value:info
       ~children:(lazy [dot, Lazy.force children])
       ())
    :: siblings,
    comments

(* These four functions go through the typedtree to extract includes *)
let rec lookup_trie_of_module_expr parents t path = function
  | Typedtree.Tmod_ident (incpath,{ Location.txt = _lid}) ->
      let incpath = path_of_ocaml incpath in
      debug "Including %s impl at %s\n" (modpath_to_string incpath) (modpath_to_string path);
      let parents = (path, lazy t) :: parents in
      let sub = lookup_parents parents path incpath in
      overriding_merge t sub
  | Typedtree.Tmod_constraint (e,_,_,_)
  (* | Typedtree.Tmod_apply (e,_,_) *) ->
      lookup_trie_of_module_expr parents t path e.mod_desc
  | Typedtree.Tmod_apply ({ mod_desc = Typedtree.Tmod_functor(id,_,_,f) },
                          { mod_desc = Typedtree.Tmod_ident (arg,_)
                                     | Typedtree.Tmod_constraint ({mod_desc = Typedtree.Tmod_ident (arg,_)},_,_,_)  },_) ->
      let id_name = Ident.name id in
      let t = lookup_trie_of_module_expr parents t path f.Typedtree.mod_desc in
      debug "Grafting %s at %s\n" id_name (modpath_to_string (path_of_ocaml arg));
      let functor_arg = lazy (lookup_parents parents (path_of_ocaml arg) path) in
      Trie.graft_lazy t (modpath_to_key [id_name]) functor_arg
  | _ -> t
let rec extract_includes_from_submodule_sig parents t path name = function
  | Typedtree.Tmty_signature sign ->
      let path = path @ [name] in
      let sub_includes = lazy (
        get_includes_sig ((path, lazy t) :: parents)
          (Trie.sub t (modpath_to_key [name])) path sign
      ) in
      Trie.graft_lazy t (modpath_to_key [name]) sub_includes
  | Typedtree.Tmty_functor (_,_,_,e)
  | Typedtree.Tmty_with (e,_) ->
      extract_includes_from_submodule_sig parents t path name e.Typedtree.mty_desc
  | _ -> t
and get_includes_impl parents t path ttree_struct =
  let rec extract_submodule_impl t name = function
    | Typedtree.Tmod_structure str ->
        let path = path @ [name] in
        let sub_includes = lazy (
          get_includes_impl ((path, lazy t) :: parents)
            (Trie.sub t (modpath_to_key [name])) path str
        ) in
        Trie.graft_lazy t (modpath_to_key [name]) sub_includes
    (* | Typedtree.Tmod_functor (arg_id,_,arg_t,e) *)
    | Typedtree.Tmod_apply ({ mod_desc = Typedtree.Tmod_functor(id,_,_,f) },
                            { mod_desc = Typedtree.Tmod_ident (arg,_)
                                       | Typedtree.Tmod_constraint ({mod_desc = Typedtree.Tmod_ident (arg,_)},_,_,_)  },_) ->
        let id_name = Ident.name id in
        debug "Grafting %s at %s\n" id_name (modpath_to_string (path_of_ocaml arg));
        let functor_arg = lazy (
          lookup_parents
            ((path, lazy t)::parents) (path_of_ocaml arg) (path@[name])
        ) in
        extract_submodule_impl
          (Trie.graft_lazy t (modpath_to_key [id_name]) functor_arg)
          name f.Typedtree.mod_desc
    | Typedtree.Tmod_functor (_,_,_,e)
    | Typedtree.Tmod_constraint (e,_,_,_) ->
        extract_submodule_impl t name e.Typedtree.mod_desc
    | _ -> t
  in
  List.fold_left (fun t struc_item ->
      match struc_item.Typedtree.str_desc with
#if OCAML_VERSION >= "4.08"
      | Typedtree.Tstr_include
          { Typedtree.incl_mod = { Typedtree.mod_desc = e }} ->
#else
      | Typedtree.Tstr_include
          { Typedtree.incl_mod = { Typedtree.mod_desc = e }} ->
#endif
          lookup_trie_of_module_expr parents t path e
#if OCAML_VERSION >= "4.08"
      | Typedtree.Tstr_open
          Typedtree.{ open_expr = { mod_desc = Tmod_ident (p, _loc) } }
          (* TODO: handle the other new open cases *)
#else
      | Typedtree.Tstr_open
          { Typedtree.open_path = p }
#endif
        ->
          let sub = lookup_parents ((path, lazy t) :: parents) path (path_of_ocaml p) in
          overriding_merge t sub
      | Typedtree.Tstr_module
          { Typedtree.mb_id = id; mb_expr = { Typedtree.mod_desc } } ->
          extract_submodule_impl t (Ident.name id) mod_desc
      | Typedtree.Tstr_recmodule l ->
          List.fold_left
            (fun t { Typedtree.mb_id; mb_expr = { Typedtree.mod_desc } } ->
               extract_submodule_impl t (Ident.name mb_id) mod_desc)
            t l
      | Typedtree.Tstr_modtype
          { Typedtree.mtd_id = id; mtd_type = Some { Typedtree.mty_desc = e } } ->
          extract_includes_from_submodule_sig parents t path (Ident.name id) e
      | _ -> t)
    t ttree_struct.Typedtree.str_items
and get_includes_sig parents t path ttree_sig =
  let rec extract_includes t = function
    | Typedtree.Tmty_ident (incpath,_) ->
        let incpath = path_of_ocaml incpath in
        debug "Including %s sig at %s\n" (modpath_to_string incpath) (modpath_to_string path);
        let parents = (path, lazy t) :: parents in
        let sub = lookup_parents parents path incpath in
        overriding_merge t sub
    | Typedtree.Tmty_with (e,_) ->
        extract_includes t e.Typedtree.mty_desc
    | Typedtree.Tmty_typeof e ->
        lookup_trie_of_module_expr parents t path
          e.Typedtree.mod_desc
    | _ -> t
  in
  List.fold_left (fun t sig_item ->
      match sig_item.Typedtree.sig_desc with
      | Typedtree.Tsig_include
          { Typedtree.incl_mod = { Typedtree.mty_desc = e }} ->
          extract_includes t e
      | Typedtree.Tsig_module
          { Typedtree.md_id = id ; md_type = { Typedtree.mty_desc } }
      | Typedtree.Tsig_modtype
          { Typedtree.mtd_id = id; mtd_type = Some { Typedtree.mty_desc } } ->
          extract_includes_from_submodule_sig parents t path
            (Ident.name id) mty_desc
      | Typedtree.Tsig_recmodule l ->
          List.fold_left
            (fun t { Typedtree.md_id; md_type = { Typedtree.mty_desc } } ->
               extract_includes_from_submodule_sig parents t path
                 (Ident.name md_id) mty_desc)
            t l
      | _ -> t)
    t ttree_sig.Typedtree.sig_items

let add_locs ~locs t =
  Trie.map (fun path info ->
      let loc_info = lazy (
        List.find (has_kind info.kind) (Trie.find_all locs path)
      ) in
      let lookup fld none =
        let loc = Lazy.force (fld info) in
        if loc = none
        then try Lazy.force (fld (Lazy.force loc_info)) with Not_found -> none
        else loc
      in
      { info with
        loc_sig = lazy (lookup (fun i -> i.loc_sig) Location.none);
        loc_impl = lazy (lookup (fun i -> i.loc_impl) Location.none);
        doc = lazy (lookup (fun i -> i.doc) None);
      }
    ) t

let cmt_includes parents t path cmt_contents =
  match cmt_contents.Cmt_format.cmt_annots with
  | Cmt_format.Implementation impl ->
      get_includes_impl parents t path impl
  | Cmt_format.Interface sign ->
      get_includes_sig parents t path sign
  | _ -> Trie.empty

(* Can work in a subtree (t doesn't have to be the root) *)
let qualify_type_idents parents t =
  let qualify _key id =
    let rel_path =
      let rec rm_pfx parents path = match parents,path with
        | [_root], path -> path
        | _::parents, _::path -> rm_pfx parents path
        | _ -> assert false
      in
      rm_pfx parents id.path
    in
    let qualify_ty ty =
      let parents =
        let rec aux acc path = match acc,path with
          | ((pfx, parent) :: _), modl::r ->
              let t = lazy (
                Trie.sub (Lazy.force parent) (string_to_key (modl) @ [dot])
              ) in
              aux ((pfx @ [modl], t) :: acc) r
          | _ -> acc
        in
        aux parents rel_path
      in
      qualify_ty_in_sig_item parents ty
    in
    { id with ty = match id.ty with Some ty -> Some (qualify_ty ty)
                                  | None -> None }
  in
  Trie.map qualify t

let cmt_sign cmt_contents =
  match cmt_contents.Cmt_format.cmt_annots with
  | Cmt_format.Implementation {Typedtree.str_type = sign; _}
  | Cmt_format.Interface {Typedtree.sig_type = sign; _}
  | Cmt_format.Packed (sign, _)
    -> Some sign
  | _ -> None

let protect_read reader f =
  try reader f with
  | Cmt_format.Error _ | Cmi_format.Error _ ->
      raise (Bad_format f)

(* Look for a cmt file for the purpose of loading implementation locations.
   (assuming other information is already loaded eg. from the cmti). *)
let lookup_loc_impl orig_file =
  match orig_file with
  | Cmt _ -> None
  | Cmi f | Cmti f ->
      let cmt = Filename.chop_extension f ^ ".cmt" in
      if not (Sys.file_exists cmt) then None else Some cmt

let load_loc_impl parents filename cmt_contents =
  debug " -Registering %s (for implementation locations)..." filename;
  let chrono = timer () in
  match cmt_sign cmt_contents with
  | Some sign ->
      let srcpath = cmt_contents.Cmt_format.cmt_builddir in
      let t =
        foldl_next
          (fun t sig_item next ->
             let chld, _comments =
               trie_of_sig_item ~srcpath (lazy None) parents (Cmt filename)
                 [] sig_item next
             in
             List.fold_left Trie.append t chld)
          Trie.empty
          sign
      in
      debug " %.3fs\n%!" (chrono());
      let includes = cmt_includes parents t [] cmt_contents in
      let t = add_locs ~locs:includes t in
      Some t
  | _ ->
      debug " %.3fs\n%!" (chrono());
      None

let load_cmi ?(qualify=false) root t modul orig_file =
  Trie.map_subtree t (string_to_key modul)
    (fun t ->
       let file = orig_file_name orig_file in
       let info = lazy (
         let chrono = timer () in
         let info = protect_read Cmi_format.read_cmi file in
         debug " %.3fs\n" (chrono());
         info
       ) in
       let impl_cmt = lazy (
         match lookup_loc_impl orig_file with
         | Some cmt ->
             debug "Loading %s (for implementation locations)..." cmt;
             let chrono = timer () in
             let cmt_contents = protect_read Cmt_format.read_cmt cmt in
             debug " %.3fs\n" (chrono());
             Some (cmt, cmt_contents)
         | None -> None
       ) in
       let children = lazy (
        let info = Lazy.force info in
        debug " -Registering %s..." file;
        let chrono = timer () in
        let rec implloc_trie = lazy (
          match Lazy.force impl_cmt with
          | Some (file, info) ->
              load_loc_impl [[modul], lazy_t; [], root] file info
          | None -> None
        ) and lazy_t = lazy (
          foldl_next
            (fun t sig_item next ->
               let parents = [[modul], lazy t; [], root] in
               let chld, _comments =
                 trie_of_sig_item implloc_trie parents
                   orig_file [modul] sig_item next
               in
               List.fold_left Trie.append t chld)
            Trie.empty
            info.Cmi_format.cmi_sign
        ) in
        let t = Lazy.force lazy_t in
        debug " %.3fs ; done\n%!" (chrono());
        t
      ) in
      let t =
        Trie.add t [] {
          path = [];
          orig_path = [];
          kind = Module;
          name = modul;
          ty = None;
          loc_sig = Lazy.from_val Location.none;
          loc_impl = Lazy.from_val Location.none;
          doc = lazy None;
          file = orig_file;
        }
      in
      let children =
        if qualify then lazy (
          qualify_type_idents [[modul], children; [], root]
            (Lazy.force children)
        ) else children
      in
      Trie.graft_lazy t [dot] children)

let load_cmt ?(qualify=false) root t modul orig_file =
  Trie.map_subtree t (string_to_key modul)
    (fun t ->
       let cmt_file = orig_file_name orig_file in
       let info = lazy (
         debug "Loading %s..." cmt_file;
         let chrono = timer () in
         let info = protect_read Cmt_format.read_cmt cmt_file in
         debug " %.3fs\n" (chrono());
         info
       ) in
       let impl_cmt = lazy (
         match lookup_loc_impl orig_file with
         | Some cmt ->
             debug "Loading %s (for implementation locations)..." cmt;
             let chrono = timer () in
             let cmt_contents = protect_read Cmt_format.read_cmt cmt in
             debug " %.3fs\n" (chrono());
             Some (cmt, cmt_contents)
         | None -> None
       ) in
       let children = lazy (
         let info = Lazy.force info in
         debug " -Registering %s..." cmt_file;
         let chrono = timer () in
         let comments = Some (Lazy.from_val info.Cmt_format.cmt_comments) in
         let rec implloc_trie = lazy (
           match Lazy.force impl_cmt with
           | Some (file, info) ->
               load_loc_impl [[modul], lazy_t; [], root] file info
           | None -> None
         ) and lazy_t = lazy (
           match cmt_sign info with
           | Some sign ->
               let srcpath = info.Cmt_format.cmt_builddir in
               let t, _trailing_comments =
                 foldl_next
                   (fun (t,comments) sig_item next ->
                      let parents = [[modul], lazy t; [], root] in
                      let chld, comments =
                        trie_of_sig_item ?comments ~srcpath
                          implloc_trie parents orig_file
                          [modul] sig_item next
                      in
                      List.fold_left Trie.append t chld, comments)
                   (Trie.empty, comments)
                   sign
               in
               t
           | None -> Trie.empty
         ) in
         let t = Lazy.force lazy_t in
         debug " %.3fs\n%!" (chrono());
         t
       ) in
       let children = lazy (
         let includes =
           cmt_includes [[modul], children; [], root]
             t [] (Lazy.force info)
         in
         add_locs ~locs:includes (Lazy.force children)
       ) in
       let loc_sig, loc_impl =
         let of_info i = match i.Cmt_format.cmt_sourcefile with
           | Some f -> Location.in_file f
           | None -> Location.none
         in
         match orig_file with
         | Cmi _ | Cmti _ ->
             lazy (of_info (Lazy.force info)),
             lazy (match Lazy.force impl_cmt with
                 | Some (_,i) -> of_info i
                 | None -> Location.none)
         | Cmt _ ->
             let l = lazy (of_info (Lazy.force info)) in
             l, l
       in
       let t =
         Trie.add t [] {
           path = [];
           orig_path = [];
           kind = Module;
           name = modul;
           ty = None;
           loc_sig;
           loc_impl;
           doc = lazy None;
           file = orig_file;
         }
       in
       let children =
         if qualify then lazy (
           qualify_type_idents [[modul], children; [], root]
             (Lazy.force children)
         ) else children
       in
       Trie.graft_lazy t [dot] children)

let debug_file_counter = ref 0
let debug_dir_counter = ref 0

let load_file root t modul f =
  incr debug_file_counter;
  match f with
  | Cmi _ -> load_cmi root t modul f
  | Cmt _ | Cmti _ -> load_cmt root t modul f

let load_files t dirfiles =
  let split_filename file =
    try
      let i = String.rindex file '.' in
      let len = String.length file in
#if OCAML_VERSION >= "4.03"
      let modul = String.capitalize_ascii (String.sub file 0 i) in
      let ext = String.lowercase_ascii (String.sub file (i+1) (len-i-1)) in
#else
      let modul = String.capitalize (String.sub file 0 i) in
      let ext = String.lowercase (String.sub file (i+1) (len-i-1)) in
#endif
      modul, ext
    with Not_found -> file, ""
  in
  let sort_modules acc (dir,file) =
    let reg base = Trie.add acc (string_to_key base) in
    match split_filename file with
    | base, "cmi" -> reg base (Cmi (Filename.concat dir file))
    | base, "cmt" -> reg base (Cmt (Filename.concat dir file))
    | base, "cmti" -> reg base (Cmti (Filename.concat dir file))
    | _ -> acc
  in
  let modules =
    List.fold_left sort_modules Trie.empty dirfiles
  in
  let rec root = lazy (
    Trie.fold0 (fun t modul files ->
        match files with
        | [] -> t
        | f1::fs ->
            (* Load by order of priority:
               - first cmti, more info than cmi, ocamldocs, and doesn't expose
                 private values
               - then cmt, it means there is no mli, everything is exposed
               - then cmi, has the interface if not much more *)
            let choose_file f1 f2 = match f1,f2 with
              | (Cmti _ as f), _ | _, (Cmti _ as f)
              | (Cmt _ as f), _ | _, (Cmt _ as f)
              | (Cmi _ as f), _ -> f
            in
            let file = List.fold_left choose_file f1 fs in
            let modul = key_to_string modul in
            load_file root t modul file)
      modules
      t
  )
  in Lazy.force root

let load_dirs t dirs =
  let dirfiles =
    List.fold_left (fun acc dir ->
        incr debug_dir_counter;
        let files =
          List.rev_map (fun f -> dir, f) (Array.to_list (Sys.readdir dir))
        in
        List.rev_append files acc)
      []
      (List.rev dirs)
  in
  load_files t dirfiles

let load paths =
  let t = Trie.create () in
  let t =
    List.fold_left
      (fun t info ->
         Trie.add t (string_to_key info.name) info)
      t
      IndexPredefined.all
  in
  let chrono = timer () in
  let t = load_dirs t paths in
  debug "Modules directory loaded in %.3fs (%d files in %d directories)...\n"
    (chrono()) !debug_file_counter !debug_dir_counter;
#if OCAML_VERSION >= "4.07"
  open_module ~cleanup_path:true t ["Stdlib"]
#else
  open_module ~cleanup_path:true t ["Pervasives"]
#endif

let fully_open_module ?(cleanup_path=false) t path =
  let base_path = match path with
    | m::_ -> string_to_key m
    | [] -> []
  in
  (* Merge trying to keep the documentation if the new trie has none *)
  let merge intfs impls =
    let keep_intf info =
      try
        let intf = List.find (has_kind info.kind) intfs in
        let doc = lazy (match info.doc with
            | lazy None -> Lazy.force intf.doc
            | lazy some -> some)
        in
        let loc_sig = intf.loc_sig in
        { info with doc; loc_sig }
      with Not_found -> info
    in
    List.map keep_intf impls
  in
  let tpath = modpath_to_key path in
  let mod_trie = Trie.sub t tpath in
  let mod_trie =
    try match (Trie.find t base_path).file with
      | Cmti f | Cmi f ->
          let f = Filename.chop_extension f ^ ".cmt" in
          if not (Sys.file_exists f) then mod_trie
          else
            let dir,base = Filename.dirname f, Filename.basename f in
            let t = load_files Trie.empty [dir,base] in
            let t = Trie.sub t tpath in
            Trie.merge ~values:merge mod_trie t
      | Cmt _ -> mod_trie
    with Not_found -> mod_trie
  in
  (* cleanup and merge at root (cf. open_module) *)
  let mod_trie =
    if cleanup_path then
      let pathlen = List.length path in
      Trie.map (fun _key -> fix_path_prefix pathlen []) mod_trie
    else mod_trie
  in
  overriding_merge t mod_trie

let add_file t file =
  let dir, file = Filename.dirname file, Filename.basename file in
  load_files t [dir,file]
