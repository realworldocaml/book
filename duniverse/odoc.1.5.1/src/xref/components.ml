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
open Names

(* Sets and maps for components *)

module SSet = Set.Make(String)

module SMap = struct

  include Map.Make(String)

  let filter_item name pred map =
    try
      let v = find name map in
        if pred v then map
        else remove name map
    with Not_found -> map

  let map_item name f map =
    try
      let v = find name map in
        add name (f v) map
    with Not_found -> map

end

module LMap = struct

  type 'a t = 'a list SMap.t

  let empty = SMap.empty

  let add name item map =
    try
      let items = SMap.find name map in
        SMap.add name (item :: items) map
    with Not_found ->
      SMap.add name [item] map

  (*
  let find name pred map =
    let items = SMap.find name map in
      List.find pred items
  *)

  let find_name name map =
    let items = SMap.find name map in
      match items with
      | [] -> raise Not_found
      | x :: _ -> x

  let map_find name pred map =
    let rec loop pred = function
      | [] -> raise Not_found
      | x :: l ->
          match pred x with
          | Some x -> x
          | None -> loop pred l
    in
    let items = SMap.find name map in
      loop pred items

  let fold f map acc =
    SMap.fold
      (fun name -> List.fold_right (f name))
      map acc

  let filter_item name pred map =
    try
      let items = SMap.find name map in
      let items = List.filter pred items in
        match items with
        | [] -> SMap.remove name map
        | _ -> SMap.add name items map
    with Not_found -> map

  let map_item name f map =
    try
      let items = SMap.find name map in
      let items = List.map f items in
        SMap.add name items map
    with Not_found -> map

end

(* Tables for caches *)
type ('a, 'b) tbl =
  { fresh: int -> ('a, 'b) tbl;
    find: 'a -> 'b;
    add: 'a -> 'b  -> unit; }

let make_tbl (type a) (equal : (a -> a -> bool) option)
           (hash : (a -> int) option) size =
  let make create find add =
    let rec fresh size =
      let t = create size in
      let find x = find t x in
      let add x y = add t x y in
        {fresh; find; add}
    in
      fresh size
  in
    match equal, hash with
    | None, None ->
        make (Hashtbl.create ?random:None) Hashtbl.find Hashtbl.add
    | _ ->
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
        let module Hash = struct
          type t = a
          let equal = equal
          let hash = hash
        end in
        let module Tbl = Hashtbl.Make(Hash) in
          make Tbl.create Tbl.find Tbl.add

(* Read labels from documentation *)

let documentation_labels acc doc =
  List.fold_left (fun acc element ->
    match element.Odoc_model.Location_.value with
    | `Heading (_, label, nested_text) ->
      let name = Identifier.name label in
      (name, nested_text)::acc
    | _ -> acc)
  acc doc

let comment_labels acc comment =
  match comment with
  | `Stop -> acc
  | `Docs doc -> documentation_labels acc doc

module rec Sig : sig

  type t

  val set_canonical : t -> (Path.Module.t * Reference.Module.t) option -> t

  val get_canonical : t -> (Path.Module.t * Reference.Module.t) option

  val set_hidden : t -> bool -> t

  val get_hidden : t -> bool

  val find_parent_module : string -> t -> Parent.module_

  val find_parent_apply : (Path.Module.t -> t) -> Path.Module.t ->
        t -> Parent.module_

  val find_parent_module_type : string -> t -> Parent.module_type

  val find_parent_signature : string -> t -> Parent.signature

  val find_parent_class_signature : string -> t -> Parent.class_signature

  val find_parent_datatype : string -> t -> Parent.datatype

  val find_parent_sig_or_type : string -> t -> Parent.sig_or_type

  val find_parent_subst : t -> Parent.subst

  val find_parent : string -> t -> Parent.any

  val find_module_element : string -> t -> Element.signature_module

  val find_apply_element : t -> Element.signature_module

  val find_module_type_element : string -> t -> Element.signature_module_type

  val find_type_element : string -> t -> Element.signature_type

  val find_constructor_element : string -> t -> Element.signature_constructor

  val find_field_element : string -> t -> Element.signature_field

  val find_extension_element : string -> t -> Element.signature_extension

  val find_exception_element : string -> t -> Element.signature_exception

  val find_value_element : string -> t -> Element.signature_value

  val find_class_element : string -> t -> Element.signature_class

  val find_class_type_element : string -> t -> Element.signature_class_type

  val find_label_element : string -> t -> Element.signature_label

  val find_element : string -> t -> Element.signature

  val find_section_title : string -> t -> Odoc_model.Comment.link_content

  val lookup_module : string -> t -> t

  val lookup_argument : int -> t -> t

  val lookup_apply : (Path.Module.t -> t) -> Path.Module.t ->
        t -> t

  val lookup_module_type  : string -> t -> t

  val lookup_class_type : string -> t -> ClassSig.t

  val lookup_datatype : string -> t -> Datatype.t

  type signature

  val empty : signature

  val add_module : string -> t -> signature -> signature

  val add_module_type : string -> t -> signature -> signature

  val add_datatype : string -> Datatype.t -> signature -> signature

  val add_class : string -> ClassSig.t -> signature -> signature

  val add_class_type : string -> ClassSig.t -> signature -> signature

  val add_element : string -> Element.signature -> signature -> signature

  val add_documentation : Odoc_model.Comment.docs -> signature -> signature

  val add_comment : Odoc_model.Comment.docs_or_stop -> signature -> signature

  val include_ : t -> signature -> signature

  val modules : t -> (ModuleName.t * t) list

  val module_types : t -> (ModuleTypeName.t * t) list

  val path : (Path.ModuleType.t -> t) -> Path.ModuleType.t -> t

  val alias : (Path.Module.t -> t) -> Path.Module.t -> t

  val signature : ('b -> signature) -> 'b -> t

  val functor_ : (Root.t -> Root.t -> bool) option -> (Root.t -> int) option ->
                 Identifier.Module.t -> t -> t -> t

  val generative : t -> t

  val abstract : t

  val unresolved : t

  val with_module : Fragment.Module.t -> t -> t -> t

  val with_module_subst : Fragment.Module.t -> t -> t

  val with_type_subst : Fragment.Type.t -> t -> t

end = struct

  type term =
    | Path of Path.ModuleType.t * bool
    | Alias of Path.Module.t * bool
    | WithModule of expr * Fragment.Module.t * t
    | WithModuleSubst of expr * Fragment.Module.t
    | WithTypeSubst of expr * Fragment.Type.t

  and expr =
    { term : term;
      expansion : t Lazy.t; }

  and functor_ =
    { id : Identifier.Module.t;
      arg : t;
      res : t;
      cache : (Path.Module.t, t) tbl; }

  and signature =
    { modules: t SMap.t;
      module_types: t SMap.t;
      class_signatures: ClassSig.t SMap.t;
      types: Element.signature_type SMap.t;
      parents: Parent.any LMap.t;
      elements: Element.signature LMap.t;
      section_titles: Odoc_model.Comment.link_content SMap.t; }

  and body =
    | Expr of expr
    | Sig of signature Lazy.t
    | Functor of functor_
    | Generative of t
    | Abstract
    | Unresolved

  and t =
    { canonical : (Path.Module.t * Reference.Module.t) option;
      hidden : bool;
      body : body }

  let set_canonical t canonical = { t with canonical }

  let set_hidden t hidden = { t with hidden }

  let get_canonical t = t.canonical

  let get_hidden t = t.hidden

  let mkExpr ex = { canonical = None; body = Expr ex; hidden = false }

  let mkSig sg = { canonical = None; body = Sig sg; hidden = false }

  let mkFunctor fn = { canonical = None; body = Functor fn; hidden = false }

  let generative t = { canonical = None; body = Generative t; hidden = false }

  let abstract = { canonical = None; body = Abstract; hidden = false }

  let unresolved = { canonical = None; body = Unresolved; hidden = false }

  let rec lift_find f x t =
    match t.body with
    | Expr expr -> begin
        match expr.term with
        | Path(_, true)
        | Alias(_, true) -> raise Not_found
        | Alias(_, false) -> begin
            let t = Lazy.force expr.expansion in
            match t.hidden with
            | false -> raise Not_found
            | true -> lift_find f x t
          end
        | _ -> lift_find f x (Lazy.force expr.expansion)
      end
    | Sig sg -> f x (Lazy.force sg)
    | Functor fn -> lift_find f x fn.res
    | Generative t -> lift_find f x t
    | Abstract -> raise Not_found
    | Unresolved -> raise Not_found

  let find_parent_module name t =
    let find name sg =
      `Module (SMap.find name sg.modules)
    in
      lift_find find name t

  let find_parent_module_type name t =
    let find name sg =
      `ModuleType (SMap.find name sg.module_types)
    in
      lift_find find name t

  let find_parent_signature name t =
    let find name sg =
      LMap.map_find name
        (function
          | `Module _ as x -> Some x
          | `ModuleType _ as x -> Some x
          | _ -> None)
        sg.parents
    in
      lift_find find name t

  let find_parent_class_signature name t =
    let find name sg =
      LMap.map_find name
        (function
          | `Class _ as x -> Some x
          | `ClassType _ as x -> Some x
          | _ -> None)
        sg.parents
    in
      lift_find find name t

  let find_parent_datatype name t =
    let find name sg =
      LMap.map_find name
        (function
          | `Datatype _ as x -> Some x
          | _ -> None)
        sg.parents
    in
      lift_find find name t

  let find_parent_sig_or_type name t =
    let find name sg =
      LMap.map_find name
        (function
          | `Module _ as x -> Some x
          | `ModuleType _ as x -> Some x
          | `Datatype _ as x -> Some x
          | _ -> None)
        sg.parents
    in
      lift_find find name t

  let rec find_parent_subst : t -> Parent.subst = fun t ->
    match t.body with
    | Expr expr -> begin
        match expr.term with
        | Path(p, true) -> Subst p
        | Alias(p, true) -> SubstAlias p
        | Alias(p, false) -> begin
            let t' = Lazy.force expr.expansion in
            match t'.hidden with
            | false -> SubstAlias p
            | true -> find_parent_subst t'
          end
        | _ -> find_parent_subst (Lazy.force expr.expansion)
      end
    | Sig _ -> raise Not_found
    | Functor fn -> find_parent_subst fn.res
    | Generative t -> find_parent_subst t
    | Abstract -> raise Not_found
    | Unresolved -> raise Not_found

  let find_parent name t =
    let find name sg = LMap.find_name name sg.parents in
      lift_find find name t

  let find_module_element name t =
    let find name sg =
      let t = SMap.find name sg.modules in
      `Module {
        Element.canonical = t.canonical;
        hidden = t.hidden;
      }
    in
      lift_find find name t

  let rec find_apply_element t =
    match t.body with
    | Expr expr -> begin
        match expr.term with
        | Path(_, true) | Alias(_, true) -> raise Not_found
        | Alias(_, false) -> begin
            let t = Lazy.force expr.expansion in
            match t.hidden with
            | false -> raise Not_found
            | true -> find_apply_element t
          end
        | _ -> find_apply_element (Lazy.force expr.expansion)
      end
    | Sig _ -> raise Not_found
    | Functor _ -> `Module { Element.canonical = None; hidden = false }
    | Generative _ -> raise Not_found
    | Abstract -> raise Not_found
    | Unresolved -> raise Not_found

  let find_module_type_element name t =
    let find name sg =
      if SMap.mem name sg.module_types then `ModuleType
      else raise Not_found
    in
      lift_find find name t

  let find_type_element name t =
    let find name sg = SMap.find name sg.types in
      lift_find find name t

  let find_constructor_element name t =
    let find name sg =
      LMap.map_find name
        (function
          | `Constructor _ as x -> Some x
          | `Extension as x -> Some x
          | `Exception as x -> Some x
          | _ -> None)
        sg.elements
    in
      lift_find find name t

  let find_field_element name t =
    let find name sg =
      LMap.map_find name
        (function
          | `Field _ as x -> Some x
          | _ -> None)
        sg.elements
    in
      lift_find find name t

  let find_extension_element name t =
    let find name sg =
      LMap.map_find name
        (function
          | `Extension as x -> Some x
          | `Exception as x -> Some x
          | _ -> None)
        sg.elements
    in
      lift_find find name t

  let find_exception_element name t =
    let find name sg =
      LMap.map_find name
        (function
          | `Exception as x -> Some x
          | _ -> None)
        sg.elements
    in
      lift_find find name t

  let find_value_element name t =
    let find name sg =
      LMap.map_find name
        (function
          | `Value as x -> Some x
          | _ -> None)
        sg.elements
    in
      lift_find find name t

  let find_class_element name t =
    let find name sg =
      match SMap.find name sg.types with
      | `Class as x -> x
      | _ -> raise Not_found
    in
      lift_find find name t

  let find_class_type_element name t =
    let find name sg =
      match SMap.find name sg.types with
      | `Class as x -> x
      | `ClassType as x -> x
      | _ -> raise Not_found
    in
      lift_find find name t

  let find_label_element name t =
    let find name sg =
      LMap.map_find name
        (function
          | `Label _ as x -> Some x
          | _ -> None)
        sg.elements
    in
      lift_find find name t

  let find_section_title name t =
    let find name sg = SMap.find name sg.section_titles in
      lift_find find name t

  let find_element name t =
    let find name sg = LMap.find_name name sg.elements in
      lift_find find name t

  let rec lookup_module name t =
    match t.body with
    | Expr expr -> lookup_module name (Lazy.force expr.expansion)
    | Sig sg -> begin
        try
          SMap.find name (Lazy.force sg).modules
        with Not_found -> unresolved
      end
    | Functor fn -> lookup_module name fn.res
    | Generative t -> lookup_module name t
    | Abstract -> unresolved
    | Unresolved -> unresolved

  let rec lookup_argument pos t =
    match t.body with
    | Expr expr -> lookup_argument pos (Lazy.force expr.expansion)
    | Sig _ -> unresolved
    | Functor fn ->
        if pos = 1 then fn.arg
        else lookup_argument (pos - 1) fn.res
    | Generative t ->
        if pos = 1 then unresolved
        else lookup_argument (pos - 1) t
    | Abstract -> unresolved
    | Unresolved -> unresolved

  let rec lookup_module_type name t =
    match t.body with
    | Expr expr -> lookup_module_type name (Lazy.force expr.expansion)
    | Sig sg -> begin
        try
          SMap.find name (Lazy.force sg).module_types
        with Not_found -> unresolved
      end
    | Functor fn -> lookup_module_type name fn.res
    | Generative t -> lookup_module_type name t
    | Abstract -> unresolved
    | Unresolved -> unresolved

  let rec lookup_class_type name t =
    match t.body with
    | Expr expr -> lookup_class_type name (Lazy.force expr.expansion)
    | Sig sg -> begin
        try
          SMap.find name (Lazy.force sg).class_signatures
        with Not_found -> ClassSig.unresolved
      end
    | Functor fn -> lookup_class_type name fn.res
    | Generative t -> lookup_class_type name t
    | Abstract -> ClassSig.unresolved
    | Unresolved -> ClassSig.unresolved

  let rec lookup_datatype name t =
    match t.body with
    | Expr expr -> lookup_datatype name (Lazy.force expr.expansion)
    | Sig sg -> begin
          try
            LMap.map_find name
              (function
                | `Datatype t -> Some t
                | _ -> None)
              (Lazy.force sg).parents
          with Not_found -> Datatype.unresolved
      end
    | Functor fn -> lookup_datatype name fn.res
    | Generative t -> lookup_datatype name t
    | Abstract -> Datatype.unresolved
    | Unresolved -> Datatype.unresolved

  let empty =
    { modules = SMap.empty;
      module_types = SMap.empty;
      class_signatures = SMap.empty;
      types = SMap.empty;
      parents = SMap.empty;
      elements = SMap.empty;
      section_titles = SMap.empty; }

  let add_module name md sg =
    let modules = SMap.add name md sg.modules in
    let parents = LMap.add name (`Module md) sg.parents in
    let elements =
      let md = `Module Element.{ canonical=md.canonical; hidden=md.hidden } in
      LMap.add name md sg.elements
    in
      {sg with modules; parents; elements}

  let add_module_type name mty sg =
    let module_types = SMap.add name mty sg.module_types in
    let parents = LMap.add name (`ModuleType mty) sg.parents in
    let elements = LMap.add name `ModuleType sg.elements in
      {sg with module_types; parents; elements}

  let add_datatype name decl sg =
    let types = SMap.add name `Type sg.types in
    let parents = LMap.add name (`Datatype decl) sg.parents in
    let elements =
      let add_element name (elem : Element.datatype) acc =
        let (`Constructor _ | `Field _ | `Label _ as elem) = elem in
          LMap.add name elem acc
      in
        LMap.fold add_element (Datatype.elements decl) sg.elements
    in
    let elements = LMap.add name `Type elements in
      {sg with types; parents; elements}

  let add_class name cl sg =
    let types = SMap.add name `Class sg.types in
    let class_signatures = SMap.add name cl sg.class_signatures in
    let parents = LMap.add name (`Class cl) sg.parents in
    let elements = LMap.add name `Class sg.elements in
      {sg with types; class_signatures; parents; elements}

  let add_class_type name clty sg =
    let types = SMap.add name `ClassType sg.types in
    let class_signatures = SMap.add name clty sg.class_signatures in
    let parents = LMap.add name (`ClassType clty) sg.parents in
    let elements = LMap.add name `ClassType sg.elements in
      {sg with types; class_signatures; parents; elements}

  let add_element name element sg =
    let elements = LMap.add name element sg.elements in
      {sg with elements}

  let add_documentation doc sg =
    let labels = documentation_labels [] doc in
    let add_label sg (label, txt) =
      let sg = add_element label (`Label None) sg in
      let section_titles = SMap.add label txt sg.section_titles in
      {sg with section_titles}
    in
      List.fold_left add_label sg labels

  let add_comment comment sg =
    let labels = comment_labels [] comment in
    let add_label sg (label, txt) =
      let sg = add_element label (`Label None) sg in
      let section_titles = SMap.add label txt sg.section_titles in
      {sg with section_titles}
    in
      List.fold_left add_label sg labels

  let strengthen_submodule path expansion name t =
    match t.body with
    | Unresolved -> t
    | Expr { term = Alias(p, b); _ } when b || not (Path.is_hidden (p :> Path.t)) -> t
    | _ ->
        let path = Path.module_ path name in
        let term = Alias(path, false) in
        let expansion = lazy (lookup_module (ModuleName.to_string name) (Lazy.force expansion)) in
        { t with body = Expr {term; expansion} }

  let rec strengthen_module path expansion t =
    if Path.is_hidden (path : Path.Module.t :> Path.t) then t else
    match t.body with
    | Expr { term; expansion = ex } -> begin
        let ex' = lazy (strengthen_module path expansion (Lazy.force ex)) in
        { t with body = Expr { term; expansion = ex' } }
      end
    | Sig sg ->
      let sg =
        lazy (
          let sg = Lazy.force sg in
          let modules = SMap.mapi (fun n -> strengthen_submodule path expansion (ModuleName.of_string n)) sg.modules in
          { sg with modules }
        )
      in
      { t with body = Sig sg }
    | Functor _ | Generative _ | Abstract | Unresolved -> t

  let rec include_ t sg =
    match t.body with
    | Expr expr -> include_ (Lazy.force expr.expansion) sg
    | Sig incl ->
        let incl = Lazy.force incl in
        let modules =
          SMap.fold SMap.add incl.modules sg.modules
        in
        let module_types =
          SMap.fold SMap.add incl.module_types sg.module_types
        in
        let class_signatures =
          SMap.fold SMap.add incl.class_signatures sg.class_signatures
        in
        let types =
          SMap.fold SMap.add incl.types sg.types
        in
        let parents =
          LMap.fold LMap.add incl.parents sg.parents
        in
        let elements =
          LMap.fold LMap.add incl.elements sg.elements
        in
        let section_titles =
          LMap.fold LMap.add incl.section_titles sg.section_titles
        in
          {modules; module_types; class_signatures;
           types; parents; elements; section_titles}
    | Functor _ | Generative _ | Abstract | Unresolved -> sg

  let rec modules t =
    match t.body with
    | Expr expr -> modules (Lazy.force expr.expansion)
    | Sig sg ->
        let sg = Lazy.force sg in
          SMap.bindings sg.modules |>
          List.map (fun (x,y) -> ModuleName.of_string x, y)
    | Functor _ | Generative _ | Abstract | Unresolved -> []

  let rec module_types t =
    match t.body with
    | Expr expr -> module_types (Lazy.force expr.expansion)
    | Sig sg ->
        let sg = Lazy.force sg in
          SMap.bindings sg.module_types |>
          List.map (fun (x,y) -> ModuleTypeName.of_string x, y)
    | Functor _ | Generative _ | Abstract | Unresolved -> []

  let path lookup p =
    let term = Path(p, false) in
    let expansion = lazy (lookup p) in
      mkExpr {term; expansion}

  let alias lookup p =
    let term = Alias(p, false) in
    let expansion =
      lazy (
        let ex = lookup p in
        strengthen_module p (Lazy.from_val ex) ex
      )
    in
      mkExpr {term; expansion}

  let signature f x = mkSig (lazy (f x))

  let functor_ equal hash id arg res =
    let equal =
      match equal with
      | None -> None
      | Some _equal -> Some Path.Module.equal
    in
    let hash =
      match hash with
      | None -> None
      | Some _hash -> Some Path.Module.hash
    in
    let cache = make_tbl equal hash 3 in
      mkFunctor {id; arg; res; cache}

  let replace_module name t sg =
    let modules = SMap.map_item name (fun _ -> t) sg.modules in
    let parents =
      LMap.map_item name
        (function
          | `Module _ -> `Module t
          | item -> item)
        sg.parents
    in
      {sg with modules; parents}

  let map_module name f sg =
    let modules = SMap.map_item name f sg.modules in
    let parents =
      LMap.map_item name
        (function
          | `Module t -> `Module (f t)
          | item -> item)
        sg.parents
    in
      {sg with modules; parents}

  let remove_module name sg =
    let modules = SMap.remove name sg.modules in
    let parents =
      LMap.filter_item name
        (function
          | `Module _ -> true
          | _ -> false)
        sg.parents
    in
    let elements =
      LMap.filter_item name
        (function `Module _ -> false | _ -> true) sg.elements
    in
      {sg with modules; parents; elements}

  let remove_datatype name sg =
    let types = SMap.filter_item name ((<>) `Type) sg.types in
    let parents =
      LMap.filter_item name
        (function
          | `Datatype _ -> true
          | _ -> false)
        sg.parents
    in
    let elements = LMap.filter_item name ((<>) `Type) sg.elements in
      {sg with types; parents; elements}

  let rec with_module frag eq t =
    match t.body with
    | Expr expr ->
        let term = WithModule(expr, frag, eq) in
        let expansion =
          lazy (with_module frag eq (Lazy.force expr.expansion))
        in
          mkExpr {term; expansion}
    | Sig sg ->
        let sg =
          lazy
            ( let sg = Lazy.force sg in
              let name, frag = Fragment.Module.split frag in
                match frag with
                | None -> replace_module name eq sg
                | Some frag -> map_module name (with_module frag eq) sg )
        in
          mkSig sg
    | Functor _ | Generative _ | Abstract | Unresolved -> t

  let rec with_module_subst frag t =
    match t.body with
    | Expr expr ->
        let term = WithModuleSubst(expr, frag) in
        let expansion =
          lazy (with_module_subst frag (Lazy.force expr.expansion))
        in
          mkExpr {term; expansion}
    | Sig sg ->
        let sg =
          lazy
            ( let sg = Lazy.force sg in
              let name, frag = Fragment.Module.split frag in
                match frag with
                | None -> remove_module name sg
                | Some frag -> map_module name (with_module_subst frag) sg )
        in
          mkSig sg
    | Functor _ | Generative _ | Abstract | Unresolved -> t

  let rec with_type_subst frag t =
    match t.body with
    | Expr expr ->
        let term = WithTypeSubst(expr, frag) in
        let expansion =
          lazy (with_type_subst frag (Lazy.force expr.expansion))
        in
          mkExpr {term; expansion}
    | Sig sg ->
        let sg =
          lazy
            ( let sg = Lazy.force sg in
              let name, frag = Fragment.Type.split frag in
                match frag with
                | None -> remove_datatype name sg
                | Some frag -> map_module name (with_type_subst frag) sg )
        in
          mkSig sg
    | Functor _ | Generative _ | Abstract | Unresolved -> t

  let module_type_substitution path expansion t =
    match t.body with
    | Abstract ->
        let term = Path(path, true) in
          mkExpr {term; expansion}
    | _ -> t

  let rec module_substitution path expansion t =
    match t.body with
    | Expr _ -> t
    | Sig sg ->
        let sg =
          lazy
            ( let sg = Lazy.force sg in
              let modules =
                SMap.mapi
                  (fun name body ->
                     let path = Path.module_ path (ModuleName.of_string name) in
                     let expansion =
                       lazy (lookup_module name (Lazy.force expansion))
                     in
                       module_substitution path expansion body)
               sg.modules
              in
              let module_types =
                SMap.mapi
                  (fun name body ->
                     let path = Path.module_type path (ModuleTypeName.of_string name) in
                     let expansion =
                       lazy (lookup_module_type name (Lazy.force expansion))
                     in
                       module_type_substitution path expansion body)
                  sg.module_types
              in
                {sg with modules; module_types} )
        in
          mkSig sg
    | Functor fn ->
        let res = module_substitution path expansion fn.res in
        let cache = fn.cache.fresh 3 in
          mkFunctor {fn with res; cache}
    | Generative body ->
        let body = module_substitution path expansion body in
          generative body
    | Abstract ->
        let term = Alias(path, true) in
          mkExpr {term; expansion}
    | Unresolved -> t

  let rec reduce_signature_ident id path = function
      | `Root _ -> None
      | `Module(p, name) -> begin
          match reduce_signature_ident id path p with
          | Some p -> Some (Path.module_ p name)
          | None -> None
        end
      | `Argument _ as id' -> if id = id' then Some path else None
      | `ModuleType _ -> None

  and reduce_module_ident id path (m : Identifier.Module.t) =
    match m with
      | `Root _ -> None
      | `Module(p, name) -> begin
          match reduce_signature_ident id path p with
          | Some p -> Some (Path.module_ p name)
          | None -> None
        end
      | `Argument _ as id' -> if id = id' then Some path else None

  and reduce_resolved_module_path in_arg id path (p : Path.Resolved.Module.t) =
    match p with
    | `Identifier id' ->
        if in_arg then reduce_module_ident id path id' else None
    | `Subst(_, p) ->
        reduce_resolved_module_path in_arg id path p
    | `SubstAlias(_, p) ->
        reduce_resolved_module_path in_arg id path p
    | `Hidden p ->
        reduce_resolved_module_path in_arg id path p
    | `Module(p, name) -> begin
        match reduce_resolved_module_path in_arg id path p with
        | Some p -> Some (Path.module_ p name)
        | None -> None
      end
    | `Canonical (p, _) ->
        reduce_resolved_module_path in_arg id path p
    | `Apply(p, arg) -> begin
        let rp = reduce_resolved_module_path in_arg id path p in
        let rarg = reduce_module_path true id path arg in
          match rp, rarg with
          | None, None -> None
          | None, Some arg -> Some(`Resolved(`Apply(p, arg)))
          | Some p, None -> Some(Path.apply p arg)
          | Some p, Some arg -> Some(Path.apply p arg)
      end

  and reduce_resolved_module_type_path id path (p : Path.Resolved.ModuleType.t) =
    match p with
    | `Identifier _ -> None
    | `ModuleType(p, name) -> begin
        match reduce_resolved_module_path false id path p with
        | Some p -> Some (Path.module_type p name)
        | None -> None
      end

  and reduce_module_path in_arg id path (p : Path.Module.t) =
    match p with
    | `Resolved r -> reduce_resolved_module_path in_arg id path r
    | `Root _ -> None
    | `Forward _ -> None
    | `Dot(p, name) -> begin
        match reduce_module_path in_arg id path p with
        | Some p -> Some (`Dot(p, name))
        | None -> None
      end
    | `Apply(p, arg) -> begin
        let rp = reduce_module_path in_arg id path p in
        let rarg = reduce_module_path true id path arg in
          match rp, rarg with
          | None, None -> None
          | None, Some arg -> Some(`Apply(p, arg))
          | Some p, None -> Some(`Apply(p, arg))
          | Some p, Some arg -> Some(`Apply(p, arg))
      end

  and reduce_module_type_path id path (m : Path.ModuleType.t) =
    match m with
    | `Resolved r -> reduce_resolved_module_type_path id path r
    | `Dot(p, name) -> begin
        match reduce_module_path false id path p with
        | Some p -> Some (`Dot(p, name))
        | None -> None
      end

  let rec subst_signature_ident id lookup path (s : Identifier.Signature.t) =
    match s with
      | `Root _ -> None
      | `Module(p, name) -> begin
          match subst_signature_ident id lookup path p with
          | Some (p, t) ->
              let p = Path.module_ p name in
              let t = lazy (lookup_module (ModuleName.to_string name) (Lazy.force t)) in
                Some (p, t)
          | None -> None
        end
      | `Argument _ as id' ->
          if id = id' then Some (path, lazy (lookup path))
          else None
      | `ModuleType _ -> None

  and subst_module_ident id lookup path (id' : Identifier.Module.t) =
    if id = id' then Some (path, lazy (lookup path))
    else match id' with
      | (`Root _ : Identifier.Module.t) -> None
      | `Module(p, name) -> begin
          match subst_signature_ident id lookup path p with
          | Some (p, t) ->
              let p = Path.module_ p name in
              let t = lazy (lookup_module (ModuleName.to_string name) (Lazy.force t)) in
                Some (p, t)
          | None -> None
        end
      | `Argument _ -> None

  and subst_module_type_ident id lookup (path : Path.Module.t) (id' : Identifier.ModuleType.t) =
      match id' with
      | `ModuleType(p, name) -> begin
          match subst_signature_ident id lookup path p with
          | Some (p, t) ->
              let p = Path.module_type p name in
              let t = lazy (lookup_module_type (ModuleTypeName.to_string name) (Lazy.force t)) in
                Some (p, t)
          | None -> None
        end

  and subst_resolved_module_path id lookup path (p : Path.Resolved.Module.t) =
    match p with
    | `Identifier id' -> subst_module_ident id lookup path id'
    | `Subst(_, p) -> subst_resolved_module_path id lookup path p
    | `SubstAlias(sub, _) -> subst_resolved_module_path id lookup path sub
    | `Hidden p -> subst_resolved_module_path id lookup path p
    | `Module(p, name) -> begin
        match subst_resolved_module_path id lookup path p with
        | Some (p, t) ->
            let p = Path.module_ p name in
            let t = lazy (lookup_module (ModuleName.to_string name) (Lazy.force t)) in
              Some (p, t)
        | None -> None
      end
    | `Canonical (p, _) -> subst_resolved_module_path id lookup path p
    | `Apply(p, arg) -> begin
        match subst_resolved_module_path id lookup path p with
        | Some (p, t) ->
            let p = Path.apply p arg in
            let t = lazy (lookup_apply lookup arg (Lazy.force t)) in
              Some (p, t)
        | None -> None
      end

  and subst_resolved_module_type_path id lookup (path : Path.Module.t) (p : Path.Resolved.ModuleType.t) =
    match p with
    | `Identifier id' -> subst_module_type_ident id lookup path id'
    | `ModuleType(p, name) -> begin
        match subst_resolved_module_path id lookup path p with
        | Some (p, t) ->
            let p = Path.module_type p name in
            let t = lazy (lookup_module_type (ModuleTypeName.to_string name) (Lazy.force t)) in
              Some (p, t)
        | None -> None
      end

  and subst_module_path id lookup path (p : Path.Module.t) =
    match p with
    | `Resolved r -> subst_resolved_module_path id lookup path r
    | `Root _ -> None
    | `Forward _ -> None
    | `Dot(p, name) -> begin
        match subst_module_path id lookup path p with
        | Some (p, t) ->
            let p = `Dot(p, name) in
            let t = lazy (lookup_module name (Lazy.force t)) in
              Some (p, t)
        | None -> None
      end
    | `Apply(p, arg) -> begin
        match subst_module_path id lookup path p with
        | Some (p, t) ->
            let p = `Apply(p, arg) in
            let t = lazy (lookup_apply lookup arg (Lazy.force t)) in
              Some (p, t)
        | None -> None
      end

  and subst_module_type_path id lookup (path : Path.Module.t) (p : Path.ModuleType.t) =
    match p with
    | `Resolved r -> subst_resolved_module_type_path id lookup path r
    | `Dot(p, name) -> begin
        match subst_module_path id lookup path p with
        | Some (p, t) ->
            let p = `Dot(p, name) in
            let t = lazy (lookup_module_type name (Lazy.force t)) in
              Some (p, t)
        | None -> None
      end

  and subst_expr id lookup path expr =
    match expr.term with
    | Path(p, sub) -> begin
        let p =
          match reduce_module_type_path id path p with
          | None -> p
          | Some p -> p
        in
          match subst_module_type_path id lookup path p with
          | None ->
              let term = Path(p, sub) in
              let expansion =
                lazy (subst id lookup path (Lazy.force expr.expansion))
              in
                {term; expansion}
          | Some (p, t) ->
              let term = Path(p, sub) in
              let expansion =
                lazy (module_type_substitution p t
                        (subst id lookup path (Lazy.force expr.expansion)) )
              in
                {term; expansion}
      end
    | Alias(p, sub) -> begin
        let p =
          match reduce_module_path false id path p with
          | None -> p
          | Some p -> p
        in
          match subst_module_path id lookup path p with
          | None ->
              let term = Alias(p, sub) in
              let expansion =
                lazy (subst id lookup path (Lazy.force expr.expansion))
              in
                {term; expansion}
          | Some (p, t) ->
              let term = Alias(p, sub) in
              let expansion =
                lazy (module_substitution p t
                        (subst id lookup path (Lazy.force expr.expansion)) )
              in
                {term; expansion}
      end
    | WithModule(body, frag, eq) ->
        let body = subst_expr id lookup path body in
        let eq = subst id lookup path eq in
        let term = WithModule(body, frag, eq) in
        let expansion =
          lazy (with_module frag eq (Lazy.force body.expansion))
        in
          {term; expansion}
    | WithModuleSubst(body, frag) ->
        let body = subst_expr id lookup path body in
        let term = WithModuleSubst(body, frag) in
        let expansion =
          lazy (with_module_subst frag (Lazy.force body.expansion))
        in
          {term; expansion}
    | WithTypeSubst(body, frag) ->
        let body = subst_expr id lookup path body in
        let term = WithTypeSubst(body, frag) in
        let expansion =
          lazy (with_type_subst frag (Lazy.force body.expansion))
        in
          {term; expansion}

  and subst id lookup path t =
    match t.body with
    | Expr expr -> mkExpr (subst_expr id lookup path expr)
    | Sig sg ->
        let sg =
          lazy
            ( let sg = Lazy.force sg in
              let modules =
                SMap.map (subst id lookup path) sg.modules
              in
              let module_types =
                SMap.map (subst id lookup path) sg.module_types
              in
                {sg with modules; module_types} )
        in
          mkSig sg
    | Functor fn ->
        let arg = subst id lookup path fn.arg in
        let res = subst id lookup path fn.res in
        let cache = fn.cache.fresh 3 in
          mkFunctor {id = fn.id; arg; res; cache}
    | Generative t -> generative (subst id lookup path t)
    | Abstract -> abstract
    | Unresolved -> unresolved

  and lookup_apply : (Paths_types.Path.module_ -> t) ->
           Paths_types.Path.module_ -> t -> t = fun lookup arg t ->
    match t.body with
    | Expr expr -> lookup_apply lookup arg (Lazy.force expr.expansion)
    | Sig _ -> unresolved
    | Functor fn -> begin
        try
          fn.cache.find arg
        with Not_found ->
          let res = subst fn.id lookup arg fn.res in
            fn.cache.add arg res;
            res
      end
    | Generative _ -> unresolved
    | Abstract -> unresolved
    | Unresolved -> unresolved

  let rec find_parent_apply : (Paths_types.Path.module_ -> t) ->
           Paths_types.Path.module_ -> t -> Parent.module_ = fun lookup arg t ->
    match t.body with
    | Expr expr -> begin
        match expr.term with
        | Path(_, true) | Alias(_, true) -> raise Not_found
        | Alias(_, false) -> begin
            let t = Lazy.force expr.expansion in
            match t.hidden with
            | false -> raise Not_found
            | true -> find_parent_apply lookup arg t
          end
        | _ -> find_parent_apply lookup arg (Lazy.force expr.expansion)
      end
    | Sig _ -> raise Not_found
    | Functor fn -> begin
        try
          `Module (fn.cache.find arg)
        with Not_found ->
          let res = subst fn.id lookup arg fn.res in
            fn.cache.add arg res;
            `Module res
      end
    | Generative _ -> raise Not_found
    | Abstract -> raise Not_found
    | Unresolved -> raise Not_found

end

and Datatype : sig

  type t

  val find_constructor_element : string -> t -> Element.datatype_constructor

  val find_field_element : string -> t -> Element.datatype_field

  val find_label_element : string -> t -> Element.datatype_label

  val find_element : string -> t -> Element.datatype

  val add_documentation : Odoc_model.Comment.docs -> t -> t

  val abstract : t

  val variant : string -> string list -> t

  val record : string -> string list -> t

  val extensible : t

  val unresolved : t

  val elements : t -> Element.datatype LMap.t

end = struct

  type variant =
    { type_name: string;
      constructors: SSet.t;
      labels: SSet.t; }

  type record =
    { type_name: string;
      fields: SSet.t;
      labels: SSet.t; }

  type t =
    | Variant of variant
    | Record of record
    | Unresolved

  let find_constructor_element name = function
    | Variant v ->
        if SSet.mem name v.constructors then `Constructor v.type_name
        else raise Not_found
    | _ -> raise Not_found

  let find_field_element name = function
    | Record r ->
        if SSet.mem name r.fields then `Field r.type_name
        else raise Not_found
    | _ -> raise Not_found

  let find_label_element name = function
    | Variant v ->
        if SSet.mem name v.labels then `Label (Some v.type_name)
        else raise Not_found
    | Record r ->
        if SSet.mem name r.labels then `Label (Some r.type_name)
        else raise Not_found
    | _ -> raise Not_found

  let find_element name = function
    | Variant v ->
        if SSet.mem name v.constructors then `Constructor v.type_name
        else if SSet.mem name v.labels then `Label (Some v.type_name)
        else raise Not_found
    | Record r ->
        if SSet.mem name r.fields then `Field r.type_name
        else if SSet.mem name r.labels then `Label (Some r.type_name)
        else raise Not_found
    | _ -> raise Not_found

  let add_documentation doc = function
    | Variant v ->
        let lbls = documentation_labels [] doc in
        let labels =
          List.fold_right (fun (lbl, _) map -> SSet.add lbl map) lbls v.labels
        in
          Variant {v with labels}
    | Record r ->
        let lbls = documentation_labels [] doc in
        let labels =
          List.fold_right (fun (lbl, _) map -> SSet.add lbl map) lbls r.labels
        in
          Record {r with labels}
    | Unresolved -> Unresolved

  let abstract = Unresolved

  let variant type_name constructors =
    let constructors =
        List.fold_right SSet.add constructors SSet.empty
    in
    let labels = SSet.empty in
    let variant = {type_name; constructors; labels} in
      Variant variant

  let record type_name fields =
    let fields =
        List.fold_right SSet.add fields SSet.empty
    in
    let labels = SSet.empty in
    let record = {type_name; fields; labels} in
      Record record

  let extensible = Unresolved

  let unresolved = Unresolved

  let elements = function
    | Variant v ->
        let elements =
          SSet.fold
            (fun name acc ->
               LMap.add name (`Constructor v.type_name) acc)
              v.constructors LMap.empty
        in
        let elements =
          SSet.fold
            (fun name acc ->
               LMap.add name (`Label (Some v.type_name)) acc)
            v.labels elements
        in
          elements
      | Record r ->
          let elements =
            SSet.fold
              (fun name acc ->
                 LMap.add name (`Field r.type_name) acc)
              r.fields LMap.empty
          in
          let elements =
            SSet.fold
              (fun name acc ->
                 LMap.add name (`Label (Some r.type_name)) acc)
              r.labels elements
          in
            elements
      | Unresolved -> LMap.empty

end

and ClassSig : sig

  type t

  val find_method_element : string -> t -> Element.class_signature_method

  val find_instance_variable_element : string -> t ->
        Element.class_signature_instance_variable

  val find_label_element : string -> t -> Element.class_signature_label

  val find_element : string -> t -> Element.class_signature

  type signature

  val empty : signature

  val add_element : string -> Element.class_signature ->
    signature -> signature

  val add_documentation : Odoc_model.Comment.docs -> signature -> signature

  val add_comment : Odoc_model.Comment.docs_or_stop -> signature -> signature

  val inherit_ : t -> signature -> signature

  val constr : (Path.ClassType.t -> t) -> Path.ClassType.t -> t

  val signature : ('b -> signature) -> 'b -> t

  val unresolved : t

end = struct

  type signature = Element.class_signature LMap.t

  type desc =
    | Sig of signature
    | Unresolved

  type t = desc Lazy.t

  let find_method_element name t =
    let desc = Lazy.force t in
      match desc with
      | Sig csig ->
          LMap.map_find name
            (function
              | `Method as x -> Some x
              | _ -> None)
            csig
      | Unresolved -> raise Not_found

  let find_instance_variable_element name t =
    let desc = Lazy.force t in
      match desc with
      | Sig csig ->
          LMap.map_find name
            (function
              | `InstanceVariable as x -> Some x
              | _ -> None)
            csig
      | Unresolved -> raise Not_found

  let find_label_element name t =
    let desc = Lazy.force t in
      match desc with
      | Sig csig ->
          LMap.map_find name
            (function
              | `Label _ as x -> Some x
              | _ -> None)
            csig
      | Unresolved -> raise Not_found

  let find_element name t =
    let desc = Lazy.force t in
      match desc with
      | Sig csig ->
          LMap.find_name name csig
      | Unresolved -> raise Not_found

  let empty = LMap.empty

  let add_element name element csig = LMap.add name element csig

  let add_documentation doc csig =
    let labels = documentation_labels [] doc in
    let add_label csig (label, _) = add_element label (`Label None) csig in
      List.fold_left add_label csig labels

  let add_comment comment sg =
    let labels = comment_labels [] comment in
    let add_label sg (label, _) = add_element label (`Label None) sg in
      List.fold_left add_label sg labels

  let inherit_ t csig =
    let desc = Lazy.force t in
      match desc with
      | Sig inhr -> LMap.fold LMap.add inhr csig
      | Unresolved -> csig

  let constr f x = lazy (Lazy.force (f x))

  let signature f x = lazy (Sig (f x))

  let unresolved = lazy Unresolved

end

and Page : sig

  type t

  val find_label_element : string -> t -> Element.page_label

  val find_section_title : string -> t -> Odoc_model.Comment.link_content

  val of_doc : Odoc_model.Comment.docs -> t

end = struct

  type t = {
    labels : Element.page_label LMap.t;
    section_titles : Odoc_model.Comment.link_content SMap.t;
  }

  let find_label_element name t =
    LMap.find_name name t.labels

  let find_section_title name t =
    SMap.find name t.section_titles

  let of_doc doc =
    let labels = documentation_labels [] doc in
    let add_label t (label, txt) =
      let labels = LMap.add label (`Label None) t.labels in
      let section_titles = SMap.add label txt t.section_titles in
      {labels; section_titles}
    in
    List.fold_left add_label
      { labels = LMap.empty; section_titles = SMap.empty } labels

end

and Parent : sig

  type t = [
    | `Module of Sig.t
    | `ModuleType of Sig.t
    | `Datatype of Datatype.t
    | `Class of ClassSig.t
    | `ClassType of ClassSig.t
  ]

  type signature = [
    | `Module of Sig.t
    | `ModuleType of Sig.t
  ]

  type class_signature = [
    | `Class of ClassSig.t
    | `ClassType of ClassSig.t
  ]


  type datatype = [
    | `Datatype of Datatype.t
  ]

  type module_ = [
    | `Module of Sig.t
  ]

  type module_type = [
    | `ModuleType of Sig.t
  ]

  type sig_or_type = [
    | `Module of Sig.t
    | `ModuleType of Sig.t
    | `Datatype of Datatype.t
  ]

  type any = t

  type subst =
    | Subst of Path.ModuleType.t
    | SubstAlias of Path.Module.t

end = Parent

and Element : sig

  type mod_t = { canonical : (Path.Module.t * Reference.Module.t) option
        ; hidden : bool }

  type s_module = [
    | `Module of mod_t
  ]

  type s_module_type = [
    | `ModuleType
  ]

  type s_type = [
    | `Type
  ]

  type s_constructor = [
    | `Constructor of string
  ]

  type s_field = [
    | `Field of string
  ]

  type s_extension = [
    | `Extension
  ]

  type s_exception = [
    | `Exception
  ]

  type s_value = [
    | `Value
  ]

  type s_class = [
    | `Class
  ]

  type s_class_type = [
    | `ClassType
  ]

  type s_method = [
    | `Method
  ]

  type s_instance_variable = [
    | `InstanceVariable
  ]

  type s_label = [
    | `Label of string option
  ]

  type t = [
    | s_module | s_module_type | s_type | s_constructor
    | s_field | s_extension | s_exception | s_value | s_class
    | s_class_type | s_method | s_instance_variable | s_label
  ]

  type signature_module = s_module

  type signature_module_type = s_module_type

  type signature_type = [ s_type | s_class | s_class_type ]

  type signature_constructor = [s_constructor | s_extension | s_exception]

  type signature_field = s_field

  type signature_extension = [s_extension | s_exception ]

  type signature_exception = s_exception

  type signature_value = s_value
  type signature_class = s_class

  type signature_class_type = [ s_class | s_class_type ]

  type signature_label = s_label

  type signature =
    [ s_module | s_module_type | s_type
         | s_constructor | s_field | s_extension
         | s_exception | s_value | s_class | s_class_type | s_label ]

  type datatype_constructor = s_constructor

  type datatype_field = s_field

  type datatype_label = s_label

  type datatype = [ s_constructor | s_field | s_label]

  type class_signature_method = s_method

  type class_signature_instance_variable = s_instance_variable

  type class_signature_label = s_label

  type class_signature = [ s_method | s_instance_variable | s_label ]

  type page_label = s_label

end = Element
