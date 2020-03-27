open Result
open StdLabels
open Odoc_model.Paths
open Odoc_model.Names

type t = {
  page : string list;
  (* in reverse order! *)

  anchor : string;

  kind : string;
}

let to_string { page; anchor; _ } =
  String.concat ~sep:"/" (List.rev page) ^ "#" ^ anchor

module Error = struct
  type nonrec t =
    | Not_linkable of string
    | Uncaught_exn of string
    (* These should basicaly never happen *)
    | Unexpected_anchor of t * string
    | Missing_anchor of t * string

  let to_string = function
    | Not_linkable s -> Printf.sprintf "Not_linkable %S" s
    | Uncaught_exn s -> Printf.sprintf "Uncaught_exn %S" s
    | Unexpected_anchor (t, s) ->
      Printf.sprintf "Unexpected_anchor %S (parent of %s)" (to_string t) s
    | Missing_anchor (t, s) ->
      Printf.sprintf "Missing_anchor on %S for %S" (to_string t) s
end

(* let (^/) x y = x ^ "/" ^ y *)

let (>>|) x f =
  match x with
  | Ok x -> Ok (f x)
  | Error _ as e -> e

let (>>=) x f =
  match x with
  | Ok x -> f x
  | Error _ as e -> e

let rec from_identifier : stop_before:bool ->
  Identifier.t -> (t, Error.t) result =
  fun ~stop_before ->
    let open Error in
    function
    | `Root (abstr, unit_name) ->
      begin try Ok abstr.package
      with exn -> Error (Uncaught_exn (Printexc.to_string exn))
      end >>| fun pkg_name ->
      let page = [ pkg_name ] in
      let kind = "module" in
      (* FIXME: for the moment we ignore [stop_before] for compilation units. At
         some point we want to change that. *)
      (*
      if stop_before then
        { page; anchor = unit_name; kind }
      else
      *)
      { page = UnitName.to_string unit_name :: page; anchor = ""; kind }
    | `Page (abstr, page_name) ->
      begin try Ok abstr.package
      with exn -> Error (Uncaught_exn (Printexc.to_string exn))
      end >>| fun pkg_name ->
      let page = [ PageName.to_string page_name ^ ".html"; pkg_name ] in
      let kind = "page" in
      { page; anchor = ""; kind }
    | `Module (parent, mod_name) ->
      from_identifier_no_anchor (parent :> Identifier.t) ("module " ^ ModuleName.to_string mod_name)
      >>| fun parent ->
      let kind = "module" in
      if stop_before then
        { page = parent; anchor = Printf.sprintf "%s-%s" kind (ModuleName.to_string mod_name); kind }
      else
        { page = (ModuleName.to_string mod_name) :: parent; anchor = ""; kind }
    | `Argument (functor_id, arg_num, arg_name) ->
      from_identifier_no_anchor (functor_id :> Identifier.t) ("arg " ^ ArgumentName.to_string arg_name)
      >>| fun parent ->
      let kind = "argument" in
      let suffix = Printf.sprintf "%s-%d-%s" kind arg_num (ArgumentName.to_string arg_name) in
      if stop_before then
        { page = parent; anchor = suffix; kind }
      else
        { page = suffix :: parent; anchor = ""; kind }
    | `ModuleType (parent, modt_name) ->
      from_identifier_no_anchor (parent :> Identifier.t) ("module type " ^ ModuleTypeName.to_string modt_name)
      >>| fun parent ->
      let kind = "module-type" in
      let suffix = Printf.sprintf "%s-%s" kind (ModuleTypeName.to_string modt_name) in
      if stop_before then
        { page = parent; anchor = suffix; kind }
      else
        { page = suffix :: parent; anchor = ""; kind }
    | `Type (parent, type_name) ->
      from_identifier_no_anchor (parent :> Identifier.t) ("type " ^ (TypeName.to_string type_name))
      >>| fun page ->
      let kind = "type" in
      { page; anchor = Printf.sprintf "%s-%s" kind (TypeName.to_string type_name); kind }
    | `CoreType ty_name ->
      Error (Not_linkable ("core_type:"^ (TypeName.to_string ty_name)))
    | `Constructor (parent, name) ->
      from_identifier ~stop_before:false (parent :> Identifier.t)
      >>= begin function
      (* FIXME: update doc-ock. *)
(*       | { anchor = ""; _ } as t -> Error (Missing_anchor (t, name)) *)
      | { page; anchor; _ } ->
        let kind = "constructor" in
        Ok { page; anchor = anchor ^ "." ^ (ConstructorName.to_string name); kind }
      end
    | `Field (parent, name) ->
      from_identifier ~stop_before:false (parent :> Identifier.t)
      >>= begin function
      (* FIXME: update doc-ock. *)
(*       | { anchor = ""; _ } as t -> Error (Missing_anchor (t, name)) *)
      | { page; anchor; _ } ->
        let kind = "field" in
        Ok { page; anchor = anchor ^ "." ^ (FieldName.to_string name); kind }
      end
    | `Extension (parent, name) ->
      from_identifier_no_anchor (parent :> Identifier.t) ("extension " ^ (ExtensionName.to_string name))
      >>| fun parent ->
      let kind = "extension" in
      { page = parent; anchor = Printf.sprintf "%s-%s" kind (ExtensionName.to_string name); kind }
    | `Exception (parent, name) ->
      from_identifier_no_anchor (parent :> Identifier.t) ("exception " ^ (ExceptionName.to_string name))
      >>| fun parent ->
      let kind = "exception" in
      { page = parent; anchor = Printf.sprintf "%s-%s" kind (ExceptionName.to_string name); kind }
    | `CoreException name ->
      Error (Not_linkable ("core_exception:" ^ (ExceptionName.to_string name)))
    | `Value (parent, name) ->
      from_identifier_no_anchor (parent :> Identifier.t) ("val " ^ (ValueName.to_string name))
      >>| fun parent ->
      let kind = "val" in
      { page = parent; anchor = Printf.sprintf "%s-%s" kind (ValueName.to_string name); kind }
    | `Class (parent, name) ->
      from_identifier_no_anchor (parent :> Identifier.t) ("class " ^ (ClassName.to_string name))
      >>| fun parent ->
      let kind = "class" in
      let suffix = Printf.sprintf "%s-%s" kind (ClassName.to_string name) in
      if stop_before then
        { page = parent; anchor = suffix; kind }
      else
        { page = suffix :: parent; anchor = ""; kind }
    | `ClassType (parent, name) ->
      from_identifier_no_anchor (parent :> Identifier.t) ("class type " ^ (ClassTypeName.to_string name))
      >>| fun parent ->
      let kind = "class-type" in
      let suffix = Printf.sprintf "%s-%s" kind (ClassTypeName.to_string name) in
      if stop_before then
        { page = parent; anchor = suffix; kind }
      else
        { page = suffix :: parent; anchor = ""; kind }
    | `Method (parent, name) ->
      let str_name = MethodName.to_string name in
      from_identifier_no_anchor (parent :> Identifier.t) ("method " ^ str_name)
      >>| fun page ->
      let kind = "method" in
      { page; anchor = Printf.sprintf "%s-%s" kind str_name; kind }
    | `InstanceVariable (parent, name) ->
      let str_name = InstanceVariableName.to_string name in
      from_identifier_no_anchor (parent :> Identifier.t) ("val " ^ str_name)
      >>| fun page ->
      let kind = "val" in
      { page; anchor = Printf.sprintf "%s-%s" kind str_name; kind }
    | `Label (parent, anchor') ->
      let anchor = LabelName.to_string anchor' in
      from_identifier ~stop_before:false (parent :> Identifier.t)
      >>= function
      | { page; anchor = ""; kind } ->
        (* Really ad-hoc and shitty, but it works. *)
        if kind = "page" then Ok { page; anchor; kind }
        else Ok {page; anchor; kind = "" }
      | otherwise ->
        Error (Unexpected_anchor (otherwise, "label " ^ anchor))

and from_identifier_no_anchor :
  Identifier.t -> string -> (string list, Error.t) result =
  fun id child ->
    from_identifier ~stop_before:false id
    >>= function
    | { page; anchor = ""; _ } -> Ok page
    | otherwise -> Error (Unexpected_anchor (otherwise, child))

let anchor_of_id_exn id =
  match from_identifier ~stop_before:true id with
  | Error e -> failwith (Error.to_string e)
  | Ok { anchor; _ } -> anchor

let kind_of_id_exn id =
  match from_identifier ~stop_before:true id with
  | Error e -> failwith (Error.to_string e)
  | Ok { kind; _ } -> kind

let render_path : Odoc_model.Paths.Path.t -> string =
  let open Odoc_model.Paths.Path in
  let rec render_resolved : Odoc_model.Paths.Path.Resolved.t -> string =
    let open Resolved in
    function
    | `Identifier id -> Identifier.name id
    | `Subst (_, p) -> render_resolved (p :> t)
    | `SubstAlias (_, p) -> render_resolved (p :> t)
    | `Hidden p -> render_resolved (p :> t)
    | `Module (p, s) -> render_resolved (p :> t) ^ "." ^ (ModuleName.to_string s)
    | `Canonical (_, `Resolved p) -> render_resolved (p :> t)
    | `Canonical (p, _) -> render_resolved (p :> t)
    | `Apply (rp, p) -> render_resolved (rp :> t) ^ "(" ^ render_path (p :> Odoc_model.Paths.Path.t) ^ ")"
    | `ModuleType (p, s) -> render_resolved (p :> t) ^ "." ^ (ModuleTypeName.to_string s)
    | `Type (p, s) -> render_resolved (p :> t) ^ "." ^ (TypeName.to_string s)
    | `Class (p, s) -> render_resolved (p :> t) ^ "." ^ (ClassName.to_string s)
    | `ClassType (p, s) -> render_resolved (p :> t) ^ "." ^ (ClassTypeName.to_string s)
  and render_path : Odoc_model.Paths.Path.t -> string =
    function
    | `Root root -> root
    | `Forward root -> root
    | `Dot (prefix, suffix) -> render_path (prefix :> t) ^ "." ^ suffix
    | `Apply (p1, p2) -> render_path (p1 :> t) ^ "(" ^ render_path (p2 :> t) ^ ")"
    | `Resolved rp -> render_resolved rp
  in
  render_path

module Anchor = struct
  type t = {
    kind : string;
    name : string;
  }

  module Polymorphic_variant_decl = struct
    let name_of_type_constr te =
      match te with
      | Odoc_model.Lang.TypeExpr.Constr (path, _) -> render_path (path :> Odoc_model.Paths.Path.t)
      | _ ->
        invalid_arg "DocOckHtml.Url.Polymorphic_variant_decl.name_of_type_constr"

    let from_element ~type_ident elt =
      match from_identifier ~stop_before:true type_ident with
      | Error e -> failwith (Error.to_string e)
      | Ok { anchor; _ } ->
        match elt with
        | Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type te ->
          { kind = "type"
          ; name = Printf.sprintf "%s.%s" anchor (name_of_type_constr te) }
        | Constructor {name; _} ->
          { kind = "constructor"
          ; name = Printf.sprintf "%s.%s" anchor name }
  end

  module Module_listing = struct
    module Reference = Odoc_model.Paths.Reference

    (* TODO: better error message. *)
    let fail () = failwith "Only modules allowed inside {!modules: ...}"

    let rec from_reference : Reference.t -> t = function
      | `Root (name, _) -> { kind = "xref-unresolved"; name = Odoc_model.Names.UnitName.to_string name }
      | `Dot (parent, suffix) ->
        let { name; _ } = from_reference (parent :> Reference.t) in
        { kind = "xref-unresolved"; name = Printf.sprintf "%s.%s" name suffix }
      | `Module (parent, suffix) ->
        let { name; _ } = from_reference (parent :> Reference.t) in
        { kind = "xref-unresolved"; name = Printf.sprintf "%s.%s" name (Odoc_model.Names.ModuleName.to_string suffix) }
      | `ModuleType (parent, suffix) ->
        let { name; _ } = from_reference (parent :> Reference.t) in
        { kind = "xref-unresolved"; name = Printf.sprintf "%s.%s" name (Odoc_model.Names.ModuleTypeName.to_string suffix) }
      | `Resolved r ->
        from_resolved r
      | _ ->
        fail ()

    and from_resolved : Reference.Resolved.t -> t =
      function
      | `Identifier id ->
        let name = Identifier.name id in
        let kind =
          match from_identifier ~stop_before:false id with
          | Ok { kind; _ } -> kind
          | Error _ -> fail ()
        in
        { name; kind }
      | `Module (parent, s) ->
        let { name; _ } = from_resolved (parent :> Reference.Resolved.t) in
        { kind = "module"; name = Printf.sprintf "%s.%s" name (Odoc_model.Names.ModuleName.to_string s) }
      | `ModuleType (parent, s) ->
        let { name; _ } = from_resolved (parent :> Reference.Resolved.t) in
        { kind = "module-type"; name = Printf.sprintf "%s.%s" name (Odoc_model.Names.ModuleTypeName.to_string s) }
      | _ ->
        fail ()
  end
end
