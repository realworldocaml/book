(*
 * Copyright (c) 2016, 2017 Thomas Refis <trefis@janestreet.com>
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



module Comment = Odoc_model.Comment
module Html = Tyxml.Html

open Odoc_model.Names

type flow = Html_types.flow5_without_header_footer
type phrasing = Html_types.phrasing
type non_link_phrasing = Html_types.phrasing_without_interactive



module Reference = struct
  module Id = Tree.Relative_link.Id

  open Odoc_model.Paths

  let rec render_resolved : Reference.Resolved.t -> string =
    fun r ->
      let open Reference.Resolved in
      match r with
      | `Identifier id -> Identifier.name id
      | `SubstAlias(_, r) -> render_resolved (r :> t)
      | `Module (r, s) -> render_resolved (r :> t) ^ "." ^ (ModuleName.to_string s)
      | `Canonical (_, `Resolved r) -> render_resolved (r :> t)
      | `Canonical (p, _) -> render_resolved (p :> t)
      | `ModuleType (r, s) -> render_resolved (r :> t) ^ "." ^ (ModuleTypeName.to_string s)
      | `Type (r, s) -> render_resolved (r :> t) ^ "." ^ (TypeName.to_string s)
      | `Constructor (r, s) -> render_resolved (r :> t) ^ "." ^ (ConstructorName.to_string s)
      | `Field (r, s) -> render_resolved (r :> t) ^ "." ^ (FieldName.to_string s)
      | `Extension (r, s) -> render_resolved (r :> t) ^ "." ^ (ExtensionName.to_string s)
      | `Exception (r, s) -> render_resolved (r :> t) ^ "." ^ (ExceptionName.to_string s)
      | `Value (r, s) -> render_resolved (r :> t) ^ "." ^ (ValueName.to_string s)
      | `Class (r, s) -> render_resolved (r :> t) ^ "." ^ (ClassName.to_string s)
      | `ClassType (r, s) -> render_resolved (r :> t) ^ "." ^ (ClassTypeName.to_string s)
      | `Method (r, s) ->
        (* CR trefis: do we really want to print anything more than [s] here?  *)
        render_resolved (r :> t) ^ "." ^ (MethodName.to_string s)
      | `InstanceVariable (r, s) ->
        (* CR trefis: the following makes no sense to me... *)
        render_resolved (r :> t) ^ "." ^ (InstanceVariableName.to_string s)
      | `Label (r, s) -> render_resolved (r :> t) ^ ":" ^ (LabelName.to_string s)

  let rec ref_to_string : Reference.t -> string =
    let open Reference in
    function
    | `Root (s, _) -> UnitName.to_string s
    | `Dot (parent, s) -> ref_to_string (parent :> t) ^ "." ^ s
    | `Module (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ModuleName.to_string s)
    | `ModuleType (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ModuleTypeName.to_string s)
    | `Type (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (TypeName.to_string s)
    | `Constructor (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ConstructorName.to_string s)
    | `Field (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (FieldName.to_string s)
    | `Extension (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ExtensionName.to_string s)
    | `Exception (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ExceptionName.to_string s)
    | `Value (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ValueName.to_string s)
    | `Class (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ClassName.to_string s)
    | `ClassType (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ClassTypeName.to_string s)
    | `Method (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (MethodName.to_string s)
    | `InstanceVariable (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (InstanceVariableName.to_string s)
    | `Label (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (LabelName.to_string s)
    | `Resolved r -> render_resolved r


  (* This is the entry point. stop_before is false on entry, true on recursive
     call. *)
  let rec to_html
      : ?text:(non_link_phrasing Html.elt) ->
        ?xref_base_uri:string ->
        stop_before:bool ->
        Reference.t ->
        phrasing Html.elt =

    fun ?text ?xref_base_uri ~stop_before ref ->
      let span' (txt : phrasing Html.elt list) : phrasing Html.elt =
        Html.span txt ~a:[ Html.a_class ["xref-unresolved"]
                  ; Html.a_title (Printf.sprintf "unresolved reference to %S"
                                (ref_to_string ref))
                  ]
      in
      let open Reference in
      match ref with
      | `Root (s, _) ->
        begin match text with
        | None -> Html.code [Html.txt (Odoc_model.Names.UnitName.to_string s)]
        | Some s -> (span' [(s :> phrasing Html.elt)] :> phrasing Html.elt)
        end
      | `Dot (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) s
      | `Module (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) (ModuleName.to_string s)
      | `ModuleType (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) (ModuleTypeName.to_string s)
      | `Type (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) (TypeName.to_string s)
      | `Constructor (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) (ConstructorName.to_string s)
      | `Field (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) (FieldName.to_string s)
      | `Extension (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) (ExtensionName.to_string s)
      | `Exception (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) (ExceptionName.to_string s)
      | `Value (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) (ValueName.to_string s)
      | `Class (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) (ClassName.to_string s)
      | `ClassType (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) (ClassTypeName.to_string s)
      | `Method (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) (MethodName.to_string s)
      | `InstanceVariable (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) (InstanceVariableName.to_string s)
      | `Label (parent, s) ->
        unresolved_parts_to_html ?xref_base_uri ?text span' (parent :> t) (LabelName.to_string s)
      | `Resolved r ->
        (* IDENTIFIER MUST BE RENAMED TO DEFINITION. *)
        let id = Reference.Resolved.identifier r in
        let txt : non_link_phrasing Html.elt =
          match text with
          | None -> Html.code [Html.txt (render_resolved r)]
          | Some s -> s
        in
        begin match Id.href ?xref_base_uri ~stop_before id with
        | exception Id.Not_linkable -> (txt :> phrasing Html.elt)
        | exception exn ->
          (* FIXME: better error message *)
          Printf.eprintf "Id.href failed: %S\n%!" (Printexc.to_string exn);
          (txt :> phrasing Html.elt)
        | href ->
          Html.a ~a:[ Html.a_href href ] [txt]
        end

  and unresolved_parts_to_html
      : ?text:(non_link_phrasing Html.elt) ->
        ?xref_base_uri:string ->
        ((phrasing Html.elt list) -> (phrasing Html.elt)) ->
        Reference.t ->
        string ->
          (phrasing Html.elt) =
    fun ?text ?xref_base_uri span' parent s ->
      match text with
      | Some s -> (span' [(s :> phrasing Html.elt)] :> phrasing Html.elt)
      | None ->
        let tail = [ Html.txt ("." ^ s) ] in
        span' (
          match to_html ?xref_base_uri ~stop_before:true parent with
          | content -> content::tail
        )
end


let location_to_syntax (loc:Odoc_model.Location_.span) =
  if Filename.check_suffix loc.file ".rei" then
    Tree.Reason
  else
    Tree.OCaml

let style_to_combinator = function
  | `Bold -> Html.b
  | `Italic -> Html.i
  | `Emphasis -> Html.em
  | `Superscript -> Html.sup
  | `Subscript -> Html.sub



let leaf_inline_element
    : Comment.leaf_inline_element -> ([> non_link_phrasing ] Html.elt) option =
  function
  | `Space -> Some (Html.txt " ")
  | `Word s -> Some (Html.txt s)
  | `Code_span s -> Some (Html.code [Html.txt s])
  | `Raw_markup (`Html, s) -> Some (Html.Unsafe.data s)

let rec non_link_inline_element
    : 'a. Comment.non_link_inline_element ->
        (([> non_link_phrasing ] as 'a) Html.elt) option =
  function
  | #Comment.leaf_inline_element as e -> leaf_inline_element e
  | `Styled (style, content) ->
    Some ((style_to_combinator style) (non_link_inline_element_list content))

and non_link_inline_element_list :
    'a. _ -> ([> non_link_phrasing ] as 'a) Html.elt list = fun elements ->
  List.fold_left (fun html_elements ast_element ->
    match non_link_inline_element ast_element.Odoc_model.Location_.value with
    | None -> html_elements
    | Some e -> e::html_elements)
    [] elements
  |> List.rev

let link_content_to_html =
  non_link_inline_element_list



let rec inline_element ?xref_base_uri : Comment.inline_element -> (phrasing Html.elt) option =
  function
  | #Comment.leaf_inline_element as e ->
    (leaf_inline_element e :> (phrasing Html.elt) option)
  | `Styled (style, content) ->
    Some ((style_to_combinator style) (inline_element_list ?xref_base_uri content))
  | `Reference (path, content) ->
    (* TODO Rework that ugly function. *)
    (* TODO References should be set in code style, if they are to code
            elements. *)
    let content =
      match content with
      | [] -> None
      | _ -> Some (Html.span (non_link_inline_element_list content))
    in
    Some (Reference.to_html ?text:content ?xref_base_uri ~stop_before:false path)
  | `Link (target, content) ->
    let content =
      match content with
      | [] -> [Html.txt target]
      | _ -> non_link_inline_element_list content
    in
    Some (Html.a ~a:[Html.a_href target] content)

and inline_element_list ?xref_base_uri elements =
  List.fold_left (fun html_elements ast_element ->
    match inline_element ?xref_base_uri ast_element.Odoc_model.Location_.value with
    | None -> html_elements
    | Some e -> e::html_elements)
    [] elements
  |> List.rev



let rec nestable_block_element
    : 'a. ?xref_base_uri:string ->
    to_syntax:Tree.syntax -> from_syntax:Tree.syntax ->
    Comment.nestable_block_element -> ([> flow ] as 'a) Html.elt =
  fun ?xref_base_uri ~to_syntax ~from_syntax -> function
  | `Paragraph [{value = `Raw_markup (`Html, s); _}] -> Html.Unsafe.data s
  | `Paragraph content -> Html.p (inline_element_list ?xref_base_uri content)
  | `Code_block s ->
    let open Tree in
    (*
    TODO: This will probably be replaced by a proper plugin / PPX system.
          See: https://discuss.ocaml.org/t/combining-ocamlformat-refmt/2316/10

    let transform fn = try (fn s, string_of_syntax to_syntax) with
      | Reason_syntax_util.Error(_loc, _err) ->
        (s, string_of_syntax from_syntax)
      | Syntaxerr.Error(_err) ->
        (* TODO: Properly report warnings *)
        (* Syntaxerr.report_error Format.std_formatter err; *)
        (s, string_of_syntax from_syntax)
    in
    let (code, classname) = match (from_syntax, to_syntax) with
      | (OCaml, OCaml) -> (s, string_of_syntax OCaml)
      | (Reason, Reason) -> (s, string_of_syntax Reason)
      | (Reason, OCaml) -> transform Utils.ocaml_from_reason
      | (OCaml, Reason) -> transform Utils.reason_from_ocaml
    in
    *)
    let code = s in
    let classname = string_of_syntax from_syntax in
    Html.pre [Html.code ~a:[Html.a_class [classname]] [Html.txt code]]
  | `Verbatim s -> Html.pre [Html.txt s]
  | `Modules ms ->
    let items = List.map (Reference.to_html ?xref_base_uri ~stop_before:false) (ms :> Odoc_model.Paths.Reference.t list)  in
    let items = (items :> (Html_types.li_content Html.elt) list) in
    let items = List.map (fun e -> Html.li [e]) items in
    Html.ul ~a:[Html.a_class ["modules"]] items
  | `List (kind, items) ->
    let items =
      items
      |> List.map begin function
        | [{Odoc_model.Location_.value = `Paragraph content; _}] ->
          (inline_element_list ?xref_base_uri content :> (Html_types.li_content Html.elt) list)
        | item ->
          nested_block_element_list ?xref_base_uri ~to_syntax ~from_syntax item
        end
    in
    let items = List.map Html.li items in

    match kind with
    | `Unordered -> Html.ul items
    | `Ordered -> Html.ol items

and nestable_block_element_list ?xref_base_uri ~to_syntax ~from_syntax elements =
  elements
  |> List.map Odoc_model.Location_.value
  |> List.map (nestable_block_element ?xref_base_uri ~to_syntax ~from_syntax)

and nested_block_element_list ?xref_base_uri ~to_syntax ~from_syntax elements =
  (nestable_block_element_list ?xref_base_uri ~to_syntax ~from_syntax elements :> (Html_types.flow5 Html.elt) list)



let tag : ?xref_base_uri:string ->
  to_syntax:Tree.syntax -> from_syntax:Tree.syntax ->
  Comment.tag -> ([> flow ] Html.elt) option =
  fun ?xref_base_uri ~to_syntax ~from_syntax t ->
    match t with
  | `Author s ->
    Some (Html.(dl [
      dt [txt "author"];
      dd [txt s]]))
  | `Deprecated content ->
    Some (Html.(dl [
      dt [txt "deprecated"];
      dd (nested_block_element_list ?xref_base_uri ~to_syntax ~from_syntax content)]))
  | `Param (name, content) ->
    Some (Html.(dl [
      dt [txt "parameter "; txt name];
      dd (nested_block_element_list ?xref_base_uri ~to_syntax ~from_syntax content)]))
  | `Raise (name, content) ->
    Some (Html.(dl [
      dt [txt "raises "; txt name];
      dd (nested_block_element_list ?xref_base_uri ~to_syntax ~from_syntax content)]))
  | `Return content ->
    Some (Html.(dl [
      dt [txt "returns"];
      dd (nested_block_element_list ?xref_base_uri ~to_syntax ~from_syntax content)]))
  | `See (kind, target, content) ->
    let target =
      match kind with
      | `Url -> Html.a ~a:[Html.a_href target] [Html.txt target]
      | `File -> Html.code [Html.txt target]
      | `Document -> Html.txt target
    in
    Some (Html.(dl [
      dt [txt "see "; target];
      dd (nested_block_element_list ?xref_base_uri ~to_syntax ~from_syntax content)]))
  | `Since s ->
    Some (Html.(dl [
      dt [txt "since"];
      dd [txt s]]))
  | `Before (version, content) ->
    Some (Html.(dl [
      dt [txt "before "; txt version];
      dd (nested_block_element_list ?xref_base_uri ~to_syntax ~from_syntax content)]))
  | `Version s ->
    Some (Html.(dl [
      dt [txt "version"];
      dd [txt s]]))
  | `Canonical _ | `Inline | `Open | `Closed ->
    None



let block_element
  : 'a. ?xref_base_uri:string -> to_syntax:Tree.syntax -> from_syntax:Tree.syntax ->
  Comment.block_element -> (([> flow ] as 'a) Html.elt) option =
  fun ?xref_base_uri ~to_syntax ~from_syntax -> function
  | #Comment.nestable_block_element as e ->
    Some (nestable_block_element ?xref_base_uri ~to_syntax ~from_syntax e)

  | `Heading (level, label, content) ->
    (* TODO Simplify the id/label formatting. *)
    let attributes =
      let `Label (_, label) = label in
      [Html.a_id (Odoc_model.Names.LabelName.to_string label)]
    in
    let a = attributes in

    let content =
      (non_link_inline_element_list content :> (phrasing Html.elt) list) in
    let content =
      let `Label (_, label) = label in
      let anchor =
        Html.a ~a:[Html.a_href ("#" ^ (Odoc_model.Names.LabelName.to_string label)); Html.a_class ["anchor"]] [] in
      anchor::content
    in

    let element =
      match level with
      | `Title -> Html.h1 ~a content
      | `Section -> Html.h2 ~a content
      | `Subsection -> Html.h3 ~a content
      | `Subsubsection -> Html.h4 ~a content
      | `Paragraph -> Html.h5 ~a content
      | `Subparagraph -> Html.h6 ~a content
    in
    Some element

  | `Tag t ->
    tag ?xref_base_uri ~to_syntax ~from_syntax t

let block_element_list ?xref_base_uri ~to_syntax elements =
  List.fold_left (fun html_elements (from_syntax, block) ->
    match block_element ?xref_base_uri  ~to_syntax ~from_syntax block with
    | Some e -> e::html_elements
    | None -> html_elements)
    [] elements
  |> List.rev



let first_to_html ?xref_base_uri ?syntax:(to_syntax=Tree.OCaml) = function
  | {Odoc_model.Location_.value = `Paragraph _ as first_paragraph; location} ::_ ->
    begin match block_element ?xref_base_uri ~to_syntax ~from_syntax:(location_to_syntax location) first_paragraph with
    | Some element -> [element]
    | None -> []
    end
  | _ -> []

let to_html ?xref_base_uri ?syntax:(to_syntax=Tree.OCaml) docs =
  block_element_list ?xref_base_uri ~to_syntax
    (List.map (fun el -> Odoc_model.Location_.((location el |> location_to_syntax, value el))) docs)

let has_doc docs =
  docs <> []
