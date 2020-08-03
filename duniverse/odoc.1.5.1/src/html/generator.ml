(*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
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



module Location = Odoc_model.Location_
module Paths = Odoc_model.Paths

open Utils



let a_href = Tree.Relative_link.to_sub_element

let functor_arg_pos { Odoc_model.Lang.FunctorParameter.id ; _ } =
  match id with
  | `Argument (_, nb, _) -> nb
  | _ ->
    failwith "TODO"
    (* let id = string_of_sexp @@ Identifier.sexp_of_t id in
    invalid_arg (Printf.sprintf "functor_arg_pos: %s" id) *)

let label = function
  | Odoc_model.Lang.TypeExpr.Label s -> [ Html.txt s ]
  | Optional s -> [ Html.txt "?"; Html.entity "#8288"; Html.txt s ]

let keyword keyword =
  Html.span ~a:[ Html.a_class ["keyword"] ] [ Html.txt keyword ]

let type_var tv =
  Html.span ~a:[ Html.a_class ["type-var"] ] [ Html.txt tv ]

let enclose ~l ~r content =
  Html.span (Html.txt l :: content @ [Html.txt r])

include Generator_signatures

(**
   Main functor to create an {!To_html_tree.Html_generator}
 *)
module Make (Syntax : SYNTAX) = struct
module Type_expression :
sig
  val type_expr : ?needs_parentheses:bool -> Lang.TypeExpr.t -> text

  val format_type_path :
    delim:[ `parens | `brackets ] -> Lang.TypeExpr.t list -> text -> text
end =
struct
  let rec te_variant (t : Odoc_model.Lang.TypeExpr.Polymorphic_variant.t) : text =
    let style_arguments ~constant arguments =
      (* Multiple arguments in a polymorphic variant constructor correspond
         to a conjunction of types, not a product: [`Lbl int&float].
         If constant is [true], the conjunction starts with an empty type,
         for instance [`Lbl &int].
      *)
      let wrapped_type_expr =
        (* type conjunction in Reason is printed as `Lbl (t1)&(t2)` *)
        if Syntax.Type.Variant.parenthesize_params then
          fun x ->
            [Html.span (Html.txt "(" :: type_expr x @ [Html.txt ")"])]
        else
          fun x -> type_expr x
      in
      let arguments =
        list_concat_map
          arguments
          ~sep:(Html.txt " & ")
          ~f:wrapped_type_expr
      in
      if constant
      then Html.txt "& " :: arguments
      else arguments
    in
    let rec style_elements ~add_pipe = function
      | [] -> []
      | first :: rest ->
        let first =
          match first with
          | Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type te ->
            let res = type_expr te in
            if add_pipe
            then [Html.txt " "; Html.span (Html.txt "| " :: res)]
            else res
          | Constructor {constant; name; arguments; _} ->
            let constr =
              let name = "`" ^ name in
              if add_pipe
              then Html.span [Html.txt ("| " ^ name)]
              else Html.txt name
            in
            let res =
              match arguments with
              | [] -> constr
              | _ ->
                let arguments = style_arguments ~constant arguments in
                Html.span (
                  if Syntax.Type.Variant.parenthesize_params
                  then constr :: arguments
                  else constr :: Html.txt  " of " :: arguments
                )
            in
            if add_pipe
            then [Html.txt " "; res]
            else [res]
        in
        first @ style_elements ~add_pipe:true rest
    in
    let elements = style_elements ~add_pipe:false t.elements in
    [Html.span (
       match t.kind with
       | Fixed -> Html.txt "[ " :: elements @ [Html.txt " ]"]
       | Open -> Html.txt "[> " :: elements @ [Html.txt " ]"]
       | Closed [] -> Html.txt "[< " :: elements @ [Html.txt " ]"]
       | Closed lst ->
         let constrs = String.concat " " lst in
         Html.txt "[< " :: elements @ [Html.txt (" " ^ constrs ^ " ]")]
     )]

  and te_object (t : Odoc_model.Lang.TypeExpr.Object.t) : text =
    let fields =
      list_concat_map t.fields ~f:(function
        | Odoc_model.Lang.TypeExpr.Object.Method {name; type_} ->
          (Html.txt (name ^ Syntax.Type.annotation_separator)::type_expr type_)
            @ [Html.txt Syntax.Obj.field_separator]
        | Inherit type_ ->
            type_expr type_ @ [Html.txt Syntax.Obj.field_separator])
    in
    let open_tag =
        if t.open_ then Html.txt Syntax.Obj.open_tag_extendable
        else Html.txt Syntax.Obj.open_tag_closed
    in
    let close_tag =
        if t.open_ then Html.txt Syntax.Obj.close_tag_extendable
        else Html.txt Syntax.Obj.close_tag_closed
    in
    (open_tag :: fields) @ [close_tag]

  and format_type_path
        ~delim (params : Odoc_model.Lang.TypeExpr.t list) (path : text) : text =
    match params with
    | [] -> path
    | [param] ->
        let param = (type_expr ~needs_parentheses:true param) in
        let args =
          if Syntax.Type.parenthesize_constructor
          then  Html.txt "(" :: param @ [Html.txt ")"]
          else param
        in
      Syntax.Type.handle_constructor_params path args
    | params  ->
      let params =
        list_concat_map params ~sep:(Html.txt ",\194\160")
          ~f:type_expr
      in
      let params = match delim with
        | `parens   -> enclose ~l:"(" params ~r:")"
        | `brackets -> enclose ~l:"[" params ~r:"]"
      in
      Syntax.Type.handle_constructor_params path [params]

  and type_expr
        ?(needs_parentheses=false) (t : Odoc_model.Lang.TypeExpr.t) : text =
    match t with
    | Var s -> [type_var (Syntax.Type.var_prefix ^ s)]
    | Any  -> [type_var Syntax.Type.any]
    | Alias (te, alias) ->
      type_expr ~needs_parentheses:true te @
      Html.txt " " :: keyword "as" :: Html.txt " '" :: [ Html.txt alias ]
    | Arrow (None, src, dst) ->
      let res =
        type_expr ~needs_parentheses:true src @
        Html.txt " " :: Syntax.Type.arrow :: Html.txt " " :: type_expr dst
      in
      if not needs_parentheses then
        res
      else
        [enclose ~l:"(" res ~r:")"]
    | Arrow (Some lbl, src, dst) ->
      let res =
        Html.span (
          label lbl @ Html.txt ":" :: type_expr ~needs_parentheses:true src
        ) :: Html.txt " " :: Syntax.Type.arrow :: Html.txt " " :: type_expr dst
      in
      if not needs_parentheses then
        res
      else
        [enclose ~l:"(" res ~r:")"]
    | Tuple lst ->
      let res =
        list_concat_map
          lst
          ~sep:(Html.txt Syntax.Type.Tuple.element_separator)
          ~f:(type_expr ~needs_parentheses:true)
      in
      if Syntax.Type.Tuple.always_parenthesize || needs_parentheses then
        [enclose ~l:"(" res ~r:")"]
      else
        res
    | Constr (path, args) ->
      let link = Tree.Relative_link.of_path ~stop_before:false (path :> Paths.Path.t) in
      format_type_path ~delim:(`parens) args link
    | Polymorphic_variant v -> te_variant v
    | Object o -> te_object o
    | Class (path, args) ->
      format_type_path ~delim:(`brackets) args
        (Tree.Relative_link.of_path ~stop_before:false (path :> Paths.Path.t))
    | Poly (polyvars, t) ->
      Html.txt (String.concat " " polyvars ^ ". ") :: type_expr t
    | Package pkg ->
      [enclose ~l:"(" ~r:")" (
         keyword "module" :: Html.txt " " ::
         Tree.Relative_link.of_path ~stop_before:false
           (pkg.path :> Paths.Path.t) @
         match pkg.substitutions with
         | [] -> []
         | lst ->
           Html.txt " " :: keyword "with" :: Html.txt " " ::
           list_concat_map_list_sep
             ~sep:[Html.txt " "; keyword "and"; Html.txt " "]
             lst
             ~f:(package_subst pkg.path)
       )]

  and package_subst (pkg_path : Paths.Path.ModuleType.t)
        (frag_typ, te : Paths.Fragment.Type.t * Odoc_model.Lang.TypeExpr.t)
    : text =
    keyword "type" ::
    Html.txt " " ::
    (match pkg_path with
    | `Resolved rp ->
      let base = (Paths.Path.Resolved.ModuleType.identifier rp :> Paths.Identifier.Signature.t) in
      Tree.Relative_link.of_fragment ~base
        (frag_typ :> Paths.Fragment.t)
    | _ ->
      [Html.txt
        (Tree.render_fragment (frag_typ :> Paths.Fragment.t))]) @
    Html.txt " = " ::
    type_expr te
end
open Type_expression



(* Also handles constructor declarations for exceptions and extensible
   variants, and exposes a few helpers used in formatting classes and signature
   constraints. *)
module Type_declaration :
sig
  val type_decl :
    ?is_substitution:bool -> Lang.Signature.recursive * Lang.TypeDecl.t ->
      rendered_item * Odoc_model.Comment.docs
  val extension : Lang.Extension.t -> rendered_item * Odoc_model.Comment.docs
  val exn : Lang.Exception.t -> rendered_item * Odoc_model.Comment.docs

  val format_params :
    ?delim:[ `parens | `brackets ] ->
    Lang.TypeDecl.param list ->
      [> `PCDATA ] Html.elt

  val format_manifest : ?is_substitution:bool -> ?compact_variants:bool -> Lang.TypeDecl.Equation.t -> text * bool

  val format_constraints : (Lang.TypeExpr.t * Lang.TypeExpr.t) list -> text
end =
struct
  let record fields =
    let field mutable_ id typ =
      match Url.from_identifier ~stop_before:true id with
      | Error e -> failwith (Url.Error.to_string e)
      | Ok { anchor; kind; _ } ->
        let name = Paths.Identifier.name id in
        let cell =
          Html.td ~a:[ Html.a_class ["def"; kind ] ]
            [Html.a ~a:[Html.a_href ("#" ^ anchor); Html.a_class ["anchor"]] []
            ; Html.code (
                (if mutable_ then [keyword "mutable"; Html.txt " "] else [])
                @  (Html.txt name)
                :: (Html.txt Syntax.Type.annotation_separator)
                :: (type_expr typ)
                @  [Html.txt Syntax.Type.Record.field_separator]
              )
            ]
        in
        anchor, cell
    in
    let rows =
      fields |> List.map (fun fld ->
        let open Odoc_model.Lang.TypeDecl.Field in
        let anchor, lhs = field fld.mutable_ (fld.id :> Paths.Identifier.t) fld.type_ in
        let rhs = Comment.to_html fld.doc in
        let rhs = (rhs :> (Html_types.td_content Html.elt) list) in
        Html.tr ~a:[ Html.a_id anchor; Html.a_class ["anchored"] ] (
          lhs ::
          if not (Comment.has_doc fld.doc) then [] else [
            Html.td ~a:[ Html.a_class ["doc"] ] rhs
          ]
        )
      )
    in
    [ Html.code [Html.txt "{"]
    ; Html.table ~a:[ Html.a_class ["record"] ] rows
    ; Html.code [Html.txt "}"]]



  let constructor
    : Paths.Identifier.t -> Odoc_model.Lang.TypeDecl.Constructor.argument
    -> Odoc_model.Lang.TypeExpr.t option
    -> [> `Code | `PCDATA | `Table ] Html.elt list
  = fun id args ret_type ->
      let name = Paths.Identifier.name id in
      let cstr =
        Html.span
          ~a:[Html.a_class [Url.kind_of_id_exn id]] [Html.txt name]
      in
      let is_gadt, ret_type =
        match ret_type with
        | None -> false, []
        | Some te ->
          let constant =
            match args with
            | Tuple [] -> true
            | _ -> false
          in
          let ret_type =
            Html.txt " " ::
            (if constant then Html.txt ":" else Syntax.Type.GADT.arrow) ::
            Html.txt " " ::
            type_expr te
          in
          true, ret_type
      in
      match args with
      | Tuple [] -> [ Html.code (cstr :: ret_type) ]
      | Tuple lst ->
        let params = list_concat_map lst
          ~sep:(Html.txt Syntax.Type.Tuple.element_separator)
          ~f:(type_expr ~needs_parentheses:is_gadt)
        in
        [ Html.code (
            cstr ::
            (
              if Syntax.Type.Variant.parenthesize_params
              then Html.txt "(" :: params @ [ Html.txt ")" ]
              else
                (if is_gadt then
                  [Html.txt Syntax.Type.annotation_separator]
                else
                  [Html.txt " "; keyword "of"; Html.txt " "]) @
                params
            )
            @ ret_type
          )
        ]
      | Record fields ->
        if is_gadt then
          (Html.code [cstr; Html.txt Syntax.Type.annotation_separator])
          ::(record fields)
          @ [Html.code ret_type]
        else
          (Html.code
            [cstr; Html.txt " "; keyword "of"; Html.txt " "])::(record fields)



  let variant cstrs : [> Html_types.table ] Html.elt =
    let constructor id args res =
      match Url.from_identifier ~stop_before:true id with
      | Error e -> failwith (Url.Error.to_string e)
      | Ok { anchor; kind; _ } ->
        let cell =
          Html.td ~a:[ Html.a_class ["def"; kind ] ] (
            Html.a ~a:[Html.a_href ("#" ^ anchor); Html.a_class ["anchor"]]
              [] ::
            Html.code [Html.txt "| " ] ::
            constructor id args res
          )
        in
        anchor, cell
    in
    let rows =
      cstrs |> List.map (fun cstr ->
        let open Odoc_model.Lang.TypeDecl.Constructor in
        let anchor, lhs = constructor (cstr.id :> Paths.Identifier.t) cstr.args cstr.res in
        let rhs = Comment.to_html cstr.doc in
        let rhs = (rhs :> (Html_types.td_content Html.elt) list) in
        Html.tr ~a:[ Html.a_id anchor; Html.a_class ["anchored"] ] (
          lhs ::
          if not (Comment.has_doc cstr.doc) then [] else [
            Html.td ~a:[ Html.a_class ["doc"] ] rhs
          ]
        )
      )
    in
    Html.table ~a:[ Html.a_class ["variant"] ] rows



  let extension_constructor (t : Odoc_model.Lang.Extension.Constructor.t) =
    (* TODO doc *)
    constructor (t.id :> Paths.Identifier.t) t.args t.res

  let extension (t : Odoc_model.Lang.Extension.t) =
    let extension =
      Html.code (
        keyword "type" ::
        Html.txt " " ::
        Tree.Relative_link.of_path ~stop_before:false (t.type_path :> Paths.Path.t) @
        [ Html.txt " += " ]
      ) ::
      list_concat_map t.constructors ~sep:(Html.code [Html.txt " | "])
        ~f:extension_constructor
      @ (if Syntax.Type.type_def_semicolon then [ Html.txt ";" ] else [])
    in
    extension, t.doc



  let exn (t : Odoc_model.Lang.Exception.t) =
    let cstr = constructor (t.id :> Paths.Identifier.t) t.args t.res in
    let exn = Html.code [ keyword "exception"; Html.txt " " ] :: cstr
      @ (if Syntax.Type.Exception.semicolon then [ Html.txt ";" ] else [])
    in
    exn, t.doc



  let polymorphic_variant
      ~type_ident (t : Odoc_model.Lang.TypeExpr.Polymorphic_variant.t) =

    let row item =
      let kind_approx, cstr, doc =
        match item with
        | Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type te ->
          "unknown", [Html.code (type_expr te)], None
        | Constructor {constant; name; arguments; doc; _} ->
          let cstr = "`" ^ name in
          "constructor",
          begin match arguments with
          | [] -> [Html.code [ Html.txt cstr ]]
          | _ ->
            (* Multiple arguments in a polymorphic variant constructor correspond
               to a conjunction of types, not a product: [`Lbl int&float].
               If constant is [true], the conjunction starts with an empty type,
               for instance [`Lbl &int].
            *)
            let wrapped_type_expr =
              (* type conjunction in Reason is printed as `Lbl (t1)&(t2)` *)
              if Syntax.Type.Variant.parenthesize_params then
                fun x -> Html.txt "(" :: type_expr x @ [Html.txt ")"]
              else
                fun x -> type_expr x
            in
            let params = list_concat_map arguments
              ~sep:(Html.txt " & ")
              ~f:wrapped_type_expr
            in
            let params =
              if constant then Html.txt "& " :: params else params in
            [ Html.code (
                Html.txt cstr ::
                (
                if Syntax.Type.Variant.parenthesize_params
                then params
                else Html.txt " " :: keyword "of" :: Html.txt " " :: params
                )
              )
            ]
          end,
          match doc with
          | [] ->
            None
          | _ ->
            Some (Comment.to_html doc :> (Html_types.td_content Html.elt) list)
      in
      try
        let { Url.Anchor. name = anchor; kind } =
          Url.Anchor.Polymorphic_variant_decl.from_element ~type_ident item
        in
        let constructor_column =
          Html.td ~a:[ Html.a_class ["def"; kind] ] (
            Html.a ~a:[
              Tyxml.Html.a_href ("#" ^ anchor); Html.a_class ["anchor"] ] [] ::
            Html.code [Html.txt "| " ] ::
            cstr)
        in
        let columns =
          match doc with
          | None ->
            [constructor_column]
          | Some doc ->
            [constructor_column; Html.td ~a:[Html.a_class ["doc"]] doc]
        in
        Html.tr ~a:[ Html.a_id anchor; Html.a_class ["anchored"] ] columns
      with Failure s ->
        Printf.eprintf "ERROR: %s\n%!" s;
        Html.tr [
          Html.td ~a:[ Html.a_class ["def"; kind_approx] ] (
            Html.code [Html.txt "| " ] ::
            cstr
          );
        ]
    in
    let table =
      Html.table ~a:[Html.a_class ["variant"]] (List.map row t.elements) in
    match t.kind with
    | Fixed ->
      Html.code [Html.txt "[ "] :: table :: [Html.code [Html.txt " ]"]]
    | Open ->
      Html.code [Html.txt "[> "] :: table :: [Html.code [Html.txt " ]"]]
    | Closed [] ->
      Html.code [Html.txt "[< "] :: table :: [Html.code [Html.txt " ]"]]
    | Closed lst ->
      let constrs = String.concat " " lst in
      Html.code [Html.txt "[< "] :: table ::
        [Html.code [Html.txt (" " ^ constrs ^ " ]")]]



  let format_params
    : 'row. ?delim:[`parens | `brackets] -> Odoc_model.Lang.TypeDecl.param list
    -> ([> `PCDATA ] as 'row) Html.elt
  = fun ?(delim=`parens) params ->
    let format_param (desc, variance_opt) =
      let param_desc =
        match desc with
        | Odoc_model.Lang.TypeDecl.Any -> "_"
        | Var s -> "'" ^ s
      in
      match variance_opt with
      | None -> param_desc
      | Some Odoc_model.Lang.TypeDecl.Pos -> "+" ^ param_desc
      | Some Odoc_model.Lang.TypeDecl.Neg -> "-" ^ param_desc
    in
    Html.txt (
      match params with
      | [] -> ""
      | [x] -> format_param x |> Syntax.Type.handle_format_params
      | lst ->
        let params = String.concat ", " (List.map format_param lst) in
        (match delim with `parens -> "(" | `brackets -> "[")
        ^ params ^
        (match delim with `parens -> ")" | `brackets -> "]")
    )

  let format_constraints constraints =
    list_concat_map constraints ~f:begin fun (t1, t2) ->
      Html.txt " " ::
      keyword "constraint" ::
      Html.txt " " ::
      type_expr t1 @
      Html.txt " = " ::
      type_expr t2
    end

  let format_manifest
    : 'inner_row 'outer_row. ?is_substitution:bool -> ?compact_variants:bool
    -> Odoc_model.Lang.TypeDecl.Equation.t
    -> text * bool
  = fun ?(is_substitution=false) ?(compact_variants=true) equation ->
    let _ = compact_variants in (* TODO *)
    let private_ = equation.private_ in
    match equation.manifest with
    | None -> [], private_
    | Some t ->
      let manifest =
        Html.txt (if is_substitution then " := " else " = ") ::
        (if private_ then
          [keyword Syntax.Type.private_keyword; Html.txt " "]
        else []) @
        type_expr t
      in
      manifest, false



  let type_decl ?(is_substitution=false) ((recursive, t) : Lang.Signature.recursive * Lang.TypeDecl.t) =
    let tyname = Paths.Identifier.name t.id in
    let params = format_params t.equation.params in
    let constraints = format_constraints t.equation.constraints in
    let manifest, need_private =
      match t.equation.manifest with
      | Some (Odoc_model.Lang.TypeExpr.Polymorphic_variant variant) ->
        let manifest =
          (Html.txt (if is_substitution then " := " else " = ") ::
          if t.equation.private_ then
            [keyword Syntax.Type.private_keyword; Html.txt " "]
          else
            []) @
          polymorphic_variant ~type_ident:(t.id :> Paths.Identifier.t) variant
        in
        manifest, false
      | _ ->
        let manifest, need_private = format_manifest ~is_substitution t.equation in
        Utils.optional_code manifest, need_private
    in
    let representation =
      match t.representation with
      | None -> []
      | Some repr ->
        Html.code (
          Html.txt " = " ::
          if need_private then
            [keyword Syntax.Type.private_keyword; Html.txt " "]
          else
            []
        ) ::
        match repr with
        | Extensible -> [Html.code [Html.txt ".."]]
        | Variant cstrs -> [variant cstrs]
        | Record fields -> record fields
    in
    let tdecl_def =
      let keyword' =
        match recursive with
        | Ordinary | Rec -> [keyword "type"]
        | And -> [keyword "and"]
        | Nonrec -> [keyword "type"; Html.txt " "; keyword "nonrec"]
      in

      Html.code (
          keyword' @ Html.txt " "
          :: (Syntax.Type.handle_constructor_params [Html.txt tyname] [params])
      ) ::
      manifest @
      representation @
      Utils.optional_code constraints
      @ (if Syntax.Type.type_def_semicolon then [ Html.txt ";" ] else [])
    in
    tdecl_def, t.doc
end
open Type_declaration



module Value :
sig
  val value : Lang.Value.t -> rendered_item * Odoc_model.Comment.docs
  val external_ : Lang.External.t -> rendered_item * Odoc_model.Comment.docs
end =
struct
  let value (t : Odoc_model.Lang.Value.t) =
    let name = Paths.Identifier.name t.id in
    let value =
      keyword Syntax.Value.variable_keyword ::
      Html.txt " " ::
      Html.txt name ::
      Html.txt Syntax.Type.annotation_separator ::
      type_expr t.type_
      @ (if Syntax.Value.semicolon then [ Html.txt ";" ] else [])
    in
    [Html.code value], t.doc

  let external_ (t : Odoc_model.Lang.External.t) =
    let name = Paths.Identifier.name t.id in
    let external_ =
      keyword Syntax.Value.variable_keyword ::
      Html.txt " " ::
      Html.txt name ::
      Html.txt Syntax.Type.annotation_separator ::
      type_expr t.type_
      @ (if Syntax.Type.External.semicolon then [ Html.txt ";" ] else [])
    in
    [Html.code external_], t.doc
end
open Value

module ModuleSubstitution :
sig
  val module_substitution : Lang.ModuleSubstitution.t -> rendered_item * Odoc_model.Comment.docs
end =
struct
  let module_substitution (t : Odoc_model.Lang.ModuleSubstitution.t) =
    let name = Paths.Identifier.name t.id in
    let path = Tree.Relative_link.of_path ~stop_before:true (t.manifest :> Paths.Path.t) in
    let value =
      keyword "module" ::
      Html.txt " " ::
      Html.txt name ::
      Html.txt " := " ::
      path
    in
    [Html.code value], t.doc
end
open ModuleSubstitution


(* This chunk of code is responsible for laying out signatures and class
   signatures: the parts of OCaml that contain other parts as nested items.

   Each item is either

   - a leaf, like a type declaration or a value,
   - something that has a nested signature/class signature, or
   - a comment.

   Comments can contain section headings, and the top-level markup code is also
   responsible for generating a table of contents. As a result, it must compute
   the nesting of sections.

   This is also a good opportunity to properly nest everything in <section>
   tags. Even though that is not strictly required by HTML, we carry out the
   computation for it anyway when computing nesting for the table of
   contents.

   Leaf items are set in <dl> tags – the name and any definition in <dt>, and
   documentation in <dd>. Multiple adjacent undocumented leaf items of the same
   kind are set as sibling <dt>s in one <dl>, until one of them has
   documentation. This indicates groups like:

{[
type sync
type async
(** Documentation for both types. *)
]}

   Nested signatures are currently marked up with <article> tags. The top-level
   layout code is eventually indirectly triggered recursively for laying them
   out, as well. *)

module Top_level_markup :
sig
  val lay_out :
    item_to_id:('item -> string option) ->
    item_to_spec:('item -> string option) ->
    render_leaf_item:('item -> rendered_item * Odoc_model.Comment.docs) ->
    render_nested_article:
      ('item -> rendered_item * Odoc_model.Comment.docs * Tree.t list) ->
    ((_, 'item) tagged_item) list ->
      (Html_types.div_content Html.elt) list * toc * Tree.t list

  val render_toc :
    toc -> ([> Html_types.flow5_without_header_footer ] Html.elt) list

  val lay_out_page : Odoc_model.Comment.docs ->
      ((Html_types.div_content Html.elt) list *
       (Html_types.flow5_without_header_footer Html.elt) list *
       toc)
end =
struct
  (* Just some type abbreviations. *)
  type html = Html_types.flow5 Html.elt
  type comment_html = Html_types.flow5_without_header_footer Html.elt



  let add_anchor item_to_id item html =
    match item_to_id item with
    | None ->
      html,
      []
    | Some anchor_text ->
      let anchor =
        Html.a
          ~a:[Html.a_href ("#" ^ anchor_text); Html.a_class ["anchor"]]
          []
      in
      anchor::html,
      [Html.a_id anchor_text]


(* Adds spec class to the list of existing item attributes. *)
  let add_spec item_to_spec item a =
    match item_to_spec item with
    | Some spec -> Html.a_class ["spec " ^ spec] :: a
    | None -> a


  (* "Consumes" adjacent leaf items of the same kind, until one is found with
     documentation. Then, joins all their definitions, and the documentation of
     the last item (if any), into a <dl> element. The rendered <dl> element is
     paired with the list of unconsumed items remaining in the input. *)
  let leaf_item_group
      item_to_id item_to_spec render_leaf_item first_item_kind items
      : html * 'item list =

    let rec consume_leaf_items_until_one_is_documented =
        fun items acc ->

      match items with
      | (`Leaf_item (this_item_kind, item))::items
          when this_item_kind = first_item_kind ->

        let html, maybe_docs = render_leaf_item item in
        let html, maybe_id = add_anchor item_to_id item html in
        let a = add_spec item_to_spec item maybe_id in
        let html = Html.dt ~a html in
        let acc = html::acc in

        begin match maybe_docs with
        | [] ->
          consume_leaf_items_until_one_is_documented items acc
        | docs ->
          let docs = Comment.to_html docs in
          let docs = (docs :> (Html_types.dd_content Html.elt) list) in
          let docs = Html.dd docs in
          List.rev (docs::acc), items
        end

      | _ ->
        List.rev acc, items
    in

    let rendered_item_group, remaining_items =
      consume_leaf_items_until_one_is_documented items [] in

    Html.dl rendered_item_group, remaining_items



  (* When we encounter a stop comment, [(**/**)], we read everything until the
     next stop comment, if there is one, and discard it. The returned item list
     is the signature items following the next stop comment, if there are
     any. *)
  let rec skip_everything_until_next_stop_comment : 'item list -> 'item list =
    function
    | [] -> []
    | item::items ->
      match item with
      | `Comment `Stop -> items
      | _ -> skip_everything_until_next_stop_comment items



  (* Reads comment content until the next heading, or the end of comment, and
     renders it as HTML. The returned HTML is paired with the remainder of the
     comment, which will either start with the next section heading in the
     comment, or be empty if there are no section headings. *)
  let render_comment_until_heading_or_end
      : Odoc_model.Comment.docs -> comment_html list * Odoc_model.Comment.docs =
      fun docs ->

    let rec scan_comment acc docs =
      match docs with
      | [] -> List.rev acc, docs
      | block::rest ->
        match block.Location.value with
        | `Heading _ -> List.rev acc, docs
        | _ -> scan_comment (block::acc) rest
    in
    let included, remaining = scan_comment [] docs in
    let docs = Comment.to_html included in
    docs, remaining



  (* The sectioning functions take several arguments, and return "modified"
     instances of them as results. So, it is convenient to group them into a
     record type. This is most useful for the return type, as otherwise there is
     no way to give names to its components.

     The components themselves are:

     - The current comment being read. When non-empty, this is progressively
       replaced with its tail until it is exhausted.
     - The general signature items to be read. These are read one at a time when
       there is no current comment. Upon encountering a comment, it becomes the
       "current comment," and the sectioning functions read it one block element
       at a time, scanning for section headings.
     - A reversed accumulator of the rendered signature items.
     - A reversed accumulator of the table of contents.
     - An accumulator of the subpages generated for nested signatures.

     The record is also convenient for passing around the two item-rendering
     functions. *)
  type ('kind, 'item) sectioning_state = {
    input_items : (('kind, 'item) tagged_item) list;
    acc_subpages : Tree.t list;
    comment_state : comment_state;
    item_to_id : 'item -> string option;
    item_to_spec : 'item -> string option;
    render_leaf_item : 'item -> rendered_item * Odoc_model.Comment.docs;
    render_nested_article :
      'item -> rendered_item * Odoc_model.Comment.docs * Tree.t list;
  }


  (* Comment state used to generate HTML and TOC for both mli and mld inputs. *)
  and comment_state = {
    input_comment : Odoc_model.Comment.docs;
    acc_html : html list;
    acc_toc : toc;
  }

  let finish_comment_state (state : comment_state) =
    {state with
      acc_html = List.rev state.acc_html;
      acc_toc = List.rev state.acc_toc;
    }

  let is_deeper_section_level =
    let level_to_int = function
      | `Title -> 0
      | `Section -> 1
      | `Subsection -> 2
      | `Subsubsection -> 3
      | `Paragraph -> 4
      | `Subparagraph -> 5
    in
    fun other_level ~than ->
      level_to_int other_level > level_to_int than



  let rec section_items section_level state =
    match state.input_items with
    | [] ->
      {state with comment_state =
        finish_comment_state state.comment_state }

    | tagged_item::input_items ->
      match tagged_item with
      | `Leaf_item (kind, _) ->
        let html, input_items =
          leaf_item_group
            state.item_to_id
            state.item_to_spec
            state.render_leaf_item
            kind
            state.input_items
        in
        section_items section_level {state with
            input_items;
            comment_state = { state.comment_state with
              acc_html = html::state.comment_state.acc_html };
          }

      | `Nested_article item ->
        let html, maybe_docs, subpages = state.render_nested_article item in
        let html, maybe_id = add_anchor state.item_to_id item html in
        let a = add_spec state.item_to_spec item maybe_id in
        let html =
          match maybe_docs with
          | [] -> Html.div ~a html
          | docs ->
            let docs = Comment.first_to_html docs in
            let docs = (docs :> (Html_types.dd_content Html.elt) list) in
            Html.dl [Html.dt ~a html; Html.dd docs]
        in
        section_items section_level { state with
          input_items;
          comment_state = { state.comment_state with
            acc_html = html::state.comment_state.acc_html };
          acc_subpages = state.acc_subpages @ subpages;
        }

      | `Comment `Stop ->
        let input_items = skip_everything_until_next_stop_comment input_items in
        section_items section_level {state with
            input_items;
          }

      | `Comment (`Docs input_comment) ->
        section_comment section_level {state with
            input_items;
            comment_state = { state.comment_state with input_comment };
        }



  and section_comment section_level state =
    match state.comment_state.input_comment with
    | [] ->
      section_items section_level state

    | element::input_comment ->

      match element.Location.value with
      | `Heading (level, label, content) ->
        if not (is_deeper_section_level level ~than:section_level) then
          {state with comment_state =
            finish_comment_state state.comment_state }

        else
          (* We have a deeper section heading in a comment within this section.
             Parse it recursively. We start the nested HTML by parsing the
             section heading itself, and anything that follows it in the current
             comment, up to the next section heading, if any. All of this
             comment matter goes into a <header> element. The nested HTML will
             then be extended recursively by parsing more structure items,
             including, perhaps, additional comments in <aside> elements. *)
          let heading_html = Comment.to_html [element] in
          let more_comment_html, input_comment =
            render_comment_until_heading_or_end input_comment in
          let html = Html.header (heading_html @ more_comment_html) in
          let nested_section_state =
            { state with
              comment_state = {
                input_comment;
                acc_html = [html];
                acc_toc = [];
              }
            }
          in
          let nested_section_state = section_comment level nested_section_state in
          (* Wrap the nested section in a <section> element, and extend the
            table of contents. *)
          let html = Html.section nested_section_state.comment_state.acc_html in

          let `Label (_, label) = label in
          let toc_entry =
            {
              anchor = Odoc_model.Names.LabelName.to_string label;
              text = content;
              children = nested_section_state.comment_state.acc_toc;
            }
          in

          (* Continue parsing after the nested section. In practice, we have
             either run out of items, or the first thing in the input will be
             another section heading – the nested section will have consumed
             everything else. *)
          section_comment section_level {nested_section_state with
              comment_state = { nested_section_state.comment_state with
                acc_html = html::state.comment_state.acc_html;
                acc_toc = toc_entry::state.comment_state.acc_toc;
              }
            }

      | _ ->
        let html, input_comment =
          render_comment_until_heading_or_end state.comment_state.input_comment in
        let html = (html :> (Html_types.aside_content Html.elt) list) in
        section_comment section_level {state with
            comment_state = { state.comment_state with
              input_comment;
              acc_html = (Html.aside html)::state.comment_state.acc_html;
            }
          }

  let lay_out ~item_to_id ~item_to_spec ~render_leaf_item ~render_nested_article items =
    let initial_state =
      {
        input_items = items;
        comment_state = {
          input_comment = [];
          acc_html = [];
          acc_toc = [];
        };

        acc_subpages = [];

        item_to_id;
        item_to_spec;
        render_leaf_item;
        render_nested_article;
      }
    in
    let state = section_items `Title initial_state in
    state.comment_state.acc_html, state.comment_state.acc_toc, state.acc_subpages


  let rec page_section_comment ~header_docs section_level state =
    match state.input_comment with
    | [] -> {state with acc_toc = List.rev state.acc_toc}, header_docs
    | element::input_comment ->
      begin match element.Location.value with
      | `Heading (`Title, _label, _content) ->
        let heading_html = Comment.to_html [element] in
        let more_comment_html, input_comment =
          render_comment_until_heading_or_end input_comment in
        let header_docs = heading_html @ more_comment_html in
        let nested_section_state = {
          input_comment = input_comment;
          acc_html = [];
          acc_toc = [];
        } in
        let nested_section_state, header_docs =
          page_section_comment ~header_docs `Section nested_section_state in
        let acc_html = state.acc_html @ nested_section_state.acc_html in
        page_section_comment ~header_docs section_level
          { nested_section_state with acc_html }

      | `Heading (level, _label, _content)
        when not (is_deeper_section_level level ~than:section_level) ->
          {state with acc_toc = List.rev state.acc_toc}, header_docs

      | `Heading (level, label, content) ->
        let heading_html = Comment.to_html [element] in
        let more_comment_html, input_comment =
          render_comment_until_heading_or_end input_comment in
        let acc_html =  heading_html @ more_comment_html in
        let acc_html = (acc_html :> (Html_types.flow5 Html.elt) list) in
        let nested_section_state = {
          input_comment = input_comment;
          acc_html;
          acc_toc = [];
        } in
        let nested_section_state, header_docs = page_section_comment ~header_docs level nested_section_state in
        let acc_html = state.acc_html @ nested_section_state.acc_html in

        let acc_toc =
          let `Label (_, label) = label in
          let toc_entry = {
            anchor = Odoc_model.Names.LabelName.to_string label;
            text = content;
            children = nested_section_state.acc_toc;
          } in
          toc_entry :: state.acc_toc
        in
        page_section_comment ~header_docs section_level
          { nested_section_state with acc_html; acc_toc }

      | _ ->
        let html, input_comment =
          render_comment_until_heading_or_end state.input_comment in
        let html = (html :> (Html_types.flow5 Html.elt) list) in
        page_section_comment ~header_docs section_level {state with
            input_comment;
            acc_html = html @ state.acc_html;
          }
      end


  let lay_out_page input_comment =
    let initial_state : comment_state = {
      input_comment;
      acc_html = [];
      acc_toc = [];
    } in
    let state, header_docs = page_section_comment ~header_docs:[] `Title initial_state in
    state.acc_html, header_docs, state.acc_toc



  let render_toc toc =
    let rec section the_section : Html_types.li_content Html.elt list =
      let text = Comment.link_content_to_html the_section.text in
      let text =
        (text
          : Html_types.phrasing_without_interactive Html.elt list
          :> (Html_types.flow5_without_interactive Html.elt) list)
      in
      let link =
        Html.a
          ~a:[Html.a_href ("#" ^ the_section.anchor)] text
      in
      match the_section.children with
      | [] -> [link]
      | _ -> [link; sections the_section.children]

    and sections the_sections =
      the_sections
      |> List.map (fun the_section -> Html.li (section the_section))
      |> Html.ul

    in

    match toc with
    | [] -> []
    | _ -> [Html.nav ~a:[Html.a_class ["toc"]] [sections toc]]
end

(* TODO Figure out when this function would fail. It is currently pasted from
   [make_def], but the [make_spec] version doesn't have a [failwith]. *)
let path_to_id path =
  match Url.from_identifier ~stop_before:true path with
  | Error _ ->
    None
  | Ok {anchor; _} ->
    Some anchor



module Class :
sig
  val class_ :
    ?theme_uri:Tree.uri -> Lang.Signature.recursive -> Lang.Class.t ->
      rendered_item * Odoc_model.Comment.docs * Tree.t list

  val class_type :
    ?theme_uri:Tree.uri -> Lang.Signature.recursive -> Lang.ClassType.t ->
      rendered_item * Odoc_model.Comment.docs * Tree.t list
end =
struct
  let class_signature_item_to_id : Lang.ClassSignature.item -> _ = function
    | Method {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | InstanceVariable {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | Constraint _
    | Inherit _
    | Comment _ -> None

  let class_signature_item_to_spec : Lang.ClassSignature.item -> _ = function
    | Method _ -> Some "method"
    | InstanceVariable _ -> Some "instance-variable"
    | Constraint _
    | Inherit _
    | Comment _ -> None

  let tag_class_signature_item : Lang.ClassSignature.item -> _ = fun item ->
    match item with
    | Method _ -> `Leaf_item (`Method, item)
    | InstanceVariable _ -> `Leaf_item (`Variable, item)
    | Constraint _ -> `Leaf_item (`Constraint, item)
    | Inherit _ -> `Leaf_item (`Inherit, item)

    | Comment comment -> `Comment comment

  let rec render_class_signature_item : Lang.ClassSignature.item -> text * _ =
    function
    | Method m -> method_ m
    | InstanceVariable v -> instance_variable v
    | Constraint (t1, t2) -> format_constraints [(t1, t2)], []
    | Inherit (Signature _) -> assert false (* Bold. *)
    | Inherit class_type_expression ->
      [Html.code (
        keyword "inherit" ::
        Html.txt " " ::
        class_type_expr class_type_expression)],
      []

    | Comment _ -> assert false

  and class_signature (c : Lang.ClassSignature.t) =
    (* FIXME: use [t.self] *)
    let tagged_items = List.map tag_class_signature_item c.items in
    Top_level_markup.lay_out
      ~item_to_id:class_signature_item_to_id
      ~item_to_spec:class_signature_item_to_spec
      ~render_leaf_item:(fun item ->
        let text, docs = render_class_signature_item item in
        (text :> rendered_item), docs
      )
      ~render_nested_article:(fun _ -> assert false)
      tagged_items

  and method_ (t : Odoc_model.Lang.Method.t) =
    let name = Paths.Identifier.name t.id in
    let virtual_ =
      if t.virtual_ then [keyword "virtual"; Html.txt " "] else [] in
    let private_ =
      if t.private_ then [keyword "private"; Html.txt " "] else [] in
    let method_ =
      keyword "method" ::
      Html.txt " " ::
      private_ @
      virtual_ @
      Html.txt name ::
      Html.txt Syntax.Type.annotation_separator ::
      type_expr t.type_
    in
    [Html.code method_], t.doc

  and instance_variable (t : Odoc_model.Lang.InstanceVariable.t) =
    let name = Paths.Identifier.name t.id in
    let virtual_ =
      if t.virtual_ then [keyword "virtual"; Html.txt " "] else [] in
    let mutable_ =
      if t.mutable_ then [keyword "mutable"; Html.txt " "] else [] in
    let val_ =
      keyword "val" ::
      Html.txt " " ::
      mutable_ @
      virtual_ @
      Html.txt name ::
      Html.txt Syntax.Type.annotation_separator ::
      type_expr t.type_
    in
    [Html.code val_], t.doc

  and class_type_expr (cte : Odoc_model.Lang.ClassType.expr) =
    match cte with
    | Constr (path, args) ->
      let link = Tree.Relative_link.of_path ~stop_before:false (path :> Paths.Path.t) in
      format_type_path ~delim:(`brackets) args link
    | Signature _ ->
      [
        Syntax.Class.open_tag;
        Html.txt " ... ";
        Syntax.Class.close_tag
      ]

  and class_decl (cd : Odoc_model.Lang.Class.decl) =
    match cd with
    | ClassType expr -> class_type_expr expr
    (* TODO: factorize the following with [type_expr] *)
    | Arrow (None, src, dst) ->
      type_expr ~needs_parentheses:true src @
      Html.txt " " :: Syntax.Type.arrow :: Html.txt " " :: class_decl dst
    | Arrow (Some lbl, src, dst) ->
      label lbl @ Html.txt ":" ::
                  type_expr ~needs_parentheses:true src @
      Html.txt " " :: Syntax.Type.arrow :: Html.txt " " :: class_decl dst

  and class_ ?theme_uri recursive (t : Odoc_model.Lang.Class.t) =
    let name = Paths.Identifier.name t.id in
    let params = format_params ~delim:(`brackets) t.params in
    let virtual_ =
      if t.virtual_ then [keyword "virtual"; Html.txt " "] else [] in
    let cd = class_decl t.type_ in
    let cname, subtree =
      match t.expansion with
      | None -> Html.txt name, []
      | Some csig ->
        Tree.enter ~kind:(`Class) name;
        let doc = Comment.to_html t.doc in
        let expansion, toc, _ = class_signature csig in
        let header_docs =
          match toc with
          | [] -> doc
          | _ -> doc @ (Top_level_markup.render_toc toc)
        in
        let subtree = Tree.make ~header_docs ?theme_uri expansion [] in
        Tree.leave ();
        Html.a ~a:[ a_href ~kind:`Class name ] [Html.txt name], [subtree]
    in
    let class_def_content =
      let open Lang.Signature in
      let keyword' =
        match recursive with
        | Ordinary | Nonrec | Rec -> "class"
        | And -> "and"
      in
      keyword keyword' ::
      Html.txt " " ::
      virtual_ @
      params ::
      Html.txt " " ::
      cname ::
      Html.txt Syntax.Type.annotation_separator ::
      cd
    in
    let region = [Html.code class_def_content] in
    region, t.doc, subtree


  and class_type ?theme_uri recursive (t : Odoc_model.Lang.ClassType.t) =
    let name = Paths.Identifier.name t.id in
    let params = format_params ~delim:(`brackets) t.params in
    let virtual_ =
      if t.virtual_ then [keyword "virtual"; Html.txt " "] else [] in
    let expr = class_type_expr t.expr in
    let cname, subtree =
      match t.expansion with
      | None -> Html.txt name, []
      | Some csig ->
        Tree.enter ~kind:(`Cty) name;
        let doc = Comment.to_html t.doc in
        let expansion, _, _ = class_signature csig in
        let subtree = Tree.make ~header_docs:doc ?theme_uri expansion [] in
        Tree.leave ();
        Html.a ~a:[ a_href ~kind:`Cty name ] [Html.txt name], [subtree]
    in
    let ctyp =
      let open Lang.Signature in
      let keyword' =
        match recursive with
        | Ordinary | Nonrec | Rec ->
          [keyword "class"; Html.txt " "; keyword "type"]
        | And -> [keyword "and"]
      in
      keyword' @
      [Html.txt " "] @
      virtual_ @
      params ::
      Html.txt " " ::
      cname ::
      Html.txt " = " ::
      expr
    in
    let region = [Html.code ctyp] in
    region, t.doc, subtree
end
open Class



module Module :
sig
  val signature : ?theme_uri:Tree.uri -> Lang.Signature.t ->
    (Html_types.div_content Html.elt) list * toc * Tree.t list
end =
struct
  let signature_item_to_id : Lang.Signature.item -> _ = function
    | Type (_, {id; _}) -> path_to_id (id :> Paths.Identifier.t)
    | TypeSubstitution {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | Exception {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | Value {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | External {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | Module (_, {id; _}) -> path_to_id (id :> Paths.Identifier.t)
    | ModuleType {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | ModuleSubstitution {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | Class (_, {id; _}) -> path_to_id (id :> Paths.Identifier.t)
    | ClassType (_, {id; _}) -> path_to_id (id :> Paths.Identifier.t)
    | TypExt _
    | Include _
    | Comment _ -> None

  let signature_item_to_spec : Lang.Signature.item -> _ = function
    | Type _ -> Some "type"
    | TypeSubstitution _ -> Some "type-subst"
    | Exception _ -> Some "exception"
    | Value _ -> Some "value"
    | External _ -> Some "external"
    | Module _ -> Some "module"
    | ModuleType _ -> Some "module-type"
    | ModuleSubstitution _ -> Some "module-substitution"
    | Class _ -> Some "class"
    | ClassType _ -> Some "class-type"
    | TypExt _ -> Some "extension"
    | Include _
    | Comment _ -> None

  let tag_signature_item : Lang.Signature.item -> _ = fun item ->
    match item with
    | Type _ -> `Leaf_item (`Type, item)
    | TypeSubstitution _ -> `Leaf_item (`TypeSubstitution, item)
    | TypExt _ -> `Leaf_item (`Extension, item)
    | Exception _ -> `Leaf_item (`Exception, item)
    | Value _ -> `Leaf_item (`Value, item)
    | External _ -> `Leaf_item (`External, item)
    | ModuleSubstitution _ -> `Leaf_item (`ModuleSubstitution, item)

    | Module _
    | ModuleType _
    | Include _
    | Class _
    | ClassType _ -> `Nested_article item

    | Comment comment -> `Comment comment

  let rec render_leaf_signature_item : Lang.Signature.item -> _ = function
    | Type (r, t) -> type_decl (r, t)
    | TypeSubstitution t -> type_decl ~is_substitution:true (Ordinary, t)
    | TypExt e -> extension e
    | Exception e -> exn e
    | Value v -> value v
    | External e -> external_ e
    | ModuleSubstitution m -> module_substitution m
    | _ -> assert false

  and render_nested_signature_or_class
      : ?theme_uri:Tree.uri -> Lang.Signature.item -> _ = fun ?theme_uri item ->
    match item with
    | Module (recursive, m) -> module_ ?theme_uri recursive m
    | ModuleType m -> module_type ?theme_uri m
    | Class (recursive, c) -> class_ ?theme_uri recursive c
    | ClassType (recursive, c) -> class_type ?theme_uri recursive c
    | Include m -> include_ ?theme_uri m
    | _ -> assert false

  and signature ?theme_uri s =
    let tagged_items = List.map tag_signature_item s in
    Top_level_markup.lay_out
      ~item_to_id:signature_item_to_id
      ~item_to_spec:signature_item_to_spec
      ~render_leaf_item:render_leaf_signature_item
      ~render_nested_article:(render_nested_signature_or_class ?theme_uri)
      tagged_items

  and functor_argument
    : 'row. ?theme_uri:Tree.uri -> Odoc_model.Lang.FunctorParameter.parameter
    -> Html_types.div_content Html.elt list * Tree.t list
  = fun ?theme_uri arg ->
    let open Odoc_model.Lang.FunctorParameter in
    let name = Paths.Identifier.name arg.id in
    let nb = functor_arg_pos arg in
    let link_name = Printf.sprintf "%d-%s" nb name in
    let def_div, subtree =
      match arg.expansion with
      | None ->
        (
          Html.txt (Paths.Identifier.name arg.id) ::
          Html.txt Syntax.Type.annotation_separator ::
          mty (arg.id :> Paths.Identifier.Signature.t) arg.expr
        ), []
      | Some expansion ->
        let expansion =
          match expansion with
          | AlreadyASig ->
            begin match arg.expr with
            | Signature sg -> Odoc_model.Lang.Module.Signature sg
            | _ -> assert false
            end
          | e -> e
        in
        Tree.enter ~kind:(`Arg) link_name;
        let (doc, toc, subpages) = module_expansion ?theme_uri expansion in
        let header_docs = Top_level_markup.render_toc toc in
        let subtree = Tree.make ~header_docs ?theme_uri doc subpages in
        Tree.leave ();
        (
          Html.a ~a:[ a_href ~kind:`Arg link_name ] [Html.txt name] ::
          Html.txt Syntax.Type.annotation_separator ::
          mty (arg.id :> Paths.Identifier.Signature.t) arg.expr
        ), [subtree]
    in
    let region = [Html.code def_div] in
    region, subtree

  and module_expansion
    : ?theme_uri:Tree.uri -> Odoc_model.Lang.Module.expansion
    -> Html_types.div_content_fun Html.elt list * toc * Tree.t list
  = fun ?theme_uri t ->
    match t with
    | AlreadyASig -> assert false
    | Signature sg ->
      let expansion, toc, subpages = signature ?theme_uri sg in
      expansion, toc, subpages
    | Functor (args, sg) ->
      let sig_html, toc, subpages = signature ?theme_uri sg in
      let params, params_subpages =
        List.fold_left (fun (args, subpages as acc) arg ->
          match arg with
          | Odoc_model.Lang.FunctorParameter.Unit -> acc
          | Named arg ->
            let arg, arg_subpages = functor_argument ?theme_uri arg in
            let arg = Html.li arg in
            (args @ [arg], subpages @ arg_subpages)
        )
        ([], []) args
      in
      let html =
        Html.h3 ~a:[ Html.a_class ["heading"] ] [ Html.txt "Parameters" ] ::
        Html.ul (List.map Html.Unsafe.coerce_elt params) ::
        Html.h3 ~a:[ Html.a_class ["heading"] ] [ Html.txt "Signature" ] ::
        sig_html
      in
      html, toc, params_subpages @ subpages

  and module_
      : ?theme_uri:Tree.uri -> Odoc_model.Lang.Signature.recursive ->
        Odoc_model.Lang.Module.t ->
          rendered_item * Odoc_model.Comment.docs * Tree.t list
      = fun ?theme_uri recursive t ->
    let modname = Paths.Identifier.name t.id in
    let md =
      module_decl (t.id :> Paths.Identifier.Signature.t)
        (match t.display_type with
        | None -> t.type_
        | Some t -> t)
    in
    let modname, subtree =
      match t.expansion with
      | None -> Html.txt modname, []
      | Some expansion ->
        let expansion =
          match expansion with
          | AlreadyASig ->
            begin match t.type_ with
            | ModuleType (Odoc_model.Lang.ModuleType.Signature sg) ->
              Odoc_model.Lang.Module.Signature sg
            | _ -> assert false
            end
          | e -> e
        in
        Tree.enter ~kind:(`Mod) modname;
        let doc = Comment.to_html t.doc in
        let expansion, toc, subpages = module_expansion ?theme_uri expansion in
        let header_docs =
          match toc with
          | [] -> doc
          | _ -> doc @ (Top_level_markup.render_toc toc)
        in
        let subtree = Tree.make ~header_docs ?theme_uri expansion subpages in
        Tree.leave ();
        Html.a ~a:[ a_href ~kind:`Mod modname ] [Html.txt modname], [subtree]
    in
    let md_def_content =
      let keyword' =
        match recursive with
        | Ordinary | Nonrec -> [keyword "module"]
        | Rec -> [keyword "module"; Html.txt " "; keyword "rec"]
        | And -> [keyword "and"]
      in

      keyword' @ Html.txt " " :: modname :: md @
      (if Syntax.Mod.close_tag_semicolon then [Html.txt ";"] else []) in
    let region = [Html.code md_def_content] in
    region, t.doc, subtree

  and module_decl (base : Paths.Identifier.Signature.t) md =
    begin match md with
    | Alias _ -> Html.txt " = "
    | ModuleType _ -> Html.txt Syntax.Type.annotation_separator
    end ::
    module_decl' base md

  and extract_path_from_mt ~(default: Paths.Identifier.Signature.t) =
    let open Odoc_model.Lang.ModuleType in
    function
    | Path (`Resolved r) ->
      (Paths.Path.Resolved.ModuleType.identifier r :> Paths.Identifier.Signature.t)
    | With (mt, _) -> extract_path_from_mt ~default mt
    | TypeOf (Odoc_model.Lang.Module.Alias (`Resolved r)) ->
      (Paths.Path.Resolved.Module.identifier r :> Paths.Identifier.Signature.t)
    | TypeOf (Odoc_model.Lang.Module.ModuleType mt) ->
      extract_path_from_mt ~default mt
    | _ -> default

  and module_decl'
    : Paths.Identifier.Signature.t -> Odoc_model.Lang.Module.decl -> text =
    fun base -> function
    | Alias mod_path ->
      Tree.Relative_link.of_path ~stop_before:true (mod_path :> Paths.Path.t)
    | ModuleType mt -> mty (extract_path_from_mt ~default:base mt) mt

  and module_type ?theme_uri (t : Odoc_model.Lang.ModuleType.t) =
    let modname = Paths.Identifier.name t.id in
    let mty =
      match t.expr with
      | None -> []
      | Some expr ->
        Html.txt " = " :: mty (t.id :> Paths.Identifier.Signature.t) expr
    in
    let modname, subtree =
      match t.expansion with
      | None -> Html.txt modname, []
      | Some expansion ->
        let expansion =
          match expansion with
          | AlreadyASig ->
            begin match t.expr with
            | Some (Signature sg) -> Odoc_model.Lang.Module.Signature sg
            | _ -> assert false
            end
          | e -> e
        in
        Tree.enter ~kind:(`Mty) modname;
        let doc = Comment.to_html t.doc in
        let expansion, toc, subpages = module_expansion ?theme_uri expansion in
        let header_docs =
          match toc with
          | [] -> doc
          | _ -> doc @ (Top_level_markup.render_toc toc)
        in
        let subtree = Tree.make ~header_docs ?theme_uri expansion subpages in
        Tree.leave ();
        Html.a ~a:[ a_href ~kind:`Mty modname ] [Html.txt modname], [subtree]
    in
    let mty_def =
      (
        keyword "module" ::
        Html.txt " " ::
        keyword "type" ::
        Html.txt " " ::
        modname ::
        mty
        @ (if Syntax.Mod.close_tag_semicolon then [Html.txt ";"] else [])
      )
    in
    let region = [Html.code mty_def] in
    region, t.doc, subtree

  and mty
    : Paths.Identifier.Signature.t -> Odoc_model.Lang.ModuleType.expr -> text
  = fun base -> function
    | Path mty_path ->
      Tree.Relative_link.of_path ~stop_before:true (mty_path :> Paths.Path.t)
    | Signature _ ->
      [
        Syntax.Mod.open_tag;
        Html.txt " ... ";
        Syntax.Mod.close_tag;
      ]
    | Functor (Unit, expr) ->
      (if Syntax.Mod.functor_keyword then [keyword "functor"] else []) @
      Html.txt " () " ::
      mty base expr
    | Functor (Named arg, expr) ->
      let name =
        let open Odoc_model.Lang.FunctorParameter in
        let to_print = Html.txt @@ Paths.Identifier.name arg.id in
        match
          Tree.Relative_link.Id.href
            ~stop_before:(arg.expansion = None) (arg.id :> Paths.Identifier.t)
        with
        | exception _ -> to_print
        | href -> Html.a ~a:[ Html.a_href href ] [ to_print ]
      in
      (if Syntax.Mod.functor_keyword then [keyword "functor"] else []) @
      Html.txt " (" :: name :: Html.txt Syntax.Type.annotation_separator ::
      mty base arg.expr @
      [Html.txt ")"; Html.txt " "] @ Syntax.Type.arrow :: Html.txt " " ::
      mty base expr
    | With (expr, substitutions) ->
      mty base expr @
      Html.txt " " ::
      keyword "with" ::
      Html.txt " " ::
      list_concat_map_list_sep
        ~sep:[Html.txt " "; keyword "and"; Html.txt " "]
        ~f:(substitution base)
        substitutions
    | TypeOf md ->
      keyword "module" ::
      Html.txt " " ::
      keyword "type" ::
      Html.txt " " ::
      keyword "of" ::
      Html.txt " " ::
      module_decl' base md

  and substitution
    : Paths.Identifier.Signature.t -> Odoc_model.Lang.ModuleType.substitution
    -> text
  = fun base -> function
    | ModuleEq (frag_mod, md) ->
      keyword "module" ::
      Html.txt " " ::
      Tree.Relative_link.of_fragment ~base (frag_mod :> Paths.Fragment.t)
      @ Html.txt " = " ::
      module_decl' base md
    | TypeEq (frag_typ, td) ->
      keyword "type" ::
      Html.txt " " ::
      (Syntax.Type.handle_substitution_params
        (Tree.Relative_link.of_fragment
          ~base (frag_typ :> Paths.Fragment.t))
        [format_params td.Lang.TypeDecl.Equation.params]
      ) @
      fst (format_manifest td) @
      format_constraints td.Odoc_model.Lang.TypeDecl.Equation.constraints
    | ModuleSubst (frag_mod, mod_path) ->
      keyword "module" ::
      Html.txt " " ::
      Tree.Relative_link.of_fragment
        ~base (frag_mod :> Paths.Fragment.t) @
      Html.txt " := " ::
      Tree.Relative_link.of_path ~stop_before:true (mod_path :> Paths.Path.t)
    | TypeSubst (frag_typ, td) ->
      keyword "type" ::
      Html.txt " " ::
      (Syntax.Type.handle_substitution_params
        (Tree.Relative_link.of_fragment
          ~base (frag_typ :> Paths.Fragment.t))
        [format_params td.Lang.TypeDecl.Equation.params]
      ) @
      Html.txt " := " ::
      match td.Lang.TypeDecl.Equation.manifest with
      | None -> assert false (* cf loader/cmti *)
      | Some te ->
        type_expr te

  and include_ ?theme_uri (t : Odoc_model.Lang.Include.t) =
    let docs = Comment.to_html t.doc in
    let docs = (docs :> (Html_types.div_content Html.elt) list) in
    let included_html, _, tree = signature ?theme_uri t.expansion.content in
    let should_be_inlined =
      let is_inline_tag element =
        element.Odoc_model.Location_.value = `Tag `Inline in
      List.exists is_inline_tag t.doc
    in
    let should_be_open =
      let is_open_tag element = element.Odoc_model.Location_.value = `Tag `Open in
      let is_closed_tag element =
        element.Odoc_model.Location_.value = `Tag `Closed in
      if List.exists is_open_tag t.doc then
        true
      else
        !Tree.open_details && not (List.exists is_closed_tag t.doc)
    in
    let incl =
      if should_be_inlined then
        included_html
      else
        let incl =
          Html.code (
            keyword "include" ::
            Html.txt " " ::
            module_decl' t.parent t.decl
            @
            (if Syntax.Mod.include_semicolon then [keyword ";"] else [])
          )
        in
        (* FIXME: I'd like to add an anchor here, but I don't know what id to
           give it... *)
        [
          Html.details ~a:(if should_be_open then [Html.a_open ()] else [])
            (Html.summary [Html.span ~a:[Html.a_class ["def"]] [incl]])
            included_html
        ]
    in
    [
      Html.div ~a:[Html.a_class ["spec"; "include"]]
        [Html.div ~a:[Html.a_class ["doc"]]
          (docs @ incl)]
    ],
    [],
    tree
end
open Module



module Page :
sig
  val compilation_unit : ?theme_uri:Tree.uri -> Lang.Compilation_unit.t -> Tree.t
  val page : ?theme_uri:Tree.uri -> Lang.Page.t -> Tree.t
end =
struct
  let pack
    : Odoc_model.Lang.Compilation_unit.Packed.t ->
        Html_types.div_content Html.elt list
  = fun t ->
    let open Odoc_model.Lang in
    t
    |> List.map begin fun x ->
      let modname = Paths.Identifier.name x.Compilation_unit.Packed.id in
      let md_def =
        keyword "module" ::
        Html.txt " " ::
        Html.txt modname ::
        Html.txt " = " ::
        Tree.Relative_link.of_path ~stop_before:false (x.path :> Paths.Path.t)
      in
      [Html.code md_def]
    end
    |> List.flatten
    |> fun definitions ->
      [Html.article definitions]



  let compilation_unit ?theme_uri (t : Odoc_model.Lang.Compilation_unit.t) : Tree.t =
    let package =
      match t.id with
      | `Root (a, _) -> a.package
      | _ -> assert false
    in
    Tree.enter package;
    Tree.enter (Paths.Identifier.name t.id);
    let header_docs = Comment.to_html t.doc in
    let header_docs, html, subtree =
      match t.content with
      | Module sign ->
        let html, toc, subpages = signature ?theme_uri sign in
        let header_docs =
          match toc with
          | [] -> header_docs
          | _ -> header_docs @ (Top_level_markup.render_toc toc)
        in
        header_docs, html, subpages
      | Pack packed ->
        header_docs, pack packed, []
    in
    Tree.make ~header_docs ?theme_uri html subtree



  let page ?theme_uri (t : Odoc_model.Lang.Page.t) : Tree.t =
    let package, name =
      match t.name with
      | `Page (a, name) -> a.package, name
    in
    Tree.enter package;
    Tree.enter ~kind:`Page (Odoc_model.Names.PageName.to_string name);
    let html, header_docs, toc = Top_level_markup.lay_out_page t.content in
    let html = (html :> (Html_types.div_content Html.elt) list) in
    let header_docs =
      match toc with
      | [] -> header_docs
      | _ -> header_docs @ (Top_level_markup.render_toc toc)
    in
    Tree.make ~header_docs ?theme_uri html []
end
include Page
end
