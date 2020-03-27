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



module Html = Tyxml.Html
module Paths = Odoc_model.Paths

open Odoc_model.Names

type syntax = OCaml | Reason

type kind = [ `Arg | `Mod | `Mty | `Class | `Cty | `Page ]

let string_of_syntax = function
  | OCaml -> "ml"
  | Reason -> "re"

type uri =
  | Absolute of string
  | Relative of string

type t = {
  name : string;
  content : [ `Html ] Html.elt;
  children : t list
}

let path = Stack.create ()

let stack_to_list s =
  let acc = ref [] in
  Stack.iter (fun x -> acc := x :: !acc) s;
  !acc

let enter ?kind name = Stack.push (name, kind) path
let leave () = ignore @@ Stack.pop path

(* FIXME: reuse [Url.kind] *)
let stack_elt_to_path_fragment = function
  | (name, None)
  | (name, Some `Page) -> name (* fixme? *)
  | (name, Some `Mod) -> name
  | (name, Some `Mty) -> "module-type-" ^ name
  | (name, Some `Arg) -> "argument-" ^ name
  | (name, Some `Class) -> "class-" ^ name
  | (name, Some `Cty) -> "class-type-" ^ name

module Relative_link = struct
  open Odoc_model.Paths

  let semantic_uris = ref false

  module Id : sig
    exception Not_linkable
    exception Can't_stop_before

    val href : ?xref_base_uri:string -> stop_before:bool -> Identifier.t -> string
  end = struct
    exception Not_linkable

    let rec drop_shared_prefix l1 l2 =
      match l1, l2 with
      | l1 :: l1s, l2 :: l2s when l1 = l2 ->
        drop_shared_prefix l1s l2s
      | _, _ -> l1, l2

    exception Can't_stop_before

    let href ?xref_base_uri ~stop_before id =
      match xref_base_uri, Url.from_identifier ~stop_before id with
      (* If xref_base_uri is defined, do not perform relative URI resolution. *)
      | Some xref_base_uri, Ok { Url. page; anchor; kind } ->
        let absolute_target =
          List.rev (
            if !semantic_uris || kind = "page" then
              page
            else
              "index.html" :: page
          )
        in
        let page = xref_base_uri ^ String.concat "/" absolute_target in
        begin match anchor with
        | "" -> page
        | anchor -> page ^ "#" ^ anchor
        end
      | None, Ok { Url. page; anchor; kind } ->
        let target =
          List.rev (
            if !semantic_uris || kind = "page" then
              page
            else
              "index.html" :: page
          )
        in
        let current_loc =
          let path =
            match Stack.top path with
            | (_, Some `Page) ->
              (* Sadness. *)
              let s = Stack.copy path in
              ignore (Stack.pop s);
              s
            | _ -> path
          in
          List.map stack_elt_to_path_fragment (stack_to_list path)
        in
        let current_from_common_ancestor, target_from_common_ancestor =
          drop_shared_prefix current_loc target
        in
        let relative_target =
          List.map (fun _ -> "..") current_from_common_ancestor
          @ target_from_common_ancestor
        in
        let page = String.concat "/" relative_target in
        begin match anchor with
        | "" -> page
        | anchor -> page ^ "#" ^ anchor
        end
      | _, Error e ->
        (* TODO: handle errors better, perhaps by returning a [result] *)
        match e with
        | Not_linkable _ -> raise Not_linkable
        | otherwise ->
          Printf.eprintf "%s\n%!" (Url.Error.to_string otherwise);
          exit 1
  end

  module Of_path = struct
    let rec to_html : stop_before:bool -> Path.t -> _ =
      fun ~stop_before path ->
        match path with
        | `Root root -> [ Html.txt root ]
        | `Forward root -> [ Html.txt root ] (* FIXME *)
        | `Dot (prefix, suffix) ->
          let link = to_html ~stop_before:true (prefix :> Path.t) in
          link @ [ Html.txt ("." ^ suffix) ]
        | `Apply (p1, p2) ->
          let link1 = to_html ~stop_before (p1 :> Path.t) in
          let link2 = to_html ~stop_before (p2 :> Path.t) in
          link1 @ Html.txt "(":: link2 @ [ Html.txt ")" ]
        | `Resolved rp ->
          let id = Path.Resolved.identifier rp in
          let txt = Url.render_path path in
          begin match Id.href ~stop_before id with
          | href -> [ Html.a ~a:[ Html.a_href href ] [ Html.txt txt ] ]
          | exception Id.Not_linkable -> [ Html.txt txt ]
          | exception exn ->
            Printf.eprintf "Id.href failed: %S\n%!" (Printexc.to_string exn);
            [ Html.txt txt ]
          end
  end

  module Of_fragment = struct
    let dot prefix suffix =
      match prefix with
      | "" -> suffix
      | _  -> prefix ^ "." ^ suffix

    let rec render_raw : Fragment.t -> string =
      fun fragment ->
        match fragment with
        | `Resolved rr -> render_resolved rr
        | `Dot (prefix, suffix) -> dot (render_raw (prefix :> Fragment.t)) suffix

    and render_resolved : Fragment.Resolved.t -> string =
      let open Fragment.Resolved in
      fun fragment ->
        match fragment with
        | `Root -> ""
        | `Subst (_, rr) -> render_resolved (rr :> t)
        | `SubstAlias (_, rr) -> render_resolved (rr :> t)
        | `Module (rr, s) -> dot (render_resolved (rr :> t)) (ModuleName.to_string s)
        | `Type (rr, s) -> dot (render_resolved (rr :> t)) (TypeName.to_string s)
        | `Class (rr, s) -> dot (render_resolved ( rr :> t)) (ClassName.to_string s)
        | `ClassType (rr, s) -> dot (render_resolved (rr :> t)) (ClassTypeName.to_string s)

    let rec to_html : stop_before:bool ->
      Identifier.Signature.t -> Fragment.t -> _ =
      fun ~stop_before id fragment ->
        let open Fragment in
        match fragment with
        | `Resolved `Root ->
          begin match Id.href ~stop_before:true (id :> Identifier.t) with
          | href ->
            [Html.a ~a:[Html.a_href href] [Html.txt (Identifier.name id)]]
          | exception Id.Not_linkable -> [ Html.txt (Identifier.name id) ]
          | exception exn ->
            Printf.eprintf "[FRAG] Id.href failed: %S\n%!" (Printexc.to_string exn);
            [ Html.txt (Identifier.name id) ]
          end
        | `Resolved rr ->
          let id = Resolved.identifier id (rr :> Resolved.t) in
          let txt = render_resolved rr in
          begin match Id.href ~stop_before id with
          | href ->
            [ Html.a ~a:[ Html.a_href href ] [ Html.txt txt ] ]
          | exception Id.Not_linkable -> [ Html.txt txt ]
          | exception exn ->
            Printf.eprintf "[FRAG] Id.href failed: %S\n%!" (Printexc.to_string exn);
            [ Html.txt txt ]
          end
        | `Dot (prefix, suffix) ->
          let link = to_html ~stop_before:true id (prefix :> Fragment.t) in
          link @ [ Html.txt ("." ^ suffix) ]
  end

  let of_path ~stop_before p =
    Of_path.to_html ~stop_before p

  let of_fragment ~base frag =
    Of_fragment.to_html ~stop_before:false base frag

  let to_sub_element ~kind name =
    (* FIXME: Reuse [Url]. *)
    let prefix =
      match kind with
      | `Mod   -> ""
      | `Mty   -> "module-type-"
      | `Arg   -> "argument-"
      | `Class -> "class-"
      | `Cty   -> "class-type-"
      | `Page  -> assert false
    in
    Html.a_href (prefix ^ name ^ (if !semantic_uris then "" else "/index.html"))
end

let render_fragment = Relative_link.Of_fragment.render_raw

let page_creator ?kind ?(theme_uri = Relative "./") ~path header_docs content =
  let rec add_dotdot ~n acc =
    if n <= 0 then
      acc
    else
      add_dotdot ~n:(n - 1) ("../" ^ acc)
  in
  let resolve_relative_uri uri =
    (* Remove the first "dot segment". *)
    let uri =
      if String.length uri >= 2 && String.sub uri 0 2 = "./" then
        String.sub uri 2 (String.length uri - 2)
      else uri
    in
    (* How deep is this page? *)
    let n =
      List.length path - (
        (* This is just horrible. *)
        match kind with
        | Some `Page -> 1
        | _ -> 0)
    in
    add_dotdot uri ~n
  in

  let name = List.hd @@ List.rev path in
  let head : Html_types.head Html.elt =
    let title_string = Printf.sprintf "%s (%s)" name (String.concat "." path) in

    let theme_uri =
      match theme_uri with
      | Absolute uri -> uri
      | Relative uri -> resolve_relative_uri uri
    in

    let support_files_uri = resolve_relative_uri "./" in

    let odoc_css_uri = theme_uri ^ "odoc.css" in
    let highlight_js_uri = support_files_uri ^ "highlight.pack.js" in

    Html.head (Html.title (Html.txt title_string)) [
      Html.link ~rel:[`Stylesheet] ~href:odoc_css_uri () ;
      Html.meta ~a:[ Html.a_charset "utf-8" ] () ;
      Html.meta ~a:[ Html.a_name "generator";
                     Html.a_content "odoc %%VERSION%%" ] ();
      Html.meta ~a:[ Html.a_name "viewport";
                  Html.a_content "width=device-width,initial-scale=1.0"; ] ();
      Html.script ~a:[Html.a_src highlight_js_uri] (Html.txt "");
      Html.script (Html.txt "hljs.initHighlightingOnLoad();");
    ]
  in

  let wrapped_content : (Html_types.div_content Html.elt) list =
    let title_prefix =
      match kind with
      | None
      | Some `Mod -> Some "Module"
      | Some `Arg -> Some "Parameter"
      | Some `Mty -> Some "Module type"
      | Some `Cty -> Some "Class type"
      | Some `Class -> Some "Class"
      | Some `Page -> None
    in

    let header_docs =
      match title_prefix with
      | None ->
        header_docs
      | Some prefix ->
        let title_heading =
          Html.h1 [
            Html.txt @@ prefix ^ " ";
            Html.code [
              (* Shorten path to at most 2 levels *)
              match List.tl path |> List.rev with
              | y :: x :: _ -> Html.txt @@ x ^ "." ^ y
              | x :: _ -> Html.txt x
              | _ -> Html.txt "" (* error *)
            ]
          ]
        in
        title_heading::header_docs
    in

    let header_content =
      let dot = if !Relative_link.semantic_uris then "" else "index.html" in
      let dotdot = add_dotdot ~n:1 dot in
      let up_href = match kind with
      | Some `Page when name <> "index" -> dot
      | _ -> dotdot
      in
      let has_parent = List.length path > 1 in
      if has_parent then
        let nav =
          Html.nav @@ [
            Html.a ~a:[Html.a_href up_href] [
              Html.txt "Up"
            ];
            Html.txt " – "
          ] @
            (* Create breadcrumbs *)
            let space = Html.txt " " in
            let breadcrumb_spec = match kind with
            | Some `Page -> (fun n x -> n, dot, x)
            | _ -> (fun n x -> n, add_dotdot ~n dot, x)
            in
            let rev_path = match kind with
            | Some `Page when name = "index" -> List.tl (List.rev path)
            | _ -> List.rev path
            in
            rev_path |>
            List.mapi breadcrumb_spec |>
            List.rev |>
            Utils.list_concat_map ?sep:(Some([space; Html.entity "#x00BB"; space]))
              ~f:(fun (n, addr, lbl) ->
                if n > 0 then
                  [[Html.a ~a:[Html.a_href addr] [Html.txt lbl]]]
                else
                  [[Html.txt lbl]]
                ) |>
            List.flatten
        in
        nav::header_docs
      else
        header_docs
    in

    let header = Html.header header_content in

    [Html.div ~a:[Html.a_class ["content"]] (header::content)]
  in

  let html : [ `Html ] Html.elt = Html.html head (Html.body wrapped_content) in

  html

let make ?(header_docs = []) ?theme_uri content children =
  assert (not (Stack.is_empty path));
  let name    = stack_elt_to_path_fragment (Stack.top path) in
  let kind    = snd (Stack.top path) in
  let path    = List.map fst (stack_to_list path) in
  let content = page_creator ?kind ?theme_uri ~path header_docs content in
  { name; content; children }

let traverse ~f t =
  let rec aux parents node =
    f ~parents node.name node.content;
    List.iter (aux (node.name :: parents)) node.children
  in
  aux [] t

let open_details = ref true
