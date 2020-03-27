open Ast_mapper
open Parsetree
open Asttypes

open Tyxml_syntax

let is_jsx e =
  let f = function
    | { attr_name = {txt = "JSX"}} -> true
    | _ -> false
  in
  List.exists f e.pexp_attributes

(* When dropping support for 4.02, this module can simply be deleted. *)
module String = struct
  include String
  let lowercase_ascii = String.lowercase [@ocaml.warning "-3"]
end
module Char = struct
  include Char
  let lowercase_ascii = Char.lowercase [@ocaml.warning "-3"]
end

let lowercase_lead s =
  String.mapi (fun i c -> if i = 0 then Char.lowercase_ascii c else c) s

let to_kebab_case name =
  let length = String.length name in
  if length > 5 then
    let first = String.sub name 0 4 in
    match first with
    | "aria"
    | "data" ->
      first ^ "-" ^ lowercase_lead (String.sub name 4 (length - 4))
    | _ -> name
  else
    name

let make_attr_name name =
  let name =
    match name with
    | "className" -> "class"
    | "htmlFor" -> "for"
    | "class_" -> "class"
    | "for_" -> "for"
    | "type_" -> "type"
    | "to_" -> "to"
    | "open_" -> "open"
    | "begin_" -> "begin"
    | "end_" -> "end"
    | "in_" -> "in"
    | "method_" -> "method"
    | name -> to_kebab_case name
  in
  name

open Common

let rec filter_map f = function
  | [] -> []
  | a :: q ->
  match f a with
  | None -> filter_map f q
  | Some a -> a :: filter_map f q

(** Children *)


let make_txt ~loc ~lang s =
  let txt = Common.make ~loc lang "txt" in
  let arg = Common.wrap lang loc @@ Common.string loc s in
  Ast_helper.Exp.apply ~loc txt [Common.Label.nolabel, arg]

let element_mapper mapper e =
  match e with
  (* Convert string constant into Html.txt "constant" for convenience *)
  | { pexp_desc = Pexp_constant (Pconst_string (str, _)); pexp_loc = loc; _ } ->
    make_txt ~loc ~lang:Html str
  | _ ->
    mapper.expr mapper e

let extract_element_list mapper elements =
  let rec map acc e =
    match e with
    | [%expr []] -> List.rev acc
    | [%expr [%e? child] :: [%e? rest]] ->
      let child = Common.value (element_mapper mapper child) in
      map (child :: acc) rest
    | e ->
      List.rev (Common.antiquot (element_mapper mapper e) :: acc)
  in
  map [] elements

let extract_children mapper args =
  match
    List.find
      (function Labelled "children", _ -> true | _ -> false)
      args
  with
  | _, children -> extract_element_list mapper children
  | exception Not_found -> []

(** Attributes *)

type attr = {
  a_name: Common.name;
  a_value : string value;
  a_loc: Location.t;
}

let rec extract_attr_value ~lang a_name a_value =
  let a_name = make_attr_name a_name in
  match a_value with
  | { pexp_desc = Pexp_constant (Pconst_string (attr_value, _));
      _;
    } ->
    ((lang, a_name), Common.value attr_value)
  | e ->
    ((lang, a_name), Common.antiquot e)

and extract_attr ~lang = function
  (* Ignore last unit argument as tyxml api is pure *)
  | Nolabel, [%expr ()] -> None
  | Labelled "children", _ -> None
  | Labelled name, value ->
    Some (extract_attr_value ~lang name value)
  | Nolabel, e ->
    error e.pexp_loc "Unexpected unlabeled jsx attribute"
  | Optional name, e ->
    error e.pexp_loc "Unexpected optional jsx attribute %s" name



let classify_name ~loc hint_lang lid =
  let annotated_lang, name = match lid with
    | Longident.Ldot (Ldot (Lident s, name), "createElement")
      when String.lowercase_ascii s = "html"
      -> Some Html, lowercase_lead name
    | Longident.Ldot (Lident s, name)
      when String.lowercase_ascii s = "html"
      -> Some Html, lowercase_lead name
    | Ldot (Ldot (Lident s, name), "createElement")
      when String.lowercase_ascii s = "svg"
      -> Some Svg, lowercase_lead name
    | Longident.Ldot (Lident s, name)
      when String.lowercase_ascii s = "svg"
      -> Some Svg, lowercase_lead name
    | Lident name ->
      hint_lang, name
    | _ ->
      Common.error loc "Invalid Tyxml tag %s"
        (String.concat "." (Longident.flatten lid))
  in
  let parent_lang, elt =
    match Element.find_assembler (Html, name),
          Element.find_assembler (Svg, name),
          annotated_lang
    with
    | _, Some ("svg", _), Some l -> l, (Svg, name)
    | _, Some ("svg", _), None -> Svg, (Svg, name)
    | Some _, None, _ -> Html, (Html, name)
    | None, Some _, _ -> Svg, (Svg, name)
    | Some _, Some _, Some lang -> lang, (lang, name)
    | Some _, Some _, None ->
      (* In case of doubt, use Html *)
      Html, (Html, name)
    | None, None, _ ->
      Common.error loc "Unknown namespace for the element %s" name
  in
  parent_lang, elt

let is_homemade_component lid = match lid with
  | Longident.Ldot (( Lident s | Ldot (_, s)), "createElement") ->
    String.lowercase_ascii s <> "svg"
    && String.lowercase_ascii s <> "Html"
    && let c = s.[0] in 'A' <= c && c <= 'Z'
  | _ -> false

let mk_component ~lang ~loc f attrs children =
  let children = match children with
    | [] -> []
    | l -> [Labelled "children",  Common.list_wrap_value lang loc l]
  in
  let mk_attr ((_ns, name), v) =
    Labelled name, match v with
    | Common.Val s -> Common.string loc s
    | Common.Antiquot e -> e
  in
  let attrs = List.map mk_attr attrs in
  let args = attrs @ children @ [Nolabel,[%expr ()]] in
  Ast_helper.Exp.apply ~loc f args
  
type config = {
  mutable lang : Common.lang option ;
  mutable enabled : bool ;
}

let expr_mapper c mapper e =
  if not (is_jsx e) || not c.enabled then default_mapper.expr mapper e
  else
    let loc = e.pexp_loc in
    match e with
    (* matches <> ... </>; *)
    | [%expr []]
    | [%expr [%e? _] :: [%e? _]] ->
      let l = extract_element_list mapper e in
      Common.list_wrap_value Common.Html loc l
    (* matches <Component foo={bar}> child1 child2 </div>; *)
    | {pexp_desc = Pexp_apply
           ({ pexp_desc = Pexp_ident { txt }; _ } as f_expr, args )}
      when is_homemade_component txt
      ->
      let lang = match c.lang with
        | Some l -> l | None -> Common.Html
      in
      let attributes = filter_map (extract_attr ~lang) args in
      let children = extract_children mapper args in
      let e =
        mk_component ~loc ~lang f_expr attributes children
      in
      e
    (* matches <div foo={bar}> child1 child2 </div>; *)
    | {pexp_desc = Pexp_apply
           ({ pexp_desc = Pexp_ident { txt }; _ }, args )}
      ->
      let hint_lang = c.lang in
      let parent_lang, name = classify_name ~loc hint_lang txt in
      let lang = fst name in
      c.lang <- Some lang;
      let attributes = filter_map (extract_attr ~lang) args in
      let children = extract_children mapper args in
      let e = Element.parse ~loc
          ~parent_lang
          ~name
          ~attributes
          children
      in
      c.lang <- hint_lang ;
      e
    | _ -> default_mapper.expr mapper e

let stri_mapper c mapper stri = match stri.pstr_desc with
  | Pstr_attribute
      { attr_name = { txt = ("tyxml.jsx" | "tyxml.jsx.enable") as s } ;
        attr_payload ; attr_loc ;
      }
    ->
    begin match attr_payload with
      | PStr [%str true] -> c.enabled <- true
      | PStr [%str false] -> c.enabled <- false
      | _ ->
        Common.error
          attr_loc
          "Unexpected payload for %s. A boolean is expected." s
    end ;
    stri
  | _ -> default_mapper.structure_item mapper stri

let mapper _ _ =
  let c = { lang = None; enabled = true } in
  { default_mapper with
    expr = expr_mapper c ;
    structure_item = stri_mapper c ;
  }

let () =
  Driver.register
    ~name:"tyxml-jsx" Versions.ocaml_408
    mapper
