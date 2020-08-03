(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2016 Anton Bachin
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02110-1301, USA.
*)

(* Runs on [html_sigs.mli], [svg_sigs.mli], and [html_types.mli]. Certain type
   and value declarations are read for type information, which is stored in
   corresponding [_reflected] files - for example, [html_sigs.mli] results in
   [html_sigs_reflected.ml]. See comments by functions below and in
   [sigs_reflected.mli] for details. *)

open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
module AC = Ast_convenience


let find_attr s l =
  let f attr = attr.attr_name.txt = s in
  try Some (List.find f l)
  with Not_found -> None

let is_attribute s = String.length s >= 2 && String.sub s 0 2 = "a_"

let strip_a s =
  if String.length s < 2 || String.sub s 0 2 <> "a_" then s
  else String.sub s 2 (String.length s - 2)

(** Utilities for types of functions. *)
module FunTyp = struct

  (* Extract the tuple (arguments, return) of a function type. *)
  let get t =
    let rec scan acc = function
      | {ptyp_desc = Ptyp_arrow (lab, t, t')} -> scan ((lab,t)::acc) t'
      | ret -> (List.rev acc, ret)
    in
    scan [] t

  let arguments t = fst @@ get t
  let result t = snd @@ get t

  exception Found

  (** Check if a type contains the "elt" constructor, somewhere. *)
  let contains_elt t =
    (* Ast_iterator is not available in 4.02, so we use a mapper. *)
    let typ mapper = function
      | [%type: [%t? _] elt] -> raise Found
      | ty -> default_mapper.typ mapper ty
    in
    let m = {Ast_mapper.default_mapper with typ} in
    try ignore (m.typ m t) ; false
    with Found -> true

  (** Extract the type inside [wrap]. *)
  let unwrap = function
    (* Optional argument are [_ wrap *predef*.option], In 4.02 *)
    | {ptyp_desc = Ptyp_constr (lid, [[%type: [%t? _] wrap] as t])}
      when Longident.last lid.txt = "option" ->
      Some t
    | [%type: [%t? _] wrap] as t -> Some t
    | _ -> None

  (** Extract the type of for html/svg attributes. *)
  let extract_attribute_argument (lab, t) =
    if contains_elt t then None
    else match AC.Label.explode lab, unwrap t with
      | Nolabel, _ | _, None -> None
      | (Labelled lab | Optional lab), Some t -> Some (lab, t)

  let rec no_constructor_arguments = function
    | [] -> true
    | {prf_desc = Rinherit _}::_
    | {prf_desc = Rtag (_, _, _::_)}::_ -> false
    | {prf_desc = Rtag (_, _, [])}::more -> no_constructor_arguments more


(* Given the name of a TyXML attribute function and a list of its argument
   types, selects the attribute value parser (in module [Attribute_value])
   that should be used for that attribute. *)
let rec to_attribute_parser lang name = function
  | [] -> [%expr nowrap presence]
  | [[%type: [%t? ty] wrap]] ->
    [%expr wrap [%e to_attribute_parser lang name [ty]]]

  | [[%type: character]] -> [%expr char]
  | [[%type: bool] as ty]
    when AC.has_attr "onoff" ty.ptyp_attributes -> [%expr onoff]
  | [[%type: bool]] -> [%expr bool]
  | [[%type: unit]] -> [%expr nowrap unit]

  | [[%type: number]] when lang = `Html -> [%expr int]
  | [[%type: pixels]]
  | [[%type: int]] -> [%expr int]

  | [[%type: numbers]] when lang = `Html -> [%expr commas int]

  | [[%type: number]] when lang = `Svg -> [%expr float]
  | [[%type: float_number]] | [[%type: float]] -> [%expr float]

  | [[%type: float_number option]] ->
    [%expr option "any" float]

  | [[%type: numbers_semicolon]] ->
    [%expr semicolons float]

  | [[%type: numbers]] when lang = `Svg ->
    [%expr spaces_or_commas float]

  | [[%type: fourfloats]] ->
    [%expr fourfloats]

  | [[%type: number_optional_number]] ->
    [%expr number_pair]

  | [[%type: coords]] ->
    [%expr points]

  | [[%type: (number * number) list option]] ->
    [%expr option "any" (spaces icon_size)]

  | [[%type: coord]] | [[%type: Unit.length]] ->
    [%expr svg_length]

  | [[%type: Unit.length list]] ->
    [%expr spaces_or_commas svg_length]

  | [[%type: Unit.angle option]] ->
    [%expr option "auto" angle]

  | [[%type: string]]
  | [[%type: text]]
  | [[%type: nmtoken]]
  | [[%type: idref]]
  | [[%type: Xml.uri]]
  | [[%type: contenttype]]
  | [[%type: languagecode]]
  | [[%type: cdata]]
  | [[%type: charset]]
  | [[%type: frametarget]]
  | [[%type: iri]]
  | [[%type: color]] -> [%expr string]

  | [[%type: nmtoken]; [%type: text wrap]] -> [%expr wrap string]
  | [[%type: string]; [%type: string wrap]] -> [%expr wrap string]
  | [[%type: string]; [%type: string list wrap]] -> [%expr wrap (spaces string)]

  | [[%type: Xml.event_handler]]
  | [[%type: Xml.mouse_event_handler]]
  | [[%type: Xml.keyboard_event_handler]]
  | [[%type: Xml.touch_event_handler]] ->
    [%expr nowrap string]

  | [[%type: string option]] ->
    [%expr (option "" string)]

  | [{ptyp_desc = Ptyp_variant (_::_::_ as constructors, _, _)}]
      when no_constructor_arguments constructors ->
    [%expr variant]

  | [[%type: shape]] ->
    [%expr variant]

  | [[%type: nmtokens]]
  | [[%type: idrefs]]
  | [[%type: charsets]]
  | [[%type: spacestrings]]
  | [[%type: strings]]
  | [[%type: string list]] ->
    [%expr spaces string]

  | [[%type: commastrings]]
  | [[%type: text list]]
  | [[%type: contenttypes]] ->
    [%expr commas string]

  | [[%type: linktypes]] ->
    [%expr spaces (total_variant Html_types_reflected.linktype)]

  | [[%type: referrerpolicy]] ->
    [%expr variant_or_empty "Empty"]

  | [[%type: mediadesc]] ->
    [%expr commas (total_variant Html_types_reflected.mediadesc_token)]

  | [[%type: lengths]] ->
    [%expr spaces_or_commas svg_length]

  | [[%type: transforms]] ->
    [%expr spaces_or_commas transform]

  | [[%type: paint]] ->
    [%expr paint]

  | [[%type: number_or_datetime]] ->
    [%expr number_or_datetime]

  | [[%type: image_candidate list]] ->
    [%expr commas srcset_element]

  | _ ->
    let name = strip_a name in
    let name = if name = "in" then "in_" else name in
    AC.evar name

end

(* Given a list of attributes from a val declaration whose name begins with a_,
   checks if the declaration has a [@@reflect.attribute] annotation. If so, the
   declaration's name does not directly correspond to markup attribute name
   (e.g. "a_input_max" does not directly correspond to "max"). The annotation is
   parsed to get the markup name and the element types in which the translation
   from markup name to TyXML name should be performed. *)
let ocaml_attributes_to_renamed_attribute name attributes =
  let maybe_attribute = find_attr "reflect.attribute" attributes in

  match maybe_attribute with
  | None -> []
  | Some {attr_loc = loc; attr_payload = payload} ->
    let error () =
      Location.raise_errorf ~loc
        "Payload of [@@reflect.attribute] must be a string and a string list"
    in
    match payload with
    | PStr [%str
        [%e? const]
        [%e? element_names]] ->
      begin match Ast_convenience.get_str const with
        | None -> error ()
        | Some real_name ->
          let element_names =
            let error loc =
              Location.raise_errorf ~loc
                "List in [@@reflect.attribute] must contain strings"
            in
            let rec traverse acc = function
              | [%expr [%e? e]::[%e? tail]] ->
                begin match Ast_convenience.get_str e with
                  | Some element_name -> traverse (element_name::acc) tail
                  | None -> error e.pexp_loc
                end
              | [%expr []] -> acc
              | {pexp_loc} -> error pexp_loc
            in
            traverse [] element_names
          in
          [name, real_name, element_names]
      end
    | _ -> error ()

(* Given a val declaration, determines whether it is for an element. If so,
   evaluates to the element's child assembler (from module
   [Element_content]), list of attributes passed as labeled arguments, and
   markup name, if different from its TyXML name (for example, [object_] is
   [object] in markup).

   A val declaration is for an element if it either has a [@@reflect.element]
   attribute, or its result type is [_ nullary], [_ unary], or [_ star].

   Also understands the [@@reflect.filter_whitespace] attribute. *)
let val_item_to_element_info lang value_description =
  let name = value_description.pval_name.txt in

  let maybe_attribute =
    find_attr "reflect.element" value_description.pval_attributes
  in

  let maybe_assembler, real_name =
    match maybe_attribute with
    | Some { attr_loc = loc ; attr_payload = payload} ->
      let assembler, real_name = match payload with
        | PStr [%str [%e? assembler] [%e? name]] ->
          Ast_convenience.get_str assembler, Ast_convenience.get_str name
        | PStr [%str [%e? assembler]] ->
          Ast_convenience.get_str assembler, None
        | _ -> None, None
      in
      begin match assembler with
        | Some _ -> (assembler, real_name)
        | None ->
          Location.raise_errorf ~loc
            "Payload of [@@reflect.element] must be one or two strings"
      end

    | None ->
      let result_type = FunTyp.result value_description.pval_type in
      let assembler = match result_type with
        | [%type: ([%t? _], [%t ? _]) nullary] -> Some "nullary"
        | [%type: ([%t? _], [%t ? _], [%t ? _]) unary] -> Some "unary"
        | [%type: ([%t? _], [%t ? _], [%t ? _]) star] -> Some "star"
        | _ -> None
      in assembler, None
  in

  match maybe_assembler with
  | None -> None
  | Some assembler ->

    (* We gather all the labeled arguments that are attributes. *)
    let arguments = FunTyp.arguments value_description.pval_type in
    let labeled_attributes =
      let aux x acc = match FunTyp.extract_attribute_argument x with
        | None -> acc
        | Some (label, ty) ->
          let parser = FunTyp.to_attribute_parser lang label [ty] in
          (name, label, parser) :: acc
      in
      List.fold_right aux arguments []
    in

    let rename =
      match real_name with
      | None -> []
      | Some real_name -> [real_name, name]
    in

    let assembler = [ assembler ] in

    let assembler =
      match
        find_attr "reflect.filter_whitespace" value_description.pval_attributes
      with
      | Some _  -> "comp_filter_whitespace" :: assembler
      | None    -> assembler
    in

    Some (assembler, labeled_attributes, rename)



let attribute_parsers = ref []
let labeled_attributes = ref []
let renamed_attributes = ref []
let element_assemblers = ref []
let renamed_elements = ref []

(* Walks over signature items, looking for elements and attributes. Calls the
   functions immediately above, and accumulates their results in the above
   references. This function is relevant for [html_sigs.mli] and
   [svg_sigs.mli]. *)
let signature_item lang mapper item =
  begin match item.psig_desc with
  | Psig_value {pval_name = {txt = name}; pval_type = type_; pval_attributes}
      when is_attribute name ->
    (* Attribute declaration. *)

    let argument_types = List.map snd @@ FunTyp.arguments type_ in
    let attribute_parser_mapping =
      name, FunTyp.to_attribute_parser lang name argument_types in
    attribute_parsers := attribute_parser_mapping::!attribute_parsers;

    let renaming = ocaml_attributes_to_renamed_attribute name pval_attributes in
    renamed_attributes := renaming @ !renamed_attributes

  | Psig_value v ->
    (* Non-attribute, but potentially an element declaration. *)

    begin match val_item_to_element_info lang v with
    | None -> ()
    | Some (assembler, labeled_attributes', rename) ->
      element_assemblers := (v.pval_name.txt, assembler)::!element_assemblers;
      labeled_attributes := labeled_attributes' @ !labeled_attributes;
      renamed_elements := rename @ !renamed_elements
    end

  | _ -> ()
  end;

  default_mapper.signature_item mapper item



let reflected_variants = ref []

(* Walks over type declarations (which will be in signature items). For each
   that is marked with [@@reflect.total_variant], expects it to be a polymorphic
   variant. Splits the constructors into those that have no arguments, and one
   constructor that has one string argument. This constructor information is
   accumulated in [reflected_variants]. This function is relevant for
   [html_types.mli]. *)
let type_declaration mapper declaration =
  let is_reflect attr = attr.attr_name.txt = "reflect.total_variant" in
  if List.exists is_reflect declaration.ptype_attributes then begin
    let name = declaration.ptype_name.txt in

    match declaration.ptype_manifest with
    | Some {ptyp_desc = Ptyp_variant (rows, _, _); ptyp_loc} ->
      let rows =
        rows |> List.map (function
          | {prf_desc = Rtag (label, _, types)} -> label, types
          | {prf_desc = Rinherit {ptyp_loc}} ->
            Location.raise_errorf ~loc:ptyp_loc
              "Inclusion is not supported by [@@reflect.total_variant]")
      in

      let nullary, unary =
        List.partition (fun (_, types) -> types = []) rows in

      let unary =
        match unary with
        | [name, [[%type: string]]] -> name.txt
        | _ ->
          Location.raise_errorf ~loc:ptyp_loc
            "Expected exactly one non-nullary constructor `C of string"
      in

      let nullary = List.map (fun ({txt},_) -> txt) nullary in

      reflected_variants := (name, (unary, nullary))::!reflected_variants

    | _ ->
      Location.raise_errorf ~loc:declaration.ptype_loc
        "[@@reflect.total_variant] expects a polymorphic variant type"
  end;

  default_mapper.type_declaration mapper declaration

(** Small set of combinators to help {!make_module}. *)
module Combi = struct
  let list f l = AC.list @@ List.map f l
  let tuple2 f1 f2 (x1, x2) = Exp.tuple [f1 x1; f2 x2]
  let tuple3 f1 f2 f3 (x1, x2, x3) = Exp.tuple [f1 x1; f2 x2; f3 x3]
  let str = AC.str
  let id = AC.evar
  let expr x = x
  let let_ p f (x,e) = Str.value Nonrecursive [Vb.mk (p x) (f e)]
  let rec compose_ids =
    function
    | [ i ]   -> id i
    | i :: tl -> AC.app (id i) [compose_ids tl]
    | []      -> assert false
end

(** Create a module based on the various things collected while reading the file. *)
let emit_module () =
  begin if !attribute_parsers <> [] then [%str
    open Attribute_value

    let attribute_parsers =
      [%e Combi.(list @@ tuple2 str expr) !attribute_parsers ]
    let renamed_attributes =
      [%e Combi.(list @@ tuple3 str str (list str)) !renamed_attributes ]
    let labeled_attributes =
      [%e Combi.(list @@ tuple3 str str expr) !labeled_attributes ]

    open Element_content

    let element_assemblers =
      [%e Combi.(list @@ tuple2 str compose_ids) !element_assemblers ]
    let renamed_elements =
      [%e Combi.(list @@ tuple2 str str) !renamed_elements ]

    ] else []
  end @

  List.map Combi.(let_ AC.pvar (tuple2 str (list str))) !reflected_variants


(* Crude I/O tools to read a signature and output a structure.
   The executable will take as first argument the name of the signature
   and as second argument the name of the structure.

*)
let version =  Versions.ocaml_408

let read_sig filename =
  Location.input_name := filename ;
  let handle =
    try open_in filename
    with Sys_error msg -> prerr_endline msg; exit 1
  in
  let buf = Lexing.from_channel handle in
  Location.init buf filename ;
  let ast = Parse.interface version buf in
  close_in handle ;
  ast

let write_struct filename ast =
  let {Versions. copy_structure; _ } =
    Versions.migrate version Versions.ocaml_current in
  let ast = copy_structure ast in
  let handle =
    try open_out filename
    with Sys_error msg -> prerr_endline msg; exit 1
  in
  let fmt = Format.formatter_of_out_channel handle in
  Format.fprintf fmt "%a@." Pprintast.structure ast ;
  close_out handle

let () =
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "Usage: %s IN OUT\n" Sys.argv.(0);
    exit 2
  end;

  let in_file = Sys.argv.(1) in
  let out_file = Sys.argv.(2) in
  Ast_helper.default_loc := Location.in_file in_file ;

  let lang =
    let basename = Filename.basename in_file in
    let svg_prefix = "svg_" in
    if String.length basename >= String.length svg_prefix
       && String.sub basename 0 (String.length svg_prefix) = svg_prefix
    then `Svg
    else `Html
  in

  let mapper =
    let signature_item = signature_item lang in
    {default_mapper with signature_item; type_declaration}
  in

  let reflected_struct sig_ =
    ignore @@ mapper.signature mapper sig_ ;
    emit_module ()
  in

  try
    read_sig in_file
    |> reflected_struct
    |> write_struct out_file
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
