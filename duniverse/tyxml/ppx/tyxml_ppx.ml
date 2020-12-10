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

open Tyxml_syntax

(* When dropping support for 4.02, this module can simply be deleted. *)
module String = struct
  include String
  let capitalize_ascii = String.capitalize [@ocaml.warning "-3"]
end

open Asttypes
open Parsetree

type lang = Common.lang = Html | Svg
let lang_of_ns loc ns =
  if ns = Markup.Ns.html || ns = "" then Common.Html
  else if ns = Markup.Ns.svg then Common.Svg
  else Common.error loc "Unknown namespace %s" ns


module Loc = struct

  let shift (pos:Lexing.position) x = {pos with pos_cnum = pos.pos_cnum + x}

  let shrink {Location. loc_start ; loc_end ; loc_ghost } ~xbegin ~xend =
    { Location.loc_ghost ;
      loc_start = shift loc_start xbegin ;
      loc_end = shift loc_end xend ;
    }

  (** Returns the real (OCaml) location of the content of a string, taking
      delimiters into account. *)
  let string_start delimiter loc =
    let delimiter_length = match delimiter with
      | None -> 1
      | Some d -> String.length d + 2
    in
    shift loc.Location.loc_start delimiter_length

  (** 0-width locations do not show in the toplevel. We expand them to
      one-width.
  *)
  let one_width ?(ghost=false) pos =
    { Location.loc_ghost = ghost ;
      loc_start = pos ;
      loc_end = shift pos 1
    }

  (** Given a list of input strings for Markup.ml, evaluates to a function that
      converts Markup.ml locations of characters within these strings to their
      OCaml locations. *)
  let make_location_map located_strings =
    (* [source] is a byte stream created from the string list, which calls
       [!starting_a_string] each time it moves on to a new string in the
       list. *)
    let starting_a_string = ref (fun _ -> ()) in
    let source =
      let strings = ref located_strings in
      let offset = ref 0 in

      let rec next_byte () = match !strings with
        | [] -> None
        | (s, loc)::rest ->
          if !offset = 0 then !starting_a_string loc;

          if !offset < String.length s then begin
            offset := !offset + 1;
            Some (s.[!offset - 1])
          end
          else begin
            offset := 0;
            strings := rest;
            next_byte ()
          end
      in

      Markup.fn next_byte
    in

    (* Use Markup.ml to assign locations to characters in [source], and note
       the Markup.ml and OCaml locations of the start of each string. *)
    let location_map =
      let preprocessed_input_stream, get_markupml_location =
        source
        |> Markup.Encoding.decode Markup.Encoding.utf_8
        |> Markup.preprocess_input_stream
      in

      let location_map = ref [] in
      starting_a_string := begin fun ocaml_position ->
        location_map :=
          (get_markupml_location (), ocaml_position)::!location_map
      end;

      Markup.drain preprocessed_input_stream;
      List.rev !location_map
    in

    (* The function proper which translates Markup.ml locations into OCaml
       locations. *)
    fun given_markup_location ->
      (* [bounded_maximum None location_map] evaluates to the greatest Markup.ml
         location (and its paired OCaml location) in [location_map] that is less
         than or equal to [given_markup_location]. [best] is [Some] of the
         greatest candidate found so far, or [None] on the first iteration. *)
      let rec bounded_maximum best = function
        | [] -> best
        | ((noted_markup_location, _) as loc)::rest ->
          if Markup.compare_locations
               noted_markup_location given_markup_location > 0 then best
          else bounded_maximum (Some loc) rest
      in

      let preceding_markup_location, preceding_ocaml_position =
        match bounded_maximum None location_map with
        | Some loc -> loc
        | None -> assert false
      in

      let line, column = given_markup_location in
      let line', column' = preceding_markup_location in

      let ocaml_position =
        let open Lexing in
        if line = line' then
          {preceding_ocaml_position with
            pos_cnum = preceding_ocaml_position.pos_cnum + column - column'}
        else
          {preceding_ocaml_position with
            pos_lnum = preceding_ocaml_position.pos_lnum + line - line';
            pos_bol = 0;
            pos_cnum = column - 1}
      in

      one_width ocaml_position
end

(** Antiquotations

    We replace antiquotations expressions by a dummy token "(tyxmlX)".
    We store a table token to expression to retrieve them after parsing.
*)
module Antiquot = struct

  let fmt_id = Printf.sprintf "(tyxml%i)"
  let regex_id = Re.(seq [ str "(tyxml" ; rep digit ; char ')' ])
  let re_id = Re.compile regex_id

  let make_id =
    let r = ref 0 in
    fun () -> incr r ; fmt_id !r

  module H = Hashtbl.Make(struct
      type t = string
      let hash = Hashtbl.hash
      let equal (x:string) y = x = y
    end)

  let tbl = H.create 17

  let create expr =
    let s = make_id () in
    H.add tbl s expr ;
    s

  let get loc s =
    if H.mem tbl s then H.find tbl s
    else
      Common.error loc
        "Internal error: This expression placeholder is not registered"

  let contains loc s = match Re.exec_opt re_id s with
    | None -> `No
    | Some g ->
      let (i,j) = Re.Group.offset g 0 in
      let is_whole = i = 0 && j = String.length s in
      if is_whole
      then `Whole (get loc s)
      else `Yes (get loc @@ Re.Group.get g 0)

  let assert_no_antiquot ~loc kind (_namespace,s) =
    match contains loc s with
    | `No -> ()
    | `Yes e | `Whole e ->
      Common.error e.pexp_loc
        "OCaml expressions are not accepted as %s names" kind

end

(** Building block to rebuild the output with expressions intertwined. *)

(** Walk the text list to replace placeholders by OCaml expressions when
    appropriate. Use {!make_txt} on the rest. *)
let make_text ~loc ~lang ss =
  let buf = Buffer.create 17 in
  let push_txt buf l =
    let s = Buffer.contents buf in
    Buffer.clear buf ;
    if s = "" then l else Common.value (Common.txt ~loc ~lang s) :: l
  in
  let rec aux ~loc res = function
    | [] -> push_txt buf res
    | `Text s :: t ->
        Buffer.add_string buf s ;
        aux ~loc res t
    | `Delim g :: t ->
      let e = Antiquot.get loc @@ Re.Group.get g 0 in
      aux ~loc (Common.antiquot e :: push_txt buf res) t
  in
  aux ~loc [] @@ Re.split_full Antiquot.re_id @@ String.concat "" ss

let replace_attribute ~loc ((ns,attr_name),value) =
  let attr = (lang_of_ns loc ns, attr_name) in
  Antiquot.assert_no_antiquot ~loc "attribute" attr ;
  match Antiquot.contains loc value with
  | `No -> (attr, Common.value value)
  | `Whole e -> (attr, Common.antiquot e)
  | `Yes _ ->
      Common.error loc
      "Mixing literals and OCaml expressions is not supported in attribute values"


(** Processing *)

(** Takes the ast and transforms it into a Markup.ml char stream.

    The payload [expr] is either a single token, or an application (that is, a list).
    A token is either a string or an antiquotation. Antiquotations are replaced
    by placeholder strings (see {!Antiquot}).

    Each token is equipped with a starting (but no ending) position.
*)
let ast_to_stream expressions =

  let strings =
    expressions |> List.map @@ fun expr ->
    match Ast_convenience.get_str_with_quotation_delimiter expr with
    | Some (s, delimiter) ->
      (s, Loc.string_start delimiter expr.pexp_loc)
    | None ->
      (Antiquot.create expr, expr.pexp_loc.loc_start)
  in

  let source =
    let items = ref strings in
    let offset = ref 0 in

    let rec next_byte () = match !items with
      | [] -> None
      | (s, _)::rest ->
        if !offset < String.length s then begin
          offset := !offset + 1;
          Some (s.[!offset - 1])
        end
        else begin
          offset := 0;
          items := rest;
          next_byte ()
        end
    in

    Markup.fn next_byte
  in

  source, Loc.make_location_map strings

let context_of_lang = function
  | Common.Svg -> Some (`Fragment "svg")
  | Html -> None

(** Given the payload of a [%html ...] or [%svg ...] expression,
    converts it to a TyXML expression representing the markup
    contained therein. *)
let markup_to_expr lang loc expr =
  let context = context_of_lang lang in

  let input_stream, adjust_location = ast_to_stream expr in

  let report loc error = 
    match error with
    | `Bad_content _ -> ()
    | _ ->

      let loc = adjust_location loc in
      let message =
        Markup.Error.to_string error |> String.capitalize_ascii
      in
      Common.error loc "%s" message
  in
  let parser =
    Markup.parse_html
      ?context
      ~encoding:Markup.Encoding.utf_8
      ~report
      input_stream
  in
  let signals = Markup.signals parser in
  let get_loc () = adjust_location @@ Markup.location parser in

  let rec assemble lang children =
    match Markup.next signals with
    | None | Some `End_element -> List.rev children

    | Some (`Text ss) ->
      let loc = get_loc () in
      let node = make_text ~loc ~lang ss in
      assemble lang (node @ children)

    | Some (`Start_element ((ns, elt_name), attributes)) ->
      let newlang = lang_of_ns loc ns in
      let name = (newlang, elt_name) in
      let loc = get_loc () in

      let sub_children = assemble newlang [] in
      Antiquot.assert_no_antiquot ~loc "element" name ;
      let attributes = List.map (replace_attribute ~loc) attributes in
      let node =
        Element.parse
          ~parent_lang:lang ~loc ~name ~attributes sub_children
      in
      assemble lang (Common.Val node :: children)

    | Some (`Comment s) ->
      let loc = get_loc () in
      let node = Common.value @@ Element.comment ~loc ~lang s in
      assemble lang (node :: children)

    | Some (`Xml _ | `Doctype _ | `PI _)  ->
      assemble lang children
  in

  let l =
    Element_content.filter_surrounding_whitespace @@
    assemble lang []
  in

  match l  with
  | [ Val x | Antiquot x ] -> x
  | l -> Common.list_wrap_value lang loc l

let markup_to_expr_with_implementation lang modname loc expr =
  match modname with
  | Some modname ->
    let current_modname = Common.implementation lang in
    Common.set_implementation lang modname ;
    let res = markup_to_expr lang loc expr in
    Common.set_implementation lang current_modname ;
    res
  | _ ->
    markup_to_expr lang loc expr


let is_capitalized s =
  if String.length s < 0 then false
  else match s.[0] with
    | 'A'..'Z' -> true
    | _ -> false

(** Extract and verify the modname in the annotation [%html.Bar.Baz .. ].
    We need to fiddle with length to provide a correct location. *)
let get_modname ~loc len l =
  let s = String.concat "." l in
  let loc = Loc.shrink loc ~xbegin:(len - String.length s) ~xend:0 in
  if l = [] then None
  else if not (List.for_all is_capitalized l) then
    Common.error loc "This identifier is not a module name"
  else Some s

let re_dot = Re.(compile @@ char '.')
let dispatch_ext {txt ; loc} =
  let l = Re.split re_dot txt in
  let len = String.length txt in
  match l with
  | "html" :: l
  | "tyxml" :: "html" :: l ->
    Some (Common.Html, get_modname ~loc len l)
  | "svg" :: l
  | "tyxml" :: "svg" :: l ->
    Some (Common.Svg, get_modname ~loc len l)
  | _ -> None

let application_to_list expr =
  match expr.pexp_desc with
  | Pexp_apply (f, arguments) -> f::(List.map snd arguments)
  | _ -> [expr]


open Ast_mapper
open Ast_helper

let error { txt ; loc } =
  Common.error loc "Invalid payload for [%%%s]" txt

let markup_cases ~lang ~modname cases =
  let f ({pc_rhs} as case) =
    let loc = pc_rhs.pexp_loc in
    let pc_rhs =
      markup_to_expr_with_implementation lang modname loc @@
      application_to_list pc_rhs
    in {case with pc_rhs}
  in
  List.map f cases

let rec markup_function ~lang ~modname e =
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_fun (label,def,pat,content) ->
    let content = markup_function ~lang ~modname content in
    {e with pexp_desc = Pexp_fun (label,def,pat,content)}
  | Pexp_function cases ->
    let cases = markup_cases ~lang ~modname cases in
    {e with pexp_desc = Pexp_function cases}
  | _ ->
    markup_to_expr_with_implementation lang modname loc @@
    application_to_list e

let markup_bindings ~lang ~modname l =
  let f ({pvb_expr} as b) =
    let pvb_expr = markup_function ~lang ~modname pvb_expr in
    {b with pvb_expr}
  in
  List.map f l

let rec expr mapper e =
  match e.pexp_desc with
  | Pexp_extension (ext, payload) ->
    begin match dispatch_ext ext, payload with
    | Some (lang, modname), PStr [{pstr_desc = Pstr_eval (e, _)}] ->
      begin match e.pexp_desc with
        | Pexp_let (recflag, bindings, next) ->
          let bindings = markup_bindings ~lang ~modname bindings in
          {e with pexp_desc = Pexp_let (recflag, bindings, expr mapper next)}
        | _ ->
          markup_to_expr_with_implementation lang modname e.pexp_loc  @@
          application_to_list e
      end
    | Some _, _ -> error ext
    | None, _ -> default_mapper.expr mapper e
    end
  | _ -> default_mapper.expr mapper e

let structure_item mapper stri =
  match stri.pstr_desc with
  | Pstr_extension ((ext, payload), _attrs) ->
    begin match dispatch_ext ext, payload with
    | Some (lang, modname),
      PStr [{pstr_desc = Pstr_value (recflag, bindings) }] ->
      let bindings = markup_bindings ~lang ~modname bindings in
      Str.value recflag bindings

    | Some _, _ -> error ext
    | None, _ -> default_mapper.structure_item mapper stri
    end
  | _ -> default_mapper.structure_item mapper stri

let mapper _ _ =
  {default_mapper with expr ; structure_item}
