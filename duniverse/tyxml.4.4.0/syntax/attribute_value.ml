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

[@@@ocaml.warning "-3"]

open Ast_helper

type 'a gparser =
  ?separated_by:string -> ?default:string -> Location.t -> string -> 'a ->
    Parsetree.expression option

type parser = string gparser
type vparser = string Common.value gparser

(* Handle expr *)

let expr (parser : parser) : vparser =
  fun ?separated_by ?default loc name v ->
    match v with
    | Antiquot e -> Some e
    | Val s -> parser ?separated_by ?default loc name s

(* Options. *)

let option none (parser : parser) ?separated_by:_ ?default:_ loc name s =
  if s = none then Some [%expr None] [@metaloc loc]
  else
    match parser ~default:none loc name s with
    | None -> None
    | Some e -> Some [%expr Some [%e e]] [@metaloc loc]



(* Lists. *)

let filter_map f l =
  l
  |> List.fold_left (fun acc v ->
    match f v with
    | None -> acc
    | Some v' -> v'::acc)
    []
  |> List.rev

(* Splits the given string on the given delimiter (a regular expression), then
   applies [element_parser] to each resulting component. Each such application
   resulting in [Some expr] is included in the resulting expression list. *)
let exp_list delimiter separated_by (element_parser : parser) loc name s =
  Re_str.split delimiter s
  |> filter_map (element_parser ~separated_by loc name)

(* Behaves as _expr_list, but wraps the resulting expression list as a list
   expression. *)
let list
    delimiter separated_by element_parser ?separated_by:_ ?default:_ loc name s =

  exp_list delimiter separated_by element_parser loc name s
  |> Common.list loc
  |> fun e -> Some e

let spaces = list (Re_str.regexp " +") "space"
let commas = list (Re_str.regexp " *, *") "comma"
let semicolons = list (Re_str.regexp " *; *") "semicolon"

let spaces_or_commas_regexp = Re_str.regexp "\\( *, *\\)\\| +"
let spaces_or_commas_ = exp_list spaces_or_commas_regexp "space- or comma"
let spaces_or_commas = list spaces_or_commas_regexp "space- or comma"



(* Wrapping. *)

let wrap (parser : parser) implementation =
  expr @@
  fun ?separated_by:_ ?default:_ loc name s ->
  match parser loc name s with
  | None -> Common.error loc "wrap applied to presence; nothing to wrap"
  | Some e -> Some (Common.wrap implementation loc e)

let nowrap (parser : parser) _ =
  expr @@
  fun ?separated_by:_ ?default:_ loc name s ->
  parser loc name s



(* Error reporting for values in lists and options. *)

let must_be_a
    singular_description plural_description separated_by default loc name =

  let description =
    match separated_by with
    | Some separated_by ->
      Printf.sprintf "a %s-separated list of %s" separated_by plural_description
    | None ->
      match default with
      | Some default -> Printf.sprintf "%s or %s" singular_description default
      | None -> singular_description
  in

  Common.error loc "Value of %s must be %s" name description



(* General helpers. *)

(* Checks that the given string matches the given regular expression exactly,
   i.e. the match begins at position 0 and ends at the end of the string. *)
let does_match regexp s =
  Re_str.string_match regexp s 0 && Re_str.match_end () = String.length s

(* Checks that the group with the given index was matched in the given
   string. *)
let group_matched index s =
  try Re_str.matched_group index s |> ignore; true
  with Not_found -> false

let int_exp loc s =
  try Some (Common.int loc (int_of_string s))
  with Failure _ -> None

let float_exp loc s =
  try
    Some (Common.float loc @@ float_of_string s)
  with Failure _ ->
    None

let bool_exp loc b =
  let s = if b then "true" else "false" in
  Exp.construct ~loc (Location.mkloc (Longident.Lident s) loc) None

(* Numeric. *)

let char ?separated_by:_ ?default:_ loc name s =
  let encoding = `UTF_8 in (* OCaml source files are always in utf8 *)
  let decoder = Uutf.decoder ~encoding (`String s) in

  let c =
    match Uutf.decode decoder with
    | `End -> Common.error loc "No character in attribute %s" name
    | `Uchar i when Uchar.is_char i -> Uchar.to_char i
    | `Uchar _ ->
      Common.error loc "Character out of range in attribute %s" name
    | `Await -> assert false
    | `Malformed s ->
      Common.error loc "Malformed character %s in attribute %s" s name
  in
  begin match Uutf.decode decoder with
  | `End -> ()
  | _ -> Common.error loc "Multiple characters in attribute %s" name
  end;
  Some (with_default_loc loc @@ fun () -> Ast_convenience.char c)

let onoff ?separated_by:_ ?default:_ loc name s =
  let b = match s with
    | "" | "on" -> true
    | "off" -> false
    | _ ->
      Common.error loc {|Value of %s must be "on", "" or "off"|} name
  in
  Some (bool_exp loc b)

let bool ?separated_by:_ ?default:_ loc name s =
  let b = match s with
    | "" | "true" -> true
    | "false" -> false
    | _ ->
      Common.error loc {|Value of %s must be "true", "" or "false"|} name
  in
  Some (bool_exp loc b)

let unit ?separated_by:_ ?default:_ loc name s =
  if s = "" || s = name then
    Some (Ast_convenience.(with_default_loc loc unit))
  else
    Common.error loc
      {|Value of %s must be %s or "".|}
      name name

let int ?separated_by ?default loc name s =
  match int_exp loc s with
  | Some _ as e -> e
  | None ->
    must_be_a "a whole number" "whole numbers" separated_by default loc name

let float ?separated_by ?default loc name s =
  match float_exp loc s with
  | Some _ as e -> e
  | None ->
    must_be_a
      "a number (decimal fraction)" "numbers (decimal fractions)"
      separated_by default loc name

let points ?separated_by:_ ?default:_ loc name s =
  let expressions = spaces_or_commas_ float loc name s in

  let rec pair acc = function
    | [] -> List.rev acc |> Common.list loc
    | [_] -> Common.error loc "Unpaired coordinate in %s" name
    | ex::ey::rest -> pair (([%expr [%e ex], [%e ey]] [@metaloc loc])::acc) rest
  in

  Some (pair [] expressions)

let number_pair ?separated_by:_ ?default:_ loc name s =
  let e =
    begin match spaces_or_commas_ float loc name s with
    | [orderx] -> [%expr [%e orderx], None]
    | [orderx; ordery] -> [%expr [%e orderx], Some [%e ordery]]
    | _ -> Common.error loc "%s requires one or two numbers" name
    end [@metaloc loc]
  in

  Some e

let fourfloats ?separated_by:_ ?default:_ loc name s =
  match spaces_or_commas_ float loc name s with
  | [min_x; min_y; width; height] ->
    Some [%expr ([%e min_x], [%e min_y], [%e width], [%e height])]
      [@metaloc loc]
  | _ -> Common.error loc "Value of %s must be four numbers" name

(* These are always in a list; hence the error message. *)
let icon_size =
  let regexp = Re_str.regexp "\\([0-9]+\\)[xX]\\([0-9]+\\)" in

  fun ?separated_by:_ ?default:_ loc name s ->
    if not @@ does_match regexp s then
      Common.error loc "Value of %s must be a %s, or %s"
        name "space-separated list of icon sizes, such as 16x16" "any";

    let width, height =
      try
        int_of_string (Re_str.matched_group 1 s),
        int_of_string (Re_str.matched_group 2 s)
      with Invalid_argument _ ->
        Common.error loc "Icon dimension out of range in %s" name
    in

    Some
      [%expr
        [%e Common.int loc width],
        [%e Common.int loc height]] [@metaloc loc]



(* Dimensional. *)

let svg_quantity =
  let integer = "[+-]?[0-9]+" in
  let integer_scientific = Printf.sprintf "%s\\([Ee]%s\\)?" integer integer in
  let fraction = Printf.sprintf "[+-]?[0-9]*\\.[0-9]+\\([Ee]%s\\)?" integer in
  let number = Printf.sprintf "%s\\|%s" integer_scientific fraction in
  let quantity = Printf.sprintf "\\(%s\\)\\([^0-9]*\\)$" number in
  let regexp = Re_str.regexp quantity in

  fun kind_singular kind_plural parse_unit ?separated_by ?default loc name s ->
    if not @@ does_match regexp s then
      must_be_a kind_singular kind_plural separated_by default loc name;

    let n =
      match float_exp loc (Re_str.matched_group 1 s) with
      | Some n -> n
      | None -> Common.error loc "Number out of range in %s" name
    in

    let unit_string = Re_str.matched_group 4 s in
    let unit =
      (if unit_string = "" then [%expr None]
      else [%expr Some [%e parse_unit loc name unit_string]]) [@metaloc loc]
    in

    [%expr [%e n], [%e unit]] [@metaloc loc]

let svg_length =
  let parse_unit loc name unit =
    begin match unit with
    | "cm" -> [%expr `Cm]
    | "em" -> [%expr `Em]
    | "ex" -> [%expr `Ex]
    | "in" -> [%expr `In]
    | "mm" -> [%expr `Mm]
    | "pc" -> [%expr `Pc]
    | "pt" -> [%expr `Pt]
    | "px" -> [%expr `Px]
    | "%" -> [%expr `Percent]
    | s -> Common.error loc "Invalid length unit %s in %s" s name
    end [@metaloc loc]
  in

  fun ?separated_by ?default loc name s ->
    Some
      (svg_quantity "an SVG length" "SVG lengths" parse_unit
        ?separated_by ?default loc name s)

let angle_ =
  let parse_unit loc name unit =
    begin match unit with
    | "deg" -> [%expr `Deg]
    | "rad" -> [%expr `Rad]
    | "grad" -> [%expr `Grad]
    | s -> Common.error loc "Invalid angle unit %s in %s" s name
    end [@metaloc loc]
  in

  svg_quantity "an SVG angle" "SVG angles" parse_unit

let angle ?separated_by ?default loc name s =
  Some (angle_ ?separated_by ?default loc name s)

let offset =
  let bad_form name loc =
    Common.error loc "Value of %s must be a number or percentage" name in

  let regexp = Re_str.regexp "\\([-+0-9eE.]+\\)\\(%\\)?" in

  fun ?separated_by:_ ?default:_ loc name s ->
    if not @@ does_match regexp s then bad_form name loc;

    begin
      let n =
        match float_exp loc (Re_str.matched_group 1 s) with
        | Some n -> n
        | None -> bad_form name loc
      in

      if group_matched 2 s then Some [%expr `Percentage [%e n]]
      else Some [%expr `Number [%e n]]
    end [@metaloc loc]

let transform =
  let regexp = Re_str.regexp "\\([^(]+\\)(\\([^)]*\\))" in

  fun ?separated_by:_ ?default:_ loc name s ->
    if not @@ does_match regexp s then
      Common.error loc "Value of %s must be an SVG transform" name;

    let kind = Re_str.matched_group 1 s in
    let values = Re_str.matched_group 2 s in

    let e =
      begin match kind with
      | "matrix" ->
        begin match spaces_or_commas_ float loc "matrix" values with
        | [a; b; c; d; e; f] ->
          [%expr `Matrix ([%e a], [%e b], [%e c], [%e d], [%e e], [%e f])]
        | _ ->
          Common.error loc "%s: matrix requires six numbers" name
        end

      | "translate" ->
        begin match spaces_or_commas_ float loc "translate" values with
        | [tx; ty] -> [%expr `Translate ([%e tx], Some [%e ty])]
        | [tx] -> [%expr `Translate ([%e tx], None)]
        | _ ->
          Common.error loc "%s: translate requires one or two numbers" name
        end

      | "scale" ->
        begin match spaces_or_commas_ float loc "scale" values with
        | [sx; sy] -> [%expr `Scale ([%e sx], Some [%e sy])]
        | [sx] -> [%expr `Scale ([%e sx], None)]
        | _ -> Common.error loc "%s: scale requires one or two numbers" name
        end

      | "rotate" ->
        begin match Re_str.bounded_split spaces_or_commas_regexp values 2 with
        | [a] -> [%expr `Rotate ([%e angle_ loc "rotate" a], None)]
        | [a; axis] ->
          begin match spaces_or_commas_ float loc "rotate axis" axis with
          | [cx; cy] ->
            [%expr `Rotate
              ([%e angle_ loc "rotate" a], Some ([%e cx], [%e cy]))]
          | _ ->
            Common.error loc "%s: rotate center requires two numbers" name
          end
        | _ ->
          Common.error loc
            "%s: rotate requires an angle and an optional center" name
        end

      | "skewX" -> [%expr `SkewX [%e angle_ loc "skewX" values]]

      | "skewY" -> [%expr `SkewY [%e angle_ loc "skewY" values]]

      | s -> Common.error loc "%s: %s is not a valid transform type" name s
      end [@metaloc loc]
    in

    Some e



(* String-like. *)

let string ?separated_by:_ ?default:_ loc _ s =
  Some (with_default_loc loc @@ fun () -> Ast_convenience.str s)

let variand s =
  let without_backtick s =
    let length = String.length s in
    String.sub s 1 (length - 1)
  in

  s |> Name_convention.polyvar |> without_backtick

let variant ?separated_by:_ ?default:_ loc _ s =
  Some (Exp.variant ~loc (variand s) None)

let total_variant (unary, nullary) ?separated_by:_ ?default:_ loc _name s =
  let variand = variand s in
  if List.mem variand nullary then Some (Exp.variant ~loc variand None)
  else Some (Exp.variant ~loc unary (Some (Common.string loc s)))

let variant_or_empty empty ?separated_by:_ ?default:_ loc _ s =
  let variand = variand s in
  let variand = if variand = "" then empty else variand in
  Some (Exp.variant ~loc variand None)


(* Miscellaneous. *)

let presence ?separated_by:_ ?default:_ _ _ _ = None

let paint_without_icc loc _name s =
  begin match s with
  | "none" ->
    [%expr `None]

  | "currentColor" ->
    [%expr `CurrentColor]

  | _ ->
    let icc_color_start =
      try Some (Re_str.search_forward (Re_str.regexp "icc-color(\\([^)]*\\))") s 0)
      with Not_found -> None
    in

    match icc_color_start with
    | None -> [%expr `Color ([%e Common.string loc s], None)]
    | Some i ->
      let icc_color = Re_str.matched_group 1 s in
      let color = String.sub s 0 i in
      [%expr `Color
        ([%e Common.string loc color],
         Some [%e Common.string loc icc_color])]
  end [@metaloc loc]

let paint ?separated_by:_ ?default:_ loc name s =
  if not @@ Re_str.string_match (Re_str.regexp "url(\\([^)]+\\))") s 0 then
    Some (paint_without_icc loc name s)
  else
    let iri = Re_str.matched_group 1 s |> Common.string loc in
    let remainder_start = Re_str.group_end 0 in
    let remainder_length = String.length s - remainder_start in
    let remainder =
      String.sub s remainder_start remainder_length |> String.trim in

    begin
      if remainder = "" then
        Some [%expr `Icc ([%e iri], None)]
      else
        Some
          [%expr
            `Icc ([%e iri], Some [%e paint_without_icc loc name remainder])]
    end [@metaloc loc]

let srcset_element =
  let space = Re_str.regexp " +" in

  fun ?separated_by:_ ?default:_ loc name s ->
    let e =
      begin match Re_str.bounded_split space s 2 with
      | [url] ->
        [%expr `Url [%e Common.string loc url]]

      | [url; descriptor] ->
        let bad_descriptor () =
          Common.error loc "Bad width or density descriptor in %s" name in

        let url = Common.string loc url in
        let suffix_index = String.length descriptor - 1 in

        let is_width =
          match descriptor.[suffix_index] with
          | 'w' -> true
          | 'x' -> false
          | _ -> bad_descriptor ()
          | exception Invalid_argument _ -> bad_descriptor ()
        in

        if is_width then
          let n =
            match int_exp loc (String.sub descriptor 0 suffix_index) with
            | Some n -> n
            | None ->
              Common.error loc "Bad number for width in %s" name
          in

          [%expr `Url_width ([%e url], [%e n])]

        else
          let n =
            match float_exp loc (String.sub descriptor 0 suffix_index) with
            | Some n -> n
            | None ->
              Common.error loc "Bad number for pixel density in %s" name
          in

          [%expr `Url_pixel ([%e url], [%e n])]

      | _ -> Common.error loc "Missing URL in %s" name
      end [@metaloc loc]
    in

    Some e

let number_or_datetime ?separated_by:_ ?default:_ loc _ s =
  match int_exp loc s with
  | Some n -> Some [%expr `Number [%e n]]
  | None -> Some [%expr `Datetime [%e Common.string loc s]]
  [@metaloc loc]



(* Special-cased. *)

let sandbox = spaces variant

let in_ = total_variant Svg_types_reflected.in_value

let in2 = in_

let xmlns ?separated_by:_ ?default:_ loc name s =
  if s <> "http://www.w3.org/1999/xhtml" then
    Common.error loc "%s: namespace must be http://www.w3.org/1999/xhtml" name;
  Some [%expr `W3_org_1999_xhtml] [@metaloc loc]
