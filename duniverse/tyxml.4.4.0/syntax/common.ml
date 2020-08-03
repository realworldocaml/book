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

open Ast_helper
module Label = Ast_convenience.Label

(** Lang utilities *)

type lang = Html | Svg

type name = lang * string

let html_implementation = ref "Html"
let svg_implementation = ref "Svg"

let implemenentation_ref = function
  | Html -> html_implementation
  | Svg -> svg_implementation

let set_implementation lang s =
  (implemenentation_ref lang) := s

let implementation lang =
  !(implemenentation_ref lang)

let lang = function
  | Html -> "HTML"
  | Svg -> "SVG"

let make_lid ~loc i s =
  Location.mkloc
    (Longident.parse @@ implementation i ^ "." ^ s)
    loc

let make ~loc i s =
  Exp.ident ~loc @@ make_lid ~loc i s

(** Generic *)

let find f l =
  try Some (List.find f l)
  with Not_found -> None

let with_loc loc f x =
  with_default_loc loc @@ fun () -> f x

let error loc ppf =
  (* Originally written by @Drup in 24d87befcc505a9e3a1b081849b12560ce38028f. *)
  (* We use a custom implementation because the type of Location.raise_errorf
     changed in 4.03 *)
  let buf = Buffer.create 17 in
  let fmt = Format.formatter_of_buffer buf in
  Format.kfprintf
    (fun _ ->
      Format.pp_print_flush fmt ();
      Location.raise_errorf ~loc "%s@." (Buffer.contents buf))
    fmt
    ppf

(** Ast manipulation *)

let int loc = with_loc loc Ast_convenience.int

let float loc = with_loc loc Ast_convenience.float

let string loc = with_loc loc Ast_convenience.str

let add_constraints ~list lang e =
  let loc = {e.Parsetree.pexp_loc with loc_ghost = true} in
  let elt = make_lid ~loc lang "elt" in
  let wrap =
    if list then make_lid ~loc lang "list_wrap"
    else make_lid ~loc lang "wrap"
  in
  let ty =
    Typ.(constr ~loc wrap [ constr ~loc elt [any ~loc ()]])
  in
  Exp.constraint_ ~loc e ty

type 'a value =
  | Val of 'a
  | Antiquot of Parsetree.expression

let value x = Val x
let antiquot e = Antiquot e
let map_value f = function
  | Val x -> Val (f x)
  | Antiquot x -> Antiquot x

let list_gen cons append nil l =
  let f acc = function
    | Val x -> cons acc x
    | Antiquot e -> append acc e
  in
  (l |> List.rev |> List.fold_left f nil)

let list loc l =
  let nil = [%expr []][@metaloc loc] in
  let cons acc x = [%expr [%e x]::[%e acc]][@metaloc loc] in
  let append acc x = [%expr [%e x]@[%e acc]][@metaloc loc] in
  list_gen cons append nil @@ List.map (fun x -> Val x) l

let list_wrap_value lang loc =
  let (!!) = make ~loc lang in
  let nil =
    [%expr
      [%e !!"Xml.W.nil"]
      ()] [@metaloc loc]
  in
  let cons acc x =
    [%expr [%e !!"Xml.W.cons"]
        ([%e !!"Xml.W.return"] [%e x])
        [%e acc]
    ][@metaloc loc]
  in
  let append acc x =
    [%expr
      [%e !!"Xml.W.append"]
        [%e add_constraints ~list:true lang x] [%e acc]
    ][@metaloc loc]
  in
  list_gen cons append nil

let list_wrap lang loc l =
  list_wrap_value lang loc @@ List.map (fun x -> Val x) l

let wrap implementation loc e =
  [%expr
    [%e make ~loc implementation "Xml.W.return"]
    [%e e]] [@metaloc loc]

let wrap_value lang loc = function
  | Val x -> wrap lang loc x
  | Antiquot e -> add_constraints ~list:false lang e

let txt ~loc ~lang s =
  let txt = make ~loc lang "txt" in
  let arg = wrap lang loc @@ string loc s in
  Ast_helper.Exp.apply ~loc txt [Label.nolabel, arg]
