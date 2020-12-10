(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2011 Pierre Chambart, Grégoire Henry
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

(** Typesafe constructors and printers for Svg documents.

    @see <http://www.w3.org/TR/SVG> W3C Recommendation *)

(** Concrete implementation of Svg typesafe constructors.
    See {!module-type:Svg_sigs.T}.
*)
include Svg_sigs.Make(Tyxml_xml).T
  with module Xml.W = Xml_wrap.NoWrap

(** {2 Printers} *)

(** [pp ()] is a {!Format} printer for Svg documents.

    It can be used in combination with ["%a"]. For example, to get a string:
    {[let s = Format.asprintf "%a" (Tyxml.Svg.pp ()) my_svg]}
*)
val pp:
  ?encode:(string -> string) -> ?indent:bool -> ?advert:string -> unit ->
  Format.formatter -> doc -> unit

(** [pp_elt ()] is a {!Format} printer for Svg elements. *)
val pp_elt :
  ?encode:(string -> string) -> ?indent:bool -> unit ->
  Format.formatter -> 'a elt -> unit

(** Parametrized stream printer for Svg documents.
    @deprecated Use {!pp} instead.
*)
module Make_printer(O : Xml_sigs.Output) :
  Xml_sigs.Typed_printer with type out := O.out
                          and type 'a elt := 'a elt
                          and type doc := doc
     [@@ocaml.deprecated "Use Svg.pp instead."]

(**/*)

(** Toplevel printers *)

val _pp : Format.formatter -> doc -> unit
[@@ocaml.toplevel_printer]

val _pp_elt : Format.formatter -> _ elt -> unit
[@@ocaml.toplevel_printer]
