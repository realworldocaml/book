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

module M = Svg_f.Make(Tyxml_xml)
module P = Xml_print.Make_typed_fmt(Tyxml_xml)(M)
module Make_printer = Xml_print.Make_typed(Tyxml_xml)(M)
[@@ocaml.warning "-3"]


include M
include P

let _pp = pp ()
let _pp_elt = pp_elt ()
