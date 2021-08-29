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

(** Signature of [Html_sigs_reflected] and [Svg_sigs_reflected] (but not
    [Html_types_reflected]). *)

module type S =
sig
  val attribute_parsers :
    (string * (Common.lang -> Attribute_value.vparser)) list
  (** Pairs [tyxml_attribute_name, wrapped_attribute_value_parser]. *)

  val renamed_attributes : (string * string * string list) list
  (** Triples [tyxml_attribute_name, markup_name, in_element_types]. *)

  val labeled_attributes :
    (string * string * (Common.lang -> Attribute_value.vparser)) list
  (** Triples [tyxml_element_name, label, wrapped_attribute_value_parser]. *)

  val element_assemblers : (string * Element_content.assembler) list
  (** Pairs [tyxml_element_name, child_argument_assembler]. *)

  val renamed_elements : (string * string) list
  (** Pairs [markup_element_name, tyxml_name]. *)
end
