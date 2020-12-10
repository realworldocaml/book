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

(** Element parsing. *)

val parse :
  loc:Location.t ->
  parent_lang:Common.lang ->
  name:Common.name ->
  attributes:(Common.name * string Common.value) list ->
  Parsetree.expression Common.value list ->
  Parsetree.expression
(** [parse ~loc ~parent_lang ~name ~attributes children]
    evaluates to a parse tree for applying the TyXML function corresponding
    to element [name] to suitable arguments representing [attributes] and
    [children].
*)

val comment :
  loc:Location.t ->
  lang:Common.lang ->
  string ->
  Parsetree.expression
(** [comment ~loc ~ns s] evaluates to a parse tree that represents an XML comment. *)

val find_assembler :
  Common.name -> 
  (string * Element_content.assembler) option
