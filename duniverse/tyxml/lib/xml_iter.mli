(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2004 Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
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

(** Basic iterators over XML tree (functorial interface). *)

module Make(Xml : Xml_sigs.Iterable) : sig

  open Xml

  val amap : (ename -> attrib list -> attrib list) -> elt -> elt
  (** Recursively edit attributes for the element and all its children. *)

  val amap1 : (ename -> attrib list -> attrib list) -> elt -> elt
  (** Edit attributes only for one element. *)

  (** The following can safely be exported by higher level libraries,
      because removing an attribute from a element is always legal. *)

  val rm_attrib : (aname -> bool) -> attrib list -> attrib list
  val rm_attrib_from_list : (aname -> bool) -> (string -> bool) -> attrib list -> attrib list

  val map_int_attrib :
    (aname -> bool) -> (int -> int) -> attrib list -> attrib list
  val map_float_attrib :
    (aname -> bool) -> (float -> float) -> attrib list -> attrib list
  val map_string_attrib :
    (aname -> bool) -> (string -> string) -> attrib list -> attrib list
  val map_string_attrib_in_list :
    (aname -> bool) -> (string -> string) -> attrib list -> attrib list

  (** Exporting the following by higher level libraries would drive
      a hole through a type system, because they allow to add {e any}
      attribute to {e any} element. *)

  val add_int_attrib : aname -> int -> attrib list -> attrib list
  val add_float_attrib : aname -> float -> attrib list -> attrib list
  val add_string_attrib : aname -> string -> attrib list -> attrib list
  val add_comma_sep_attrib : aname -> string -> attrib list -> attrib list
  val add_space_sep_attrib : aname -> string -> attrib list -> attrib list

  val fold : (unit -> 'a) -> (string -> 'a) -> (string -> 'a) -> (string -> 'a) ->
    (string -> 'a) -> (ename -> attrib list -> 'a) ->
    (ename -> attrib list -> 'a list -> 'a) ->
    elt -> 'a

  val all_entities : elt -> string list

  val translate :
    (ename -> attrib list -> elt) ->
    (ename -> attrib list -> elt list -> elt) ->
    ('state -> ename -> attrib list -> elt list) ->
    ('state -> ename -> attrib list -> elt list -> elt list) ->
    (ename -> attrib list -> 'state -> 'state) -> 'state -> elt -> elt

end
