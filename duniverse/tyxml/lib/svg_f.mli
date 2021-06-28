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

(** Typesafe constructors for SVG documents (Functorial interface)

    This module is experimental, it may lack of some attributes,
    and the interface is very low level and do not take deeply into account
    the needs of SVG elements.

    {% See <<a_manual chapter="functors"|the manual of the functorial interface>>. %}
*)

(*
open Svg_types
module Unit : sig

  open Unit

  val rel: float -> 'a quantity
      (** Do not specify the unit *)

  val deg : float -> angle
  val grad : float -> angle
  val rad : float -> angle

  val s : float -> time
  val ms : float -> time

  val em : float -> length
  val ex : float -> length
  val px : float -> length
  val in_ : float -> length
  val cm : float -> length
  val mm : float -> length
  val pt : float -> length
  val pc : float -> length

  val hz : float -> frequency
  val khz : float -> frequency

  val string_of_angle : angle -> string
  val string_of_time : time -> string
  val string_of_length : length -> string
  val string_of_freq : frequency -> string

end

open Unit

val string_of_number : number -> string
val string_of_number_optional_number : number_optional_number -> string
val string_of_percentage : percentage -> string
val string_of_strings : strings -> string
val string_of_spacestrings : spacestrings -> string
val string_of_commastrings : commastrings -> string
val string_of_fourfloats : fourfloats -> string
val string_of_numbers : numbers -> string
val string_of_numbers_semicolon : numbers_semicolon -> string
val string_of_lengths : lengths -> string
val string_of_coord : coord -> string
val string_of_coords : coords -> string
val string_of_transform : transform -> string
val string_of_transforms : transforms -> string
*)

(** Create a new implementation of [Svg], using the given underlying [Xml]
    implementation. Will output a module of type {!Svg_sigs.T} with
    the various type equalities.

    If your [Xml] implementation uses a special function wrapping, use
    {!Make_with_wrapped_functions}.
*)
module Make(Xml : Xml_sigs.T with type ('a, 'b) W.ft = ('a -> 'b))
  : Svg_sigs.Make(Xml).T
    with type +'a elt = Xml.elt
     and type +'a attrib = Xml.attrib

(** The standard set of wrapped functions, when [W.ft] is the regular function. *)
module Wrapped_functions
    (Xml: Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
  : Svg_sigs.Wrapped_functions with module Xml = Xml

(** Similar to {!Make} but with a custom set of wrapped functions. *)
module Make_with_wrapped_functions
    (Xml : Xml_sigs.T)
    (C : Svg_sigs.Wrapped_functions with module Xml = Xml)
  : Svg_sigs.Make(Xml).T
    with type +'a elt = Xml.elt
     and type +'a attrib = Xml.attrib
