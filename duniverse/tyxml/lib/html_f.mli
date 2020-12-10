(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
 * Copyright (C) 2007 by Vincent Balat, Gabriel Kerneis
 * Copyright (C) 2010 by Cecile Herbelin
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

(** Typesafe constructors for HTML documents (Functorial interface)

    {% See <<a_manual chapter="functors"|the manual of the functorial interface>>. %}
*)

(** Create a new implementation of [HTML], using the given underlying [Xml]
    and [Svg] implementation. Will output a module of type {!Html_sigs.T} with
    the various type equalities.

    If your [Xml] implementation uses a special function wrapping, use
    {!Make_with_wrapped_functions}.
*)
module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
  : Html_sigs.Make(Xml)(Svg).T
    with type +'a elt = Xml.elt
     and type +'a attrib = Xml.attrib

(** The standard set of wrapped functions, when [W.ft] is the regular function. *)
module Wrapped_functions
    (Xml: Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
  : Html_sigs.Wrapped_functions with module Xml = Xml

(** Similar to {!Make} but with a custom set of wrapped functions. *)
module Make_with_wrapped_functions
    (Xml : Xml_sigs.T)
    (C : Html_sigs.Wrapped_functions with module Xml = Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
  : Html_sigs.Make(Xml)(Svg).T
    with type +'a elt = Xml.elt
     and type +'a attrib = Xml.attrib
