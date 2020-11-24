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

(** Attribute parsing. *)

val parse :
  Location.t -> Common.name -> (Common.name * string Common.value) list ->
    (Common.Label.t * Parsetree.expression) list
(** [parse loc element_name attributes] evaluates to a list of labeled parse
    trees, each representing an attribute argument to the element function for
    [element_name]. For example, if called on the HTML element
    [<img src='foo' alt='bar' id='some-image'>], this function will evaluate to
    parse trees for the arguments:

{[
~src:(return "foo") ~alt:(return "bar") ~a:[id (return "some-image")]
]}

    This satisfies the attribute arguments in the signature of
    [Html_sigs.T.img]. *)
