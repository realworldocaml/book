(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2016 Anton Bachin, Gabriel Radanne
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

(** TyXML ppx library.

    This is the documentation for the internal ppx library.
    {% Documentation for the ppx itself is available
    <<a_manual chapter="ppx"|here>>. %}
*)

type lang = Html | Svg

val markup_to_expr :
  lang ->
  Location.t -> Parsetree.expression list -> Parsetree.expression
(** Given the payload of a [%html ...] or [%svg ...] expression,
    converts it to a TyXML expression representing the markup
    contained therein. *)

val mapper : _ -> _ -> Ast_mapper.mapper
