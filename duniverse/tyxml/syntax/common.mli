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

val find : ('a -> bool) -> 'a list -> 'a option
(** Similar to [List.find], but evaluates to an option instead of raising
    [Not_found]. *)

(** Markup language *)

type lang = Html | Svg
val lang : lang -> string
val implementation : lang -> string
val set_implementation : lang -> string -> unit

type name = lang * string

val make_lid :
  loc:Location.t -> lang -> string -> Longident.t Location.loc
val make :
  loc:Location.t -> lang -> string -> expression

(** Expression helpers. *)

val int : Location.t -> int -> expression
val float : Location.t -> float -> expression
val string : Location.t -> string -> expression
val list : Location.t -> expression list -> expression
val list_wrap : lang -> Location.t -> expression list -> expression

val wrap :
  lang -> Location.t -> expression -> expression
(** [wrap implementation loc e] creates a parse tree for
    [implementation.Xml.W.return e]. *)

type 'a value =
  | Val of 'a
  | Antiquot of expression

val map_value : ('a -> 'b) -> 'a value -> 'b value
val value : 'a -> 'a value
val antiquot : expression -> _ value

val wrap_value :
  lang -> Location.t -> expression value -> expression
val list_wrap_value :
  lang -> Location.t -> expression value list -> expression


val error : Location.t -> ('b, Format.formatter, unit, 'a) format4 -> 'b

val txt :
  loc:Location.t -> lang:lang -> string -> expression
