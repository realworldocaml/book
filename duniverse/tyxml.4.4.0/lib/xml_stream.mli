(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2018 Gabriel Radanne
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

(** Streaming IO to/from XML trees *)

type name = string * string

(** {2 Input} *)

type signal = [
  | `Comment of string
  | `End_element
  | `Start_element of name * (name * string) list
  | `Text of string list
]

exception Malformed_stream
  
module Import (Xml : Xml_sigs.T) : sig
  val of_seq : signal Seq.t -> Xml.elt Xml.list_wrap
end
