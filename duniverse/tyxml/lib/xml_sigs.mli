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

(** XML Signatures. *)

module type T = sig

  module W : Xml_wrap.T

  type 'a wrap = 'a W.t
  type 'a list_wrap = 'a W.tlist

  type uri
  val string_of_uri : (uri, string) W.ft
  val uri_of_string : (string, uri) W.ft

  type aname = string
  type event_handler
  type mouse_event_handler
  type keyboard_event_handler
  type touch_event_handler

  type attrib

  val float_attrib : aname -> float wrap -> attrib
  val int_attrib : aname -> int wrap -> attrib
  val string_attrib : aname -> string wrap -> attrib
  val space_sep_attrib : aname -> string list wrap -> attrib
  val comma_sep_attrib : aname -> string list wrap -> attrib
  val event_handler_attrib : aname -> event_handler -> attrib
  val mouse_event_handler_attrib : aname -> mouse_event_handler -> attrib
  val keyboard_event_handler_attrib : aname -> keyboard_event_handler -> attrib
  val touch_event_handler_attrib : aname -> touch_event_handler -> attrib
  val uri_attrib : aname -> uri wrap -> attrib
  val uris_attrib : aname -> uri list wrap -> attrib

  type elt
  type ename = string

  val empty : unit -> elt
  val comment : string -> elt

  val pcdata : string wrap -> elt
  val encodedpcdata : string wrap -> elt
  val entity : string -> elt

  val leaf : ?a:(attrib list) -> ename -> elt
  val node : ?a:(attrib list) -> ename -> elt list_wrap -> elt

  val cdata : string -> elt
  val cdata_script : string -> elt
  val cdata_style : string -> elt

end

module type NoWrap = T with module W = Xml_wrap.NoWrap
module type Iterable = sig

  include NoWrap

  type separator = Space | Comma

  val aname : attrib -> aname

  type acontent = private
    | AFloat of float
    | AInt of int
    | AStr of string
    | AStrL of separator * string list
  val acontent : attrib -> acontent

  type econtent = private
    | Empty
    | Comment of string
    | EncodedPCDATA of string
    | PCDATA of string
    | Entity of string
    | Leaf of ename * attrib list
    | Node of ename * attrib list * elt list
  val content : elt -> econtent

end

module type Info = sig
  val content_type: string
  val alternative_content_types: string list
  val version: string
  val standard: string
  val namespace: string
  val doctype: string
  val emptytags: string list
end

module type Output = sig
  type out
  type m
  val empty: m
  val concat: m -> m -> m
  val put: string -> m
  val make: m -> out
end

module type Typed_xml = sig

  module Xml : NoWrap
  module Info : Info

  type 'a elt
  type doc
  val toelt : 'a elt -> Xml.elt
  val doc_toelt : doc -> Xml.elt

end

module type Printer = sig

  type xml_elt
  type out

  val print_list: ?encode:(string -> string) -> xml_elt list -> out

end

module type Simple_printer = sig

  type xml_elt

  val print_list:
    output:(string -> unit) ->
    ?encode:(string -> string) ->
    xml_elt list -> unit

end

module type Typed_printer = sig

  type 'a elt
  type doc
  type out

  val print_list: ?encode:(string -> string) -> 'a elt list -> out
  val print: ?encode:(string -> string) -> ?advert:string-> doc -> out

end


module type Typed_simple_printer = sig

  type 'a elt
  type doc

  val print_list:
    output:(string -> unit) ->
    ?encode:(string -> string) ->
    'a elt list -> unit

  val print:
    output:(string -> unit) ->
    ?encode:(string -> string) -> ?advert:string->
    doc -> unit

end

module type Pp = sig

  type elt

(** [pp ()] is a {!Format} printer for untyped XML.

    It can be used in combination with ["%a"]. For example, to get a string:
    {[let s = Format.asprintf "%a" (pp ()) my_xml]}

    A custom encoding function can be provided with the [~encode] argument.
    Various implementations of [encode] are available in {!Xml_print}.
*)
  val pp:
    ?encode:(string -> string) -> ?indent:bool -> unit ->
    Format.formatter -> elt -> unit
end

module type Typed_pp = sig

  type 'a elt
  type doc

(** [pp_elt ()] is a {!Format} printer for individual elements.

    A custom encoding function can be provided with the [~encode] argument.
    Various implementations of [encode] are available in {!Xml_print}.
*)
  val pp_elt :
    ?encode:(string -> string) -> ?indent:bool -> unit ->
    Format.formatter -> 'a elt -> unit

(** [pp ()] is a {!Format} printer for complete documents.

    It can be used in combination with ["%a"]. For example, to get a string:
    {[let s = Format.asprintf "%a" (pp ()) my_document]}

    A custom encoding function can be provided with the [~encode] argument.
    Various implementations of [encode] are available in {!Xml_print}.
*)
  val pp:
    ?encode:(string -> string) -> ?indent:bool -> ?advert:string -> unit ->
    Format.formatter -> doc -> unit

end
