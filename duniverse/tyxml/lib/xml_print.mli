(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2008 Vincent Balat, Mauricio Fernandez
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

(** Printing utilities.

    This module contains various encoding functions that can be used
    with {!Tyxml.Html.pp} and {!Tyxml.Svg.pp}.

    It also contains functors to create printers for your own XML data structure.

*)

(** {2 Encoding functions} *)

val encode_unsafe_char : string -> string
(** The encoder maps strings to HTML and {e must} encode the unsafe characters
    ['<'], ['>'], ['"'], ['&'] and the control characters 0-8, 11-12, 14-31, 127
    to HTML entities.  [encode_unsafe_char] is the default for [?encode] in [output]
    and [pretty_print] below.  Other implementations are provided by the module
    [Netencoding] in the
    {{:http://www.ocaml-programming.de/programming/ocamlnet.html}OcamlNet}
    library, e.g.:
    {[
let encode = Netencoding.Html.encode ~in_enc:`Enc_iso88591 ~out_enc:`Enc_usascii ()
    ]}
    Where national characters are replaced by HTML entities.
    The user is of course free to write her own implementation.
    @see <http://www.ocaml-programming.de/programming/ocamlnet.html> OcamlNet *)

val encode_unsafe_char_and_at : string -> string
(** In addition, encode ["@"] as ["&#64;"] in the hope that this will fool
    simple minded email address harvesters. *)

(** Utf8 normalizer and encoder for HTML.

Given a [pp] function produced by one of the functors in {!Xml_print}, this modules is used as following:
  {[
    let encode x = fst (Xml_print.Utf8.normalize_html x) in
    Format.printf "%a" (Html.pp ~encode ()) document
  ]} *)
module Utf8 : sig

  type utf8 = string

  (** [normalize str] take a possibly invalid utf-8 string
      and return a valid utf-8 string
      where invalid bytes have been replaced by
      the replacement character [U+FFFD].
      The returned boolean is true if invalid bytes were found *)
  val normalize : string -> utf8 * bool

  (** Same as [normalize] plus some extra work :
      It encode '<' , '>' , '"' , '&' characters with
      corresponding entities and replaced invalid html
      character by [U+FFFD] *)
  val normalize_html : string -> utf8 * bool

end

(** {2 Utilities} *)

val compose_decl : ?version:string -> ?encoding:string -> unit -> string
(** [encoding] is the name of the character encoding, e.g. ["US-ASCII"] or ["UTF-8"] *)

val compose_doctype : string -> string list -> string

val string_of_number : float -> string
(** Convert a float to a string using a compact representation compatible with the Javascript norm. *)

val pp_number : Format.formatter -> float -> unit
(** See {!string_of_number}. *)

(** {2 Formatter functors} *)

(** Printers for typed XML modules such as the one produced by {!Svg_f} and {!Html_f}. *)
module Make_typed_fmt
    (Xml : Xml_sigs.Iterable)
    (Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml)
  : Xml_sigs.Typed_pp
    with type 'a elt := 'a Typed_xml.elt
     and type doc := Typed_xml.doc

(** List of tags that can be printed as empty tags: [<foo />]. *)
module type TagList = sig val emptytags : string list end

(** Printers for raw XML modules. *)
module Make_fmt
    (Xml : Xml_sigs.Iterable)
    (I : TagList)
  : Xml_sigs.Pp with type elt := Xml.elt

(** {2 Deprecated functors}

    Use {!Make_fmt} and {!Make_typed_fmt} instead.
*)

module Make
    (Xml : Xml_sigs.Iterable)
    (I : TagList)
    (O : Xml_sigs.Output)
  : Xml_sigs.Printer with type out := O.out and type xml_elt := Xml.elt
    [@@ocaml.deprecated "Use Xml_print.Make_fmt instead."]

module Make_typed
    (Xml : Xml_sigs.Iterable)
    (Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml)
    (O : Xml_sigs.Output)
  : Xml_sigs.Typed_printer with type out := O.out
                            and type 'a elt := 'a Typed_xml.elt
                            and type doc := Typed_xml.doc
    [@@ocaml.deprecated "Use Xml_print.Make_typed_fmt instead."]

module Make_simple
    (Xml : Xml_sigs.Iterable)
    (I : TagList)
  : Xml_sigs.Simple_printer with type xml_elt := Xml.elt
    [@@ocaml.deprecated "Use Xml_print.Make_fmt instead."]

module Make_typed_simple
    (Xml : Xml_sigs.Iterable)
    (Typed_xml : Xml_sigs.Typed_xml with module Xml := Xml)
  : Xml_sigs.Typed_simple_printer with type 'a elt := 'a Typed_xml.elt
                                   and type doc := Typed_xml.doc
    [@@ocaml.deprecated "Use Xml_print.Make_typed_fmt instead."]
