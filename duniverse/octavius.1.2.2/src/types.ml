(*
 * Copyright (c) 2015 Leo White <leo@lpw25.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Types for the information collected in comments. *)

(** The differents kinds of style. *)
type style_kind =
  | SK_bold
  | SK_italic
  | SK_emphasize
  | SK_center
  | SK_left
  | SK_right
  | SK_superscript
  | SK_subscript
  | SK_custom of string

(** The differents kinds of element references. *)
type ref_kind =
    RK_element
  | RK_module
  | RK_module_type
  | RK_class
  | RK_class_type
  | RK_value
  | RK_type
  | RK_exception
  | RK_attribute
  | RK_method
  | RK_section
  | RK_recfield
  | RK_const
  | RK_link
  | RK_custom of string

(* The different kinds of special reference *)
type special_ref_kind =
    SRK_module_list of string list
  | SRK_index_list

and text_element =
  | Raw of string (** Raw text. *)
  | Code of string (** The string is source code. *)
  | PreCode of string (** The string is pre-formatted source code. *)
  | Verbatim of string (** String 'as is'. *)
  | Style of style_kind * text (** Text tagged with a style. *)
  | List of text list (** A list. *)
  | Enum of text list (** An enumerated list. *)
  | Newline   (** To force a line break. *)
  | Title of int * string option * text
              (** Style number, optional label, and text. *)
  | Ref of ref_kind * string * text option
    (** A reference to an element. Complete name and kind. An optional
        text can be given to display this text instead of the element name.*)
  | Special_ref of special_ref_kind (** Special kinds of reference *)
  | Target of string option * string (** (target, code) : to specify code for a specific target format *)

(** [text] is a list of text_elements. The order matters. *)
and text = text_element list

(** The different forms of references in \@see tags. *)
type see_ref =
    See_url of string
  | See_file of string
  | See_doc of string

(** Tags *)
type tag =
    Author of string (** \@author tag *)
  | Version of string (** \@version tag *)
  | See of see_ref * text (** \@see tag *)
  | Since of string (** \@since tag *)
  | Before of string * text (** \@before tag *)
  | Deprecated of text (** \@deprecated tag *)
  | Param of string * text (** \@param tag *)
  | Raised_exception of string * text (** \@raise tag *)
  | Return_value of text (** \@return tag *)
  | Inline (** \@inline tag *)
  | Custom of string * text (** custom tag *)
  | Canonical of string (** \@canonical tag *)

(** A special comment *)
type t = text * tag list
