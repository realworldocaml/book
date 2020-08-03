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

(** Attribute value parsers and parser combinators. *)


type 'a gparser =
  ?separated_by:string -> ?default:string -> Location.t -> string -> 'a ->
    Parsetree.expression option
type parser = string gparser
type vparser = string Common.value gparser
(** Attribute value parsers are assigned to each attribute depending on the type
    of the attribute's argument, though some attributes have special parsers
    based on their name, or on a [[@@reflect]] annotation. A parser is a
    function [p] such that [p loc name value] either:

    - converts the string [value] into [Some] of a parse tree representing that
      value, for use with attributes that take an argument, or
    - evaluates to [None], for use with attributes that take no argument (for
      instance, [a_selected]).

    For example, [int loc name "3"] converts ["3"] into the parse tree
    [{pexp_desc = Pexp_constant (Const_int 3); ...}].

    The parse tree is assigned the location [loc]. This {e should} be the
    location of the start of the value string, but, presently, the location of
    the element containing the value string is used.

    [name] is the name of the attribute. This is used only for error reporting.

    [~separated_by] and [~default] are used internally by combinators to modify
    the error message (for example, to make nouns plural if an error occurs in a
    list). *)



(** {2 Combinators} *)

val option : string -> parser -> parser
(** [option none parser _ _ s] behaves as follows:

    - if [s] = [none], evaluates to a parse tree for [None].
    - otherwise, if [parser _ _ s] evaluates to a parse tree for [e], [option]
      evaluates to a parse tree for [Some e]. *)

val spaces : parser -> parser
(** [spaces parser _ _ s] splits [s] on spaces, then applies [parser] to each
    component. The resulting parse trees for [e, e', ...] are combined into a
    parse tree of [[e; e'; ...]]. *)

val commas : parser -> parser
(** Similar to [spaces], but splits on commas. *)

val semicolons : parser -> parser
(** Similar to [spaces], but splits on semicolons. *)

val spaces_or_commas : parser -> parser
(** Similar to [spaces], but splits on both spaces and commas. *)

(** {3 Top combinators}
    Exported parsers should always use one of those combinators last. *)

val wrap : parser -> Common.lang -> vparser
(** [wrap parser module_ _ _ s] applies [parser _ _ s] to get a parse tree for
    [e], then evaluates to the parse tree for [module_.Xml.W.return e]. *)

val nowrap : parser -> Common.lang -> vparser
(** [nowrap parser _ _ _ s] evaluates to [parser _ _ s]. The purpose of this
    combinator is to provide a signature similar to [wrap] in situations where
    wrapping is not wanted. *)



(** {2 Numeric} *)

val char : parser
(** [char _ _ s], where [s] is a string containing a single UTF-8 character [c],
    produces a parse tree for [c] of type [char]. Note that this means the range
    is restricted to the first 256 code points. *)

val bool : parser
(** [bool _ _ s] produces a parse tree for the boolean [true]
    if [s = "true"] or [""] and [false] if [s = "false"]. *)

val onoff : parser
(** [onoff _ _ s] produces a parse tree for the boolean [true]
    if [s = "on"] or [""] and [false] if [s = "off"]. *)

val unit : parser
(** [unit _ name s] produces a parse tree for [()]. It fails if [name <> s]. *)

val int : parser
(** [int _ _ s] produces a parse tree for [int_of_string s]. *)

val float : parser
(** [float _ _ s] produces a parse tree for [float_of_string s]. This is a
    slight superset of HTML and SVG decimal fraction number syntax. *)

val points : parser
(** Similar to [spaces_or_commas float], but pairs consecutive numbers. *)

val number_pair : parser
(** [number_pair _ _ s] produces a parse tree for

    - [n, None] if [s] = [(string_of_float n)], or
    - [m, Some n'] if [s] is a space- or comma-separated list of representations
      of two floats. *)

val fourfloats : parser
(** Acts as [spaces_or_commas float], but expects the list to have exactly four
    elements. *)

val icon_size : parser
(** [icon_size _ _ s] produces a parse tree for the pair [(width, height)] when
    [s] has the form [(string_of_int width) ^ x ^ (string_of_int height)] and
    [x] is either ["x"] or ["X"]. *)



(** {2 Dimensional} *)

val svg_length : parser
(** [svg_length _ _ s] produces a parse tree for a value of type
    [Svg_types.Unit.(length quantity)]. [s] is expected to have form
    [(string_of_float n) ^ unit] for some number [n] and a valid SVG length
    unit, or no unit. *)

val angle : parser
(** Similar to [svg_length], but for SVG angles. *)

val offset : parser
(** [offset _ _ s] produces a parse tree for

    - [`Number n] if [s] = [string_of_float n], or
    - [`Percentage n] if [s] has form [(string_of_float n) ^ "%"]. *)

val transform : parser
(** Parses an SVG transform attribute value. 

    @see <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform>
*)



(* {2 String-like} *)

val string : parser
(** [string _ _ s] produces a parse tree for [s]. This is intended for ordinary
    attributes containing text that requires no further parsing. *)

val variant : parser
(** [variant _ _ s] produces a parse tree for the variand
    [Tyxml_name.polyvar s]. This is intended for attributes whose argument type
    is a polymorphic variant, none of whose constructors take arguments. *)

val total_variant : (string * string list) -> parser
(** [total_variant] is used for parsing arguments whose type is a variant with
    the following pattern:

{[
| `A | `B | `C | `EverythingElse of string
]}

    It behaves like [variant] for strings matching the no-argument constructors.
    Any other string [s] is mapped to the parse trees for
    [`EverythingElse s]. *)

val variant_or_empty : string -> parser
(** [variant_or_empty empty] is used for parsing arguments whose type
   is a variant, possibly the empty string. It behaves like [variant]
   for every string but the empty one, which will be parsed as if it
   was the [empty] parameter. *)


(* {2 Miscellaneous} *)

val presence : parser
(** [presence _ _ _] evaluates to [None]. It is used as a "parser" for
    attributes that do not take arguments. *)

val paint : parser
(* Parses SVG paint values. See
   {:{https://www.w3.org/TR/SVG/painting.html#SpecifyingPaint} Specifying
   paint}. *)

val srcset_element : parser
(** Used for [a_srcset]. *)

val number_or_datetime : parser
(** Used for [a_input_min] and [a_input_max]. *)



(* {2 Special-cased}

    These parsers are named after the attribute for which they are used. *)

val sandbox : parser
val in_ : parser
val in2 : parser
val xmlns : parser
