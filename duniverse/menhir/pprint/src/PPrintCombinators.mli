(**************************************************************************)
(*                                                                        *)
(*  PPrint                                                                *)
(*                                                                        *)
(*  François Pottier, Inria Paris                                         *)
(*  Nicolas Pouillard                                                     *)
(*                                                                        *)
(*  Copyright 2007-2019 Inria. All rights reserved. This file is          *)
(*  distributed under the terms of the GNU Library General Public         *)
(*  License, with an exception, as described in the file LICENSE.         *)
(**************************************************************************)

open PPrintEngine

(** A set of high-level combinators for building documents. *)

(** {1 Single characters} *)

(** The following constant documents consist of a single character. *)

val lparen: document
val rparen: document
val langle: document
val rangle: document
val lbrace: document
val rbrace: document
val lbracket: document
val rbracket: document
val squote: document
val dquote: document
val bquote: document
val semi: document
val colon: document
val comma: document
val space: document
val dot: document
val sharp: document
val slash: document
val backslash: document
val equals: document
val qmark: document
val tilde: document
val at: document
val percent: document
val dollar: document
val caret: document
val ampersand: document
val star: document
val plus: document
val minus: document
val underscore: document
val bang: document
val bar: document

(** {1 Delimiters} *)

(** [precede l x] is [l ^^ x]. *)
val precede: document -> document -> document

(** [terminate r x] is [x ^^ r]. *)
val terminate: document -> document -> document

(** [enclose l r x] is [l ^^ x ^^ r]. *)
val enclose: document -> document -> document -> document

(** The following combinators enclose a document within a pair of delimiters.
    They are partial applications of [enclose]. No whitespace or line break is
    introduced. *)

val squotes: document -> document
val dquotes: document -> document
val bquotes: document -> document
val braces: document -> document
val parens: document -> document
val angles: document -> document
val brackets: document -> document

(** {1 Repetition} *)

(** [twice doc] is the document obtained by concatenating two copies of
    the document [doc]. *)
val twice: document -> document

(** [repeat n doc] is the document obtained by concatenating [n] copies of
    the document [doc]. *)
val repeat: int -> document -> document

(** {1 Lists and options} *)

(** [concat docs] is the concatenation of the documents in the list [docs]. *)
val concat: document list -> document

(** [separate sep docs] is the concatenation of the documents in the list
    [docs]. The separator [sep] is inserted between every two adjacent
    documents. *)
val separate: document -> document list -> document

(** [concat_map f xs] is equivalent to [concat (List.map f xs)]. *)
val concat_map: ('a -> document) -> 'a list -> document

(** [separate_map sep f xs] is equivalent to [separate sep (List.map f xs)]. *)
val separate_map: document -> ('a -> document) -> 'a list -> document

(** [separate2 sep last_sep docs] is the concatenation of the documents in the
    list [docs]. The separator [sep] is inserted between every two adjacent
    documents, except between the last two documents, where the separator
    [last_sep] is used instead. *)
val separate2: document -> document -> document list -> document

(** [optional f None] is the empty document. [optional f (Some x)] is
    the document [f x]. *)
val optional: ('a -> document) -> 'a option -> document

(** {1 Text} *)

(** [lines s] is the list of documents obtained by splitting [s] at newline
    characters, and turning each line into a document via [substring]. This
    code is not UTF-8 aware. *)
val lines: string -> document list

(** [arbitrary_string s] is equivalent to [separate (break 1) (lines s)].
    It is analogous to [string s], but is valid even if the string [s]
    contains newline characters. *)
val arbitrary_string: string -> document

(** [words s] is the list of documents obtained by splitting [s] at whitespace
    characters, and turning each word into a document via [substring]. All
    whitespace is discarded. This code is not UTF-8 aware. *)
val words: string -> document list

(** [split ok s] splits the string [s] before and after every occurrence of a
    character that satisfies the predicate [ok]. The substrings thus obtained
    are turned into documents, and a list of documents is returned. No
    information is lost: the concatenation of the documents yields the
    original string. This code is not UTF-8 aware. *)
val split: (char -> bool) -> string -> document list

(** [flow sep docs] separates the documents in the list [docs] with the
    separator [sep] and arranges for a new line to begin whenever a document
    does not fit on the current line. This is useful for typesetting
    free-flowing, ragged-right text. A typical choice of [sep] is [break b],
    where [b] is the number of spaces that must be inserted between two
    consecutive words (when displayed on the same line). *)
val flow: document -> document list -> document

(** [flow_map sep f docs] is equivalent to [flow sep (List.map f docs)]. *)
val flow_map: document -> ('a -> document) -> 'a list -> document

(** [url s] is a possible way of displaying the URL [s]. A potential line
    break is inserted immediately before and immediately after every slash
    and dot character. *)
val url: string -> document

(** {1 Alignment and indentation} *)

(* [hang n doc] is analogous to [align], but additionally indents
   all lines, except the first one, by [n]. Thus, the text in the
   box forms a hanging indent. *)
val hang: int -> document -> document

(** [prefix n b left right] has the following flat layout:
{[
left right
]}
and the following non-flat layout:
{[
left
  right
]}
The parameter [n] controls the nesting of [right] (when not flat).
The parameter [b] controls the number of spaces between [left] and [right]
(when flat).
 *)
val prefix: int -> int -> document -> document -> document

(** [jump n b right] is equivalent to [prefix n b empty right]. *)
val jump: int -> int -> document -> document

(** [infix n b middle left right] has the following flat layout:
{[
left middle right
]}
and the following non-flat layout:
{[
left middle
  right
]}
The parameter [n] controls the nesting of [right] (when not flat).
The parameter [b] controls the number of spaces between [left] and [middle]
(always) and between [middle] and [right] (when flat).
*)
val infix: int -> int -> document -> document -> document -> document

(** [surround n b opening contents closing] has the following flat layout:
{[
opening contents closing
]}
and the following non-flat layout:
{[
opening
  contents
closing
]}
The parameter [n] controls the nesting of [contents] (when not flat).
The parameter [b] controls the number of spaces between [opening] and [contents]
and between [contents] and [closing] (when flat).
*)
val surround: int -> int -> document -> document -> document -> document

(** [soft_surround] is analogous to [surround], but involves more than one
    group, so it offers possibilities other than the completely flat layout
    (where [opening], [contents], and [closing] appear on a single line) and
    the completely developed layout (where [opening], [contents], and
    [closing] appear on separate lines). It tries to place the beginning of
    [contents] on the same line as [opening], and to place [closing] on the
    same line as the end of [contents], if possible.
*)
val soft_surround: int -> int -> document -> document -> document -> document

(** [surround_separate n b void opening sep closing docs] is equivalent to
    [surround n b opening (separate sep docs) closing], except when the
    list [docs] is empty, in which case it reduces to [void]. *)
val surround_separate: int -> int -> document -> document -> document -> document -> document list -> document

(** [surround_separate_map n b void opening sep closing f xs] is equivalent to
    [surround_separate n b void opening sep closing (List.map f xs)]. *)
val surround_separate_map: int -> int -> document -> document -> document -> document -> ('a -> document) -> 'a list -> document

(** {1 Short-hands} *)

(** [!^s] is a short-hand for [string s]. *)
val ( !^ ) : string -> document

(** [x ^/^ y] separates [x] and [y] with a breakable space.
    It is a short-hand for [x ^^ break 1 ^^ y]. *)
val ( ^/^ ) : document -> document -> document

(** [x ^//^ y] is a short-hand for [prefix 2 1 x y]. *)
val ( ^//^ ) : document -> document -> document
