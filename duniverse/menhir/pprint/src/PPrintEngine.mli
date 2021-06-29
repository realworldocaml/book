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

(** A pretty-printing engine and a set of basic document combinators. *)

(** {1 Building documents} *)

(** Documents must be built in memory before they are rendered. This may seem
    costly, but it is a simple approach, and works well. *)

(** The following operations form a set of basic (low-level) combinators for
    building documents. On top of these combinators, higher-level combinators
    can be defined: see {!PPrintCombinators}. *)

(** This is the abstract type of documents. *)
type document

(** The following basic (low-level) combinators allow constructing documents. *)

(** [empty] is the empty document. *)
val empty: document

(** [char c] is a document that consists of the single character [c]. This
    character must not be a newline. *)
val char: char -> document

(** [string s] is a document that consists of the string [s]. This string must
    not contain a newline. *)
val string: string -> document

(** [substring s ofs len] is a document that consists of the portion of the
    string [s] delimited by the offset [ofs] and the length [len]. This
    portion must not contain a newline. *)
val substring: string -> int -> int -> document

(** [fancystring s apparent_length] is a document that consists of the string
    [s]. This string must not contain a newline. The string may contain fancy
    characters: color escape characters, UTF-8 or multi-byte characters,
    etc. Thus, its apparent length (which measures how many columns the text
    will take up on screen) differs from its length in bytes. *)
val fancystring: string -> int -> document

(** [fancysubstring s ofs len apparent_length] is a document that consists of
    the portion of the string [s] delimited by the offset [ofs] and the length
    [len]. This portion must contain a newline. The string may contain fancy
    characters. *)
val fancysubstring : string -> int -> int -> int -> document

(** [utf8string s] is a document that consists of the UTF-8-encoded string [s].
    This string must not contain a newline. *)
val utf8string: string -> document

(** [utf8format format <args>...] is equivalent to
    [utf8string (Printf.sprintf format <args>...)]. *)
val utf8format: ('a, unit, string, document) format4 -> 'a

(** [hardline] is a forced newline document. This document forces all enclosing
    groups to be printed in non-flattening mode. In other words, any enclosing
    groups are dissolved. *)
val hardline: document

(** [blank n] is a document that consists of [n] blank characters. *)
val blank: int -> document

(** [break n] is a document which consists of either [n] blank characters,
    when forced to display on a single line, or a single newline character,
    otherwise. Note that there is no choice at this point: choices are encoded
    by the [group] combinator. *)
val break: int -> document

(** [doc1 ^^ doc2] is the concatenation of the documents [doc1] and [doc2]. *)
val (^^): document -> document -> document

(** [nest j doc] is the document [doc], in which the indentation level has
    been increased by [j], that is, in which [j] blanks have been inserted
    after every newline character. Read this again: indentation is inserted
    after every newline character. No indentation is inserted at the beginning
    of the document. *)
val nest: int -> document -> document

(** [group doc] encodes a choice. If possible, then the entire document [group
    doc] is rendered on a single line. Otherwise, the group is dissolved, and
    [doc] is rendered. There might be further groups within [doc], whose
    presence will lead to further choices being explored. *)
val group: document -> document

(** [ifflat doc1 doc2] is rendered as [doc1] if part of a group that can be
    successfully flattened, and is rendered as [doc2] otherwise. Use this
    operation with caution. Because the pretty-printer is free to choose
    between [doc1] and [doc2], these documents should be semantically
    equivalent. *)
val ifflat: document -> document -> document

(** [align doc] is the document [doc], in which the indentation level has been
    set to the current column. Thus, [doc] is rendered within a box whose
    upper left corner is the current position. *)
val align: document -> document

(** A point is a pair of a line number and a column number. *)
type point =
  int * int

(** A range is a pair of points. *)
type range =
  point * point

(** [range hook doc] is printed exactly like the document [doc], but allows the
    caller to register a hook that is applied, when the document is printed, to
    the range occupied by this document in the output text. This offers a way of
    mapping positions in the output text back to (sub)documents. *)
val range: (range -> unit) -> document -> document

(** {1 Rendering documents} *)

(** This renderer sends its output into an output channel. *)
module ToChannel : PPrintRenderer.RENDERER
  with type channel = out_channel
   and type document = document

(** This renderer sends its output into a memory buffer. *)
module ToBuffer : PPrintRenderer.RENDERER
  with type channel = Buffer.t
   and type document = document

(** This renderer sends its output into a formatter channel. *)
module ToFormatter : PPrintRenderer.RENDERER
  with type channel = Format.formatter
   and type document = document

(** {1 Defining custom documents} *)

(** A width requirement is expressed as an integer, where the value [max_int]
    is reserved and represents infinity. *)

type requirement = int
val infinity : requirement

(** An output channel is represented abstractly as an object equipped with
    methods for displaying one character and for displaying a substring. *)

class type output = object

  (** [char c] sends the character [c] to the output channel. *)
  method char: char -> unit

  (** [substring s ofs len] sends the substring of [s] delimited by the
      offset [ofs] and the length [len] to the output channel. *)
  method substring: string -> int (* offset *) -> int (* length *) -> unit

end

(** The rendering engine maintains the following internal state. Its structure
    is subject to change in future versions of the library. Nevertheless, it is
    exposed to the user who wishes to define custom documents. *)

type state = {

    width: int;
    (** The line width. This parameter is fixed throughout the execution of
        the renderer. *)

    ribbon: int;
    (** The ribbon width. This parameter is fixed throughout the execution of
        the renderer. *)

    mutable last_indent: int;
    (** The number of blanks that were printed at the beginning of the current
        line. This field is updated (only) when a hardline is emitted. It is
        used (only) to determine whether the ribbon width constraint is
        respected. *)

    mutable line: int;
    (** The current line. This field is updated (only) when a hardline is
        emitted. It is not used by the pretty-printing engine itself. *)

    mutable column: int;
    (** The current column. This field must be updated whenever something is
        sent to the output channel. It is used (only) to determine whether the
        width constraint is respected. *)

  }

(** A custom document is defined by implementing the following methods. *)

class type custom = object

  (** A custom document must publish the width (i.e., the number of columns)
      that it would like to occupy if it is printed on a single line (that is,
      in flattening mode). The special value [infinity] means that this
      document cannot be printed on a single line; this value causes any
      groups that contain this document to be dissolved. This method should
      in principle work in constant time. *)
  method requirement: requirement

  (** The method [pretty] is used by the main rendering algorithm. It has
      access to the output channel and to the algorithm's internal state, as
      described above. In addition, it receives the current indentation level
      and the current flattening mode (on or off). If flattening mode is on,
      then the document must be printed on a single line, in a manner that is
      consistent with the requirement that was published ahead of time. If
      flattening mode is off, then there is no such obligation. The state must
      be updated in a manner that is consistent with what is sent to the
      output channel. *)
  method pretty: output -> state -> int -> bool -> unit

  (** The method [compact] is used by the compact rendering algorithm. It has
      access to the output channel only. *)
  method compact: output -> unit

end

(** The function [custom] constructs a custom document. In other words, it
    converts an object of type [custom] to a document. *)
val custom: custom -> document

(** The key functions of the library are exposed, in the hope that they may be
    useful to authors of custom (leaf and non-leaf) documents. In the case of
    a leaf document, they can help perform certain basic functions; for
    instance, applying the function [pretty] to the document [hardline] is a
    simple way of printing a hardline, while respecting the indentation
    parameters and updating the state in a correct manner. Similarly, applying
    [pretty] to the document [blank n] is a simple way of printing [n] spaces.
    In the case of a non-leaf document (i.e., one which contains
    sub-documents), these functions are essential: they allow computing the
    width requirement of a sub-document and displaying a sub-document. *)

(** [requirement doc] computes the width requirement of the document [doc].
    It works in constant time. *)
val requirement: document -> requirement

(** [pretty output state indent flatten doc] prints the document [doc]. See
    the documentation of the method [pretty]. *)
val pretty: output -> state -> int -> bool -> document -> unit

(** [compact output doc] prints the document [doc]. See the documentation of
    the method [compact]. *)
val compact: output -> document -> unit
