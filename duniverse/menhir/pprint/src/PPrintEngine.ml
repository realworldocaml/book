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

(** A point is a pair of a line number and a column number. *)
type point =
  int * int

(** A range is a pair of points. *)
type range =
  point * point

(* ------------------------------------------------------------------------- *)

(* A type of integers with infinity. *)

type requirement =
    int (* with infinity *)

(* Infinity is encoded as [max_int]. *)

let infinity : requirement =
  max_int

(* Addition of integers with infinity. *)

let (++) (x : requirement) (y : requirement) : requirement =
  if x = infinity || y = infinity then
    infinity
  else
    x + y

(* Comparison between an integer with infinity and a normal integer. *)

let (<==) (x : requirement) (y : int) =
  x <= y

(* ------------------------------------------------------------------------- *)

(* A uniform interface for output channels. *)

class type output = object

  (** [char c] sends the character [c] to the output channel. *)
  method char: char -> unit

  (** [substring s ofs len] sends the substring of [s] delimited by the
      offset [ofs] and the length [len] to the output channel. *)
  method substring: string -> int (* offset *) -> int (* length *) -> unit

end

(* Three kinds of output channels are wrapped so as to satisfy the above
   interface: OCaml output channels, OCaml memory buffers, and OCaml
   formatters. *)

class channel_output channel = object
  method char = output_char channel
  method substring = output_substring channel
    (* We used to use [output], but, as of OCaml 4.02 and with -safe-string
       enabled, the type of [output] has changed: this function now expects
       an argument of type [bytes]. The new function [output_substring] must
       be used instead. Furthermore, as of OCaml 4.06, -safe-string is enabled
       by default. In summary, we require OCaml 4.02, use [output_substring],
       and enable -safe-string. *)
end

class buffer_output buffer = object
  method char = Buffer.add_char buffer
  method substring = Buffer.add_substring buffer
end

class formatter_output fmt = object
  method char = function
    | '\n' -> Format.pp_force_newline fmt ()
    | ' '  -> Format.pp_print_space fmt ()
    | c    -> Format.pp_print_char fmt c

  method substring str ofs len =
    Format.pp_print_text fmt (
      if ofs = 0 && len = String.length str
      then str
      else String.sub str ofs len
    )
end

(* ------------------------------------------------------------------------- *)

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

(* ------------------------------------------------------------------------- *)

(* [initial rfrac width] creates a fresh initial state. *)

let initial rfrac width = {
  width = width;
  ribbon = max 0 (min width (truncate (float_of_int width *. rfrac)));
  last_indent = 0;
  line = 0;
  column = 0
}

(* ------------------------------------------------------------------------- *)

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

(* ------------------------------------------------------------------------- *)

(* Here is the algebraic data type of documents. It is analogous to Daan
   Leijen's version, but the binary constructor [Union] is replaced with
   the unary constructor [Group], and the constant [Line] is replaced with
   more general constructions, namely [IfFlat], which provides alternative
   forms depending on the current flattening mode, and [HardLine], which
   represents a newline character, and causes a failure in flattening mode. *)

type document =

  (* [Empty] is the empty document. *)

  | Empty

  (* [Char c] is a document that consists of the single character [c]. We
     enforce the invariant that [c] is not a newline character. *)

  | Char of char

  (* [String s] is a document that consists of just the string [s]. We
     assume, but do not check, that this string does not contain a newline
     character. [String] is a special case of [FancyString], which takes up
     less space in memory. *)

  | String of string

  (* [FancyString (s, ofs, len, apparent_length)] is a (portion of a) string
     that may contain fancy characters: color escape characters, UTF-8 or
     multi-byte characters, etc. Thus, the apparent length (which corresponds
     to what will be visible on screen) differs from the length (which is a
     number of bytes, and is reported by [String.length]). We assume, but do
     not check, that fancystrings do not contain a newline character. *)

  | FancyString of string * int * int * int

  (* [Blank n] is a document that consists of [n] blank characters. *)

  | Blank of int

    (* When in flattening mode, [IfFlat (d1, d2)] turns into the document
       [d1]. When not in flattening mode, it turns into the document [d2]. *)

  | IfFlat of document * document

  (* When in flattening mode, [HardLine] causes a failure, which requires
     backtracking all the way until the stack is empty. When not in flattening
     mode, it represents a newline character, followed with an appropriate
     number of indentation. A common way of using [HardLine] is to only use it
     directly within the right branch of an [IfFlat] construct. *)

  | HardLine

  (* The following constructors store their space requirement. This is the
     document's apparent length, if printed in flattening mode. This
     information is computed in a bottom-up manner when the document is
     constructed. *)

  (* In other words, the space requirement is the number of columns that the
     document needs in order to fit on a single line. We express this value in
     the set of `integers extended with infinity', and use the value
     [infinity] to indicate that the document cannot be printed on a single
     line. *)

  (* Storing this information at [Group] nodes is crucial, as it allows us to
     avoid backtracking and buffering. *)

  (* Storing this information at other nodes allows the function [requirement]
     to operate in constant time. This means that the bottom-up computation of
     requirements takes linear time. *)

  (* [Cat (req, doc1, doc2)] is the concatenation of the documents [doc1] and
     [doc2]. The space requirement [req] is the sum of the requirements of
     [doc1] and [doc2]. *)

  | Cat of requirement * document * document

  (* [Nest (req, j, doc)] is the document [doc], in which the indentation
     level has been increased by [j], that is, in which [j] blanks have been
     inserted after every newline character. The space requirement [req] is
     the same as the requirement of [doc]. *)

  | Nest of requirement * int * document

  (* [Group (req, doc)] represents an alternative: it is either a flattened
     form of [doc], in which occurrences of [Group] disappear and occurrences
     of [IfFlat] resolve to their left branch, or [doc] itself. The space
     requirement [req] is the same as the requirement of [doc]. *)

  | Group of requirement * document

  (* [Align (req, doc)] increases the indentation level to reach the current
     column.  Thus, the document [doc] is rendered within a box whose upper
     left corner is the current position. The space requirement [req] is the
     same as the requirement of [doc]. *)

  | Align of requirement * document

  (* [Range (req, hook, doc)] is printed like [doc]. After it is printed, the
     function [hook] is applied to the range that is occupied by [doc] in the
     output. *)

  | Range of requirement * (range -> unit) * document

  (* [Custom (req, f)] is a document whose appearance is user-defined. *)

  | Custom of custom

(* ------------------------------------------------------------------------- *)

(* Retrieving or computing the space requirement of a document. *)

let rec requirement = function
  | Empty ->
      0
  | Char _ ->
      1
  | String s ->
      String.length s
  | FancyString (_, _, _, len)
  | Blank len ->
      len
  | IfFlat (doc1, _) ->
      (* In flattening mode, the requirement of [ifflat x y] is just the
         requirement of its flat version, [x]. *)
      (* The smart constructor [ifflat] ensures that [IfFlat] is never nested
         in the left-hand side of [IfFlat], so this recursive call is not a
         problem; the function [requirement] has constant time complexity. *)
      requirement doc1
  | HardLine ->
      (* A hard line cannot be printed in flattening mode. *)
      infinity
  | Cat (req, _, _)
  | Nest (req, _, _)
  | Group (req, _)
  | Align (req, _)
  | Range (req, _, _) ->
      (* These nodes store their requirement -- which is computed when the
         node is constructed -- so as to allow us to answer in constant time
         here. *)
      req
  | Custom c ->
      c#requirement

(* ------------------------------------------------------------------------- *)

(* The above algebraic data type is not exposed to the user. Instead, we
   expose the following functions. These functions construct a raw document
   and compute its requirement, so as to obtain a document. *)

let empty =
  Empty

let char c =
  assert (c <> '\n');
  Char c

let space =
  char ' '

let string s =
  String s

let fancysubstring s ofs len apparent_length =
  if len = 0 then
    empty
  else
    FancyString (s, ofs, len, apparent_length)

let substring s ofs len =
  fancysubstring s ofs len len

let fancystring s apparent_length =
  fancysubstring s 0 (String.length s) apparent_length

(* The following function was stolen from [Batteries]. *)
let utf8_length s =
  let rec length_aux s c i =
    if i >= String.length s then c else
    let n = Char.code (String.unsafe_get s i) in
    let k =
      if n < 0x80 then 1 else
      if n < 0xe0 then 2 else
      if n < 0xf0 then 3 else 4
    in
    length_aux s (c + 1) (i + k)
  in
  length_aux s 0 0

let utf8string s =
  fancystring s (utf8_length s)

let utf8format f =
  Printf.ksprintf utf8string f

let hardline =
  HardLine

let blank n =
  match n with
  | 0 ->
      empty
  | 1 ->
      space
  | _ ->
      Blank n

let ifflat doc1 doc2 =
  (* Avoid nesting [IfFlat] in the left-hand side of [IfFlat], as this
     is redundant. *)
  match doc1 with
  | IfFlat (doc1, _)
  | doc1 ->
      IfFlat (doc1, doc2)

let internal_break i =
  ifflat (blank i) hardline

let break0 =
  internal_break 0

let break1 =
  internal_break 1

let break i =
  match i with
  | 0 ->
      break0
  | 1 ->
      break1
  | _ ->
      internal_break i

let (^^) x y =
  match x, y with
  | Empty, _ ->
      y
  | _, Empty ->
      x
  | _, _ ->
      Cat (requirement x ++ requirement y, x, y)

let nest i x =
  assert (i >= 0);
  Nest (requirement x, i, x)

let group x =
  let req = requirement x in
  (* Minor optimisation: an infinite requirement dissolves a group. *)
  if req = infinity then
    x
  else
    Group (req, x)

let align x =
  Align (requirement x, x)

let range hook x =
  Range (requirement x, hook, x)

let custom c =
  (* Sanity check. *)
  assert (c#requirement >= 0);
  Custom c

(* ------------------------------------------------------------------------- *)

(* Printing blank space (indentation characters). *)

let blank_length =
  80

let blank_buffer =
  String.make blank_length ' '

let rec blanks output n =
  if n <= 0 then
    ()
  else if n <= blank_length then
    output#substring blank_buffer 0 n
  else begin
    output#substring blank_buffer 0 blank_length;
    blanks output (n - blank_length)
  end

(* ------------------------------------------------------------------------- *)

(* This function expresses the following invariant: if we are in flattening
   mode, then we must be within bounds, i.e. the width and ribbon width
   constraints must be respected. *)

let ok state flatten : bool =
  not flatten ||
  state.column <= state.width && state.column <= state.last_indent + state.ribbon

(* ------------------------------------------------------------------------- *)

(* The pretty rendering engine. *)

(* The renderer is supposed to behave exactly like Daan Leijen's, although its
   implementation is quite radically different, and simpler. Our documents are
   constructed eagerly, as opposed to lazily. This means that we pay a large
   space overhead, but in return, we get the ability of computing information
   bottom-up, as described above, which allows to render documents without
   backtracking or buffering. *)

(* The [state] record is never copied; it is just threaded through. In
   addition to it, the parameters [indent] and [flatten] influence the
   manner in which the document is rendered. *)

(* The code is written in tail-recursive style, so as to avoid running out of
   stack space if the document is very deep. Each [KCons] cell in a
   continuation represents a pending call to [pretty]. Each [KRange] cell
   represents a pending call to a user-provided range hook. *)

type cont =
  | KNil
  | KCons of int * bool * document * cont
  | KRange of (range -> unit) * point * cont

let rec pretty
  (output : output)
  (state : state)
  (indent : int)
  (flatten : bool)
  (doc : document)
  (cont : cont)
: unit =
  match doc with

  | Empty ->
      continue output state cont

  | Char c ->
      output#char c;
      state.column <- state.column + 1;
      (* assert (ok state flatten); *)
      continue output state cont

  | String s ->
      let len = String.length s in
      output#substring s 0 len;
      state.column <- state.column + len;
      (* assert (ok state flatten); *)
      continue output state cont

  | FancyString (s, ofs, len, apparent_length) ->
      output#substring s ofs len;
      state.column <- state.column + apparent_length;
      (* assert (ok state flatten); *)
      continue output state cont

  | Blank n ->
      blanks output n;
      state.column <- state.column + n;
      (* assert (ok state flatten); *)
      continue output state cont

  | HardLine ->
      (* We cannot be in flattening mode, because a hard line has an [infinity]
         requirement, and we attempt to render a group in flattening mode only
         if this group's requirement is met. *)
      assert (not flatten);
      (* Emit a hardline. *)
      output#char '\n';
      blanks output indent;
      state.line <- state.line + 1;
      state.column <- indent;
      state.last_indent <- indent;
      (* Continue. *)
      continue output state cont

  | IfFlat (doc1, doc2) ->
      (* Pick an appropriate sub-document, based on the current flattening
         mode. *)
      pretty output state indent flatten (if flatten then doc1 else doc2) cont

  | Cat (_, doc1, doc2) ->
      (* Push the second document onto the continuation. *)
      pretty output state indent flatten doc1 (KCons (indent, flatten, doc2, cont))

  | Nest (_, j, doc) ->
      pretty output state (indent + j) flatten doc cont

  | Group (req, doc) ->
      (* If we already are in flattening mode, stay in flattening mode; we
         are committed to it. If we are not already in flattening mode, we
         have a choice of entering flattening mode. We enter this mode only
         if we know that this group fits on this line without violating the
         width or ribbon width constraints. Thus, we never backtrack. *)
      let flatten =
        flatten ||
        let column = state.column ++ req in
        column <== state.width && column <== state.last_indent + state.ribbon
      in
      pretty output state indent flatten doc cont

  | Align (_, doc) ->
      (* The effect of this combinator is to set [indent] to [state.column].
         Usually [indent] is equal to [state.last_indent], hence setting it
         to [state.column] increases it. However, if [nest] has been used
         since the current line began, then this could cause [indent] to
         decrease. *)
      (* assert (state.column > state.last_indent); *)
      pretty output state state.column flatten doc cont

  | Range (_, hook, doc) ->
      let start : point = (state.line, state.column) in
      pretty output state indent flatten doc (KRange (hook, start, cont))

  | Custom c ->
      (* Invoke the document's custom rendering function. *)
      c#pretty output state indent flatten;
      (* Sanity check. *)
      assert (ok state flatten);
      (* Continue. *)
      continue output state cont

and continue output state = function
  | KNil ->
      ()
  | KCons (indent, flatten, doc, cont) ->
      pretty output state indent flatten doc cont
  | KRange (hook, start, cont) ->
      let finish : point = (state.line, state.column) in
      hook (start, finish);
      continue output state cont

(* Publish a version of [pretty] that does not take an explicit continuation.
   This function may be used by authors of custom documents. We do not expose
   the internal [pretty] -- the one that takes a continuation -- because we
   wish to simplify the user's life. The price to pay is that calls that go
   through a custom document cannot be tail calls. *)

let pretty output state indent flatten doc =
  pretty output state indent flatten doc KNil

(* ------------------------------------------------------------------------- *)

(* The compact rendering algorithm. *)

let rec compact output doc cont =
  match doc with
  | Empty ->
      continue output cont
  | Char c ->
      output#char c;
      continue output cont
  | String s ->
      let len = String.length s in
      output#substring s 0 len;
      continue output cont
  | FancyString (s, ofs, len, _apparent_length) ->
      output#substring s ofs len;
      continue output cont
  | Blank n ->
      blanks output n;
      continue output cont
  | HardLine ->
      output#char '\n';
      continue output cont
  | Cat (_, doc1, doc2) ->
      compact output doc1 (doc2 :: cont)
  | IfFlat (doc, _)
  | Nest (_, _, doc)
  | Group (_, doc)
  | Align (_, doc)
  | Range (_, _, doc) ->
      compact output doc cont
  | Custom c ->
      (* Invoke the document's custom rendering function. *)
      c#compact output;
      continue output cont

and continue output cont =
  match cont with
  | [] ->
      ()
  | doc :: cont ->
      compact output doc cont

let compact output doc =
  compact output doc []

(* ------------------------------------------------------------------------- *)

(* We now instantiate the renderers for the three kinds of output channels. *)

(* This is just boilerplate. *)

module MakeRenderer (X : sig
  type channel
  val output: channel -> output
end) = struct
  type channel = X.channel
  type dummy = document
  type document = dummy
  let pretty rfrac width channel doc = pretty (X.output channel) (initial rfrac width) 0 false doc
  let compact channel doc = compact (X.output channel) doc
end

module ToChannel =
  MakeRenderer(struct
    type channel = out_channel
    let output = new channel_output
  end)

module ToBuffer =
  MakeRenderer(struct
    type channel = Buffer.t
    let output = new buffer_output
  end)

module ToFormatter =
  MakeRenderer(struct
    type channel = Format.formatter
    let output = new formatter_output
  end)
