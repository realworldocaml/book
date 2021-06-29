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

(* -------------------------------------------------------------------------- *)

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

(* Comparison of requirements is just ordinary comparison. *)

(* -------------------------------------------------------------------------- *)

(* The type of documents. See [PPrintEngine] for documentation. *)

type document =
  | Empty
  | FancyString of string * int * int * int
  | Blank of int
  | IfFlat of document * document
  | HardLine
  | Cat of requirement * document * document
  | Nest of requirement * int * document
  | Group of requirement * document

(* -------------------------------------------------------------------------- *)

(* Retrieving or computing the space requirement of a document. *)

let rec requirement = function
  | Empty ->
      0
  | FancyString (_, _, _, len)
  | Blank len ->
      len
  | IfFlat (doc1, _) ->
      requirement doc1
  | HardLine ->
      infinity
  | Cat (req, _, _)
  | Nest (req, _, _)
  | Group (req, _) ->
      req

(* -------------------------------------------------------------------------- *)

(* Document constructors. *)

let empty =
  Empty

let fancysubstring s ofs len apparent_length =
  if len = 0 then
    empty
  else
    FancyString (s, ofs, len, apparent_length)

let fancystring s apparent_length =
  fancysubstring s 0 (String.length s) apparent_length

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

let char c =
  assert (c <> '\n');
  fancystring (String.make 1 c) 1

let space =
  char ' '

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
  if req = infinity then
    x
  else
    Group (req, x)

(* -------------------------------------------------------------------------- *)

(* Printing blank space (indentation characters). *)

let blank_length =
  80

let blank_buffer =
  String.make blank_length ' '

let rec blanks output n =
  if n <= 0 then
    ()
  else if n <= blank_length then
    Buffer.add_substring output blank_buffer 0 n
  else begin
    Buffer.add_substring output blank_buffer 0 blank_length;
    blanks output (n - blank_length)
  end

(* -------------------------------------------------------------------------- *)

(* The rendering engine maintains the following internal state. *)

(* For simplicity, the ribbon width is considered equal to the line
   width; in other words, there is no ribbon width constraint. *)

(* For simplicity, the output channel is required to be an OCaml buffer.
   It is stored within the [state] record. *)

type state =
  {
    (* The line width. *)
    width: int;
    (* The current column. *)
    mutable column: int;
    (* The output buffer. *)
    output: Buffer.t;
  }

(* -------------------------------------------------------------------------- *)

(* For simplicity, the rendering engine is *not* in tail-recursive style. *)

let rec pretty state (indent : int) (flatten : bool) doc =
  match doc with

  | Empty ->
      ()

  | FancyString (s, ofs, len, apparent_length) ->
      Buffer.add_substring state.output s ofs len;
      state.column <- state.column + apparent_length

  | Blank n ->
      blanks state.output n;
      state.column <- state.column + n

  | HardLine ->
      assert (not flatten);
      Buffer.add_char state.output '\n';
      blanks state.output indent;
      state.column <- indent

  | IfFlat (doc1, doc2) ->
      pretty state indent flatten (if flatten then doc1 else doc2)

  | Cat (_, doc1, doc2) ->
      pretty state indent flatten doc1;
      pretty state indent flatten doc2

  | Nest (_, j, doc) ->
      pretty state (indent + j) flatten doc

  | Group (req, doc) ->
      let flatten = flatten || state.column ++ req <= state.width in
      pretty state indent flatten doc

(* -------------------------------------------------------------------------- *)

(* The engine's entry point. *)

let pretty width doc =
  let output = Buffer.create 512 in
  let state = { width; column = 0; output } in
  pretty state 0 false doc;
  Buffer.contents output
