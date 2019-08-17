(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 2.1 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

(** Passed to the function specified with the [Extended] output_kind *)
type output_elt = Newline | Indent of int | Whitespace of string | Text of string

(** * If [Print f], the whole input is fed as strings through f, with expected
    lines reindented (with spaces).
    * If [Numeric f], the indentation values (i.e. total number of leading
    spaces) for each lines on which [in_lines] is true are passed through the
    function.
    * If [Extended f], every element is fed to [f] with arguments [state
    element]. There is at least an element for each token, but there may be more
    (whitespace, multiline tokens...). You may safely raise an exception from
    [f] to stop further processing. This version can be used for syntax
    highlighting or storing checkpoints. *)
type 'a output_kind =
  | Numeric of (int -> 'a -> 'a)
  | Print of (string -> 'a -> 'a)
  | Extended of (IndentBlock.t -> output_elt -> 'a -> 'a)

type 'a output = {
  debug: bool;
  config: IndentConfig.t;
  (** Returns true on the lines that should be reindented (lines start at 1) *)
  in_lines: int -> bool;
  (** if true, partial indent will adapt to the current indent of the file *)
  adaptive: bool;
  indent_empty: bool;
  kind: 'a output_kind;
}

val std_output : unit output

val proceed : 'a output -> Nstream.t -> IndentBlock.t -> 'a -> 'a
