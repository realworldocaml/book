(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* A stretch is a fragment of a source file. It holds the file name,
   the line number, and the line count (that is, the length) of the
   fragment. These are used to generate line number directives when the
   fragment is copied to an output file. It also holds the textual
   content of the fragment, as a string. The [raw_content] field holds
   the text that was found in the source file, while the [content]
   field holds the same text after transformation by the lexer (which
   may substitute keywords, insert padding, insert parentheses, etc.).
   See [Lexer.mk_stretch] and its various call sites in [Lexer]. *)

type t = {
    stretch_filename    : string;
    stretch_linenum     : int;
    stretch_linecount   : int;
    stretch_raw_content : string;
    stretch_content     : string;
    stretch_keywords    : Keyword.keyword list
  }

(* An OCaml type is either a stretch (if it was found in some
   source file) or a string (if it was inferred via [Infer]). *)

type ocamltype =
  | Declared of t
  | Inferred of string
