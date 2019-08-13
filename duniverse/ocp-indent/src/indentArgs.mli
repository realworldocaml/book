(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
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

type input = InChannel of in_channel
           | File of string

(* Type of parameters obtained from command-line options *)
type t = private {
  file_out : string option;
  numeric: bool;
  indent_config: string list;
  debug: bool;
  inplace : bool;
  indent_empty: bool;
  in_lines: int -> bool;
  indent_printer: out_channel -> unit IndentPrinter.output_kind;
  syntax_exts: string list;
  dynlink : [`Pkg of string | `Mod of string ] list;
}

val options: (t * input list) Cmdliner.Term.t

val info: Cmdliner.Term.info
