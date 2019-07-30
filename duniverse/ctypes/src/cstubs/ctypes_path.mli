(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Value paths (long identifiers) *)

type path

val path_of_string : string -> path
val format_path : Format.formatter -> path -> unit
