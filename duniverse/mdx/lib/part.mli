(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type file
(** The type for files. *)

val read : string -> file
(** [read f] is the file [f]. It's a costly operations, better to be
   done once. *)

val contents : file -> string
(** [contents f] is [f] contents. *)

val find : file -> part:string option -> string list option
(** [find f ~part] returns the lines of the part [part] in the file
   [f]. Return [None] if [f] does not contain the part [part]. *)

val replace : file -> part:string option -> lines:string list -> file
(** [replace ~file ~part ~lines] returns the lines of the file [file] where
    the lines of part [part] have been replaced by [lines].
    If [part] does not occur in the file, a new part is added at the end. *)

(**/**)

(* Exposed for test purposes only *)

module Parse_parts : sig
  type part_decl =
    | Normal of string
    | Compat_attr of string * string
    (* ^^^^ This is for compat with the [[@@@part name]] delimiters *)
    | Part_begin of string * string
    | Part_end
    | File_end

  val parse_line : (string, [< `End_of_file ]) Result.result -> part_decl
end

(**/**)
