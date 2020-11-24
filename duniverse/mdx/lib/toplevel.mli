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

(** Toplevel phrases. *)

type t = {
  vpad : int;
  hpad : int;
  line : int;
  command : string list;
  output : Output.t list;
}
(** The type for top-level phrases. *)

(** {2 Pretty-printing} *)

val dump : t Fmt.t
(** [dump] is the printer for dumping toplevel phrases. Useful for
   debugging. *)

val pp : t Fmt.t
(** [pp] is the pretty-printer for top-level phrases. [pad] is the
   size of the optionnalwhitespace left padding (by default is is
   0). *)

val pp_command : t Fmt.t
(** [pp_command] is the pretty-printer for toplevel commands. *)

(** {2 Parser} *)

val of_lines :
  syntax:Syntax.t ->
  file:string ->
  line:int ->
  column:int ->
  string list ->
  t list
(** [of_lines ~file ~line ~column lines] is the list of toplevel blocks from
   file [file] starting at line [line]. Return the vertical and
   horizontal whitespace padding as well.*)

(** {2 Accessors} *)

val command : t -> string list
(** [command t] is [t]'s command. *)

val output : t -> Output.t list
(** [output t] is [t]'s output. *)
