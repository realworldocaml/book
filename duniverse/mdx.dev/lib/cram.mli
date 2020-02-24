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

(** Cram tests *)

type t = { command : string list; output : Output.t list; exit_code : int }
(** The type for cram tests. *)

(** {2 Accessors} *)

val exit_code : t -> int
(** [exit_code t] is [t]'s exit code. *)

val use_heredoc : t -> bool
(** [use_heredoc t] is true iff [t] uses the heredoc ['<<'] syntax. *)

val command_line : t -> string
(** [command_line t] is [t]'s command line. It either adds ['\'] at
   the end of each line of [t]'s command or do nothing is [t] uses the
   heredoc syntax. *)

(** {2 Parser} *)

val of_lines : string list -> int * t list
(** [of_lines l] parses the commands [l]. It returns the optional
   whitespace padding. *)

(** {2 Pretty-printer} *)

val pp : ?pad:int -> t Fmt.t
(** [pp] is the pretty-printer for cram tests. [pad] is the size of
   the optional whitespace left padding (by default it is 0). *)

val dump : t Fmt.t
(** [dump] it the printer for dumping cram tests. Useful for debugging. *)

val pp_command : ?pad:int -> t Fmt.t
(** [pp_command] pretty-prints cram commands. *)

val pp_exit_code : ?pad:int -> int Fmt.t
(** [pp_exit_code] pretty-prints exit code. *)
