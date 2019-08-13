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

(** Code blocks. *)

type cram_value = { pad: int; tests: Cram.t list }

(** The type for block values. *)
type value =
  | Raw
  | OCaml
  | Error of string list
  | Cram of cram_value
  | Toplevel of Toplevel.t list

type section = int * string
(** The type for sections. *)

(** The type for supported code blocks. *)
type t = {
  line    : int;
  file    : string;
  section : section option;
  labels  :
    (string * ([`Eq | `Neq | `Le | `Lt | `Ge | `Gt] * string) option) list;
  header  : string option;
  contents: string list;
  value   : value;
}

val empty: t
(** [empty] is the empty block. *)

(** {2 Printers} *)

val dump: t Fmt.t
(** [dump] is the printer for dumping code blocks. Useful for debugging. *)

val pp_header: ?syntax:Syntax.t -> t Fmt.t
(** [pp_header] pretty-prints block headers. *)

val pp_contents: ?syntax:Syntax.t -> t Fmt.t
(** [pp_contents] pretty-prints block contents. *)

val pp_footer: ?syntax:Syntax.t -> unit Fmt.t
(** [pp_footer] pretty-prints block footer. *)

val pp: ?syntax:Syntax.t -> t Fmt.t
(** [pp] pretty-prints blocks. *)

val pp_line_directive: (string * int) Fmt.t
(** [pp_line_directive] pretty-prints a line directive given as a
   filename and line number. *)

(** {2 Accessors} *)

val mode: t -> [`Non_det of [`Command|`Output] | `Normal]
(** [mode t] is [t]'s mode. *)

val directory: t -> string option
(** [directory t] is the directory where [t] tests should be run. *)

val source_trees: t -> string list
(** [source_trees t] is the list of extra source-trees to add as
   dependency of the code-block. *)

val file: t -> string option
(** [file t] is the name of the file to synchronize [t] with. *)

val part: t -> string option
(** [part t] is the part of the file to synchronize [t] with.
    If lines is not specified synchronize the whole file. *)

val environment: t -> string
(** [environment t] is the name given to the environment where [t] tests
    are run. *)

val set_variables: t -> (string * string) list
(** [set_variable t] is the list of environment variable to set and their values *)

val unset_variables: t -> string list
(** [unset_variable t] is the list of environment variable to unset *)

val required_packages: t -> string list
(** [required_packages t] is the names of the required packages *)

val required_libraries: t -> (Library.Set.t, string) Result.result
(** [required_packages t] is the names of the required packages *)

val skip: t -> bool
(** [skip t] is true iff [skip] is in the labels of [t]. *)

val value: t -> value
(** [value t] is [t]'s value. *)

val section: t -> section option
(** [section t] is [t]'s section. *)

val header: t -> string option
(** [header t] is [t]'s header. *)

val executable_contents: t -> string list
(** [executable_contents t] is either [t]'s contents if [t] is a raw
   or a cram block, or [t]'s commands if [t] is a toplevel fragments
   (e.g. the phrase result is discarded). *)

val version:
  t ->
  [`Eq | `Neq | `Ge | `Gt | `Le | `Lt] * int option * int option * int option
(** [version t] is [t]'s OCaml version. *)

val version_enabled: t -> bool
(** [version_supported t] if the current OCaml version complies with [t]'s
    version. *)

(** {2 Evaluation} *)

val eval: t -> t
(** [eval t] is the same as [t] but with it's value replaced by either
   [Cram] or [Toplevel] blocks, depending on [t]'s header. *)

(** {2 Parsers} *)

val labels_of_string:
  string ->
  (string * ([`Eq | `Neq | `Gt | `Ge | `Lt | `Le] * string) option) list
(** [labels_of_string s] cuts [s] into a list of labels. *)

val require_from_line : string -> (Library.Set.t, string) Result.result
(** [require_from_line line] returns the set of libraries imported by the
    #require statement on [line] or an empty set if [line] is not a require
    statement. *)

val require_from_lines : string list -> (Library.Set.t, string) Result.result
(** Same as [require_from_line] but aggregated over several lines *)
