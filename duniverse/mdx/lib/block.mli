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

(** Code blocks headers. *)

module Header : sig
  type t = Shell of [ `Sh | `Bash ] | OCaml | Other of string

  val pp : Format.formatter -> t -> unit
  val of_string : string -> t option
  val infer_from_file : string -> t option
end

(** Code blocks. *)

type cram_value = { language : [ `Bash | `Sh ]; non_det : Label.non_det option }

type ocaml_value = {
  env : Ocaml_env.t;
      (** [env] is the name given to the environment where tests are run. *)
  non_det : Label.non_det option;
  errors : Output.t list;
}

type toplevel_value = {
  env : Ocaml_env.t;
      (** [env] is the name given to the environment where tests are run. *)
  non_det : Label.non_det option;
}

type include_ocaml_file = {
  part_included : string option;
      (** [part_included] is the part of the file to synchronize with.
          If lines is not specified synchronize the whole file. *)
}

type include_other_file = { header : Header.t option }

type include_file_kind =
  | Fk_ocaml of include_ocaml_file
  | Fk_other of include_other_file

type include_value = {
  file_included : string;
      (** [file_included] is the name of the file to synchronize with. *)
  file_kind : include_file_kind;
}

type raw_value = { header : Header.t option }

(** The type for block values. *)
type value =
  | Raw of raw_value
  | OCaml of ocaml_value
  | Cram of cram_value
  | Toplevel of toplevel_value
  | Include of include_value

type section = int * string
(** The type for sections. *)

type t = {
  loc : Location.t;
  section : section option;
  dir : string option;
  labels : Label.t list;
  legacy_labels : bool;
  contents : string list;
  skip : bool;
  version_enabled : bool;
      (** Whether the current OCaml version complies with the block's version. *)
  set_variables : (string * string) list;
  unset_variables : string list;
  value : value;
}
(** The type for supported code blocks. *)

val mk :
  loc:Location.t ->
  section:section option ->
  labels:Label.t list ->
  legacy_labels:bool ->
  header:Header.t option ->
  contents:string list ->
  errors:Output.t list ->
  (t, [ `Msg of string ]) Result.result

val mk_include :
  loc:Location.t ->
  section:section option ->
  labels:Label.t list ->
  (t, [ `Msg of string ]) Result.result
(** [mk_include] builds an include block from a comment [<!-- $MDX ... -->]
    that is not followed by a code block [``` ... ```]. *)

(** {2 Printers} *)

val dump : t Fmt.t
(** [dump] is the printer for dumping code blocks. Useful for debugging. *)

val pp_header : ?syntax:Syntax.t -> t Fmt.t
(** [pp_header] pretty-prints full block headers with the labels. *)

val pp_contents : ?syntax:Syntax.t -> t Fmt.t
(** [pp_contents] pretty-prints block contents. *)

val pp_footer : ?syntax:Syntax.t -> t Fmt.t
(** [pp_footer] pretty-prints block footer. *)

val pp : ?syntax:Syntax.t -> t Fmt.t
(** [pp] pretty-prints blocks. *)

val pp_line_directive : (string * int) Fmt.t
(** [pp_line_directive] pretty-prints a line directive given as a
   filename and line number. *)

(** {2 Accessors} *)

val non_det : t -> Label.non_det option
(** Whether a block's command or output is non-deterministic. *)

val directory : t -> string option
(** [directory t] is the directory where [t] tests should be run. *)

val file : t -> string option
(** [file t] is the name of the file to synchronize [t] with. *)

val set_variables : t -> (string * string) list
(** [set_variable t] is the list of environment variable to set and their values *)

val unset_variables : t -> string list
(** [unset_variable t] is the list of environment variable to unset *)

val skip : t -> bool
(** [skip t] is true iff [skip] is in the labels of [t]. *)

val value : t -> value
(** [value t] is [t]'s value. *)

val section : t -> section option
(** [section t] is [t]'s section. *)

val executable_contents : syntax:Syntax.t -> t -> string list
(** [executable_contents t] is either [t]'s contents if [t] is a raw
   or a cram block, or [t]'s commands if [t] is a toplevel fragments
   (e.g. the phrase result is discarded). *)

val is_active : ?section:string -> t -> bool
