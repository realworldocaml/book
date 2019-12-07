(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



(** File lexer, used for 'special' comments. *)

val no_comment_parsing : bool ref
(** If set, Bisect_ppx does not parse source files for Bisect comments, such as
    BISECT-IGNORE. Then, the [get] and [line_is_ignored] functions effectively
    do nothing significant. *)

type t = {
    mutable ignored_intervals : (int * int) list; (** lines between BISECT-IGNORE-BEGIN and BISECT-IGNORE-END commments, or with BISECT-IGNORE comment. *)
    mutable marked_lines : int list; (** lines with BISECT-MARK or BISECT-VISIT comment. *)
  }

val get : string -> t
(** Returns the information about special comments for the passed file
    (parsed file are cached).

    Raises [Sys_error] if an i/o error occurs. *)

val line_is_ignored : int -> t -> bool
