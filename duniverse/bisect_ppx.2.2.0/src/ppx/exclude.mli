(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



(** This modules defines the types related to exlusion as stored in
    files. *)

exception Exception of (int * string)
(** The exception raised by either the lexer, or the parser. *)

type t =
  | Name of string (** The exclusion is specified through an exact name. *)
  | Regexp of Str.regexp (** The exclusion is specified through a regular expression over names. *)
(** The type of an exclusion. *)

type file = {
    path : t; (** The path to the file. *)
    exclusions : t list option; (** The list of exclusions. *)
  }
(** The type describing the contents of an exclusion file. *)
