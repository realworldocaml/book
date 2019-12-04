(**************************************************************************)
(*                                                                        *)
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

type threechoices = Always | Never | Auto

(** See the [man] function to get the details of what the options are
    supposed to do (or the template .ocp-indent) *)
type t = {

  i_base: int;
  (** indentation values *)
  i_type: int;
  i_in: int;
  i_with: int;
  i_match_clause: int;
  i_ppx_stritem_ext: int;

  i_max_indent: int option;
  (** indentation toggles *)
  i_strict_with: threechoices;
  i_strict_else: threechoices;
  i_strict_comments: bool;
  i_align_ops: bool;
  i_align_params: threechoices;
}

(** Documentation of the indentation options, in the Cmdliner 'Manpage.block' format *)

type man_block =
  [ `S of string | `P of string | `Pre of string | `I of string * string
  | `Noblank | `Blocks of man_block list ]

val man: man_block list

val default: t

(** String format is ["option=value,option2=value,..."]. Commas can be replaced
    by newlines. Use [?extra] to handle extra options (by side-effects only) *)
val update_from_string : ?extra:(string -> (string -> unit) option) ->
  t -> string -> t

(** sep should be comma or newline if you want to reparse. Comma by default *)
val to_string : ?sep:string -> t -> string

(** Load from the given filename, optionally updating from the given indent
    instead of the default one. On error, returns the original indent config
    unchanged and prints a message to stderr. The file may also contain
    bindings of the form 'syntax=SYNTAX_EXTENSION[,...]', that are returned
    as a the list of their names *)
val load: ?indent:t -> string -> t * string list * [`Mod of string | `Pkg of string] list

(** Save the given indent config to the given filename; returns true on
    success *)
val save: t -> string -> bool

(** Looks in given and parent directories for a [.ocp-indent] configuration
    file *)
val find_conf_file: string -> string option

(** Returns the local default configuration, obtained from (in order), the
    built-in [default], the file [~/.ocp/ocp-indent.conf], a file [.ocp-indent]
    in the current directory or any parent, and the environment variable
    [OCP_INDENT_CONFIG]. Returns the list of syntax extensions that may
    have been activated in conf-files as well *)
val local_default: ?path:string -> unit -> t * string list * [`Mod of string | `Pkg of string] list
