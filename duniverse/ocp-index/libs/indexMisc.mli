(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the Lesser GNU Public License version 3.0.                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

val debug_enabled: bool

val debug: ('a, out_channel, unit) format -> 'a

val timer: unit -> unit -> float

(** Similar to List.fold_left but with 1 step look-ahead *)
val foldl_next: ('acc -> 'a -> 'a option -> 'acc) -> 'acc -> 'a list -> 'acc

val string_split: char -> string -> string list

type key = char list
val dot: char
val dots: string

(** Used to get the keys (or paths) in the trie *)
val string_to_key: string -> key

val key_to_string: key -> string

val modpath_to_key: ?enddot:bool -> string list -> key

val key_to_modpath: key -> string list

val modpath_to_string: string list -> string

(** Returns the parent type for field records, variants and methods *)
val parent_type: IndexTypes.info -> IndexTypes.info option

(* * Returns the list of directories and all their recursive subdirectories.
    If directory basename verifies [skip], it is not descended nor returned. *)
val unique_subdirs: ?skip:(string -> bool) -> string list -> string list

(* * An heuristic to guess where the root directory of the current project.
    Returns (project_root, build_dir) *)
val project_root: ?path:string -> unit -> string option * string option

(* * Locates a build dir within a given directory, based on name ([_build],
    [_obuild], etc.) *)
val find_build_dir: string -> string option

(** Shorten [file] by making it relative to current [path] (default cwd) *)
val make_relative: ?path:string -> string -> string
