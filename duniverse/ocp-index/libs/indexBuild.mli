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


(** This module contains the function to create our lazy data structure from
    [cmi], [cmt] and [cmti] files or from whole directories *)

open IndexTypes

(** Build the trie from a list of include directories. They will be scanned for
    [.cmi] and [.cmt] files to complete on module names, and the contents of
    these files will be lazily read whenever needed. *)
val load: string list -> t

(** Load a single file into a trie *)
val add_file: t -> string -> t

(** Consider the module at the given path as opened, i.e. rebind its contents at
    the root of the trie. If [cleanup_path], also change its contents to refer
    to the new path. *)
val open_module: ?cleanup_path:bool -> t -> string list -> t

(** Same as [open_module], but tries to open even the elements that are not in
    the external interface (this needs a cmt to be present) *)
val fully_open_module: ?cleanup_path:bool -> t -> string list -> t

(** [alias t origin alias] binds at [alias] the contents found at [origin]. If
    [~cleanup_path] is set, also change its contents to refer to the new
    path. *)
val alias: ?cleanup_path:bool -> t -> string list -> string list -> t
