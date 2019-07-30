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

(* * This module contains open types, for use within the library only. Its
    interface should only be exported through closed types in LibIndex. *)

(** Internal representation of types *)
type ty = Outcometree.out_sig_item

(** The type of files we get our data from *)
type orig_file = Cmt of string | Cmti of string | Cmi of string

(** Contains the information on a given identifier *)
type info = { path: string list;
              orig_path: string list;
              kind: kind;
              name: string;
              ty: ty option;
              loc_sig: Location.t Lazy.t;
              loc_impl: Location.t Lazy.t;
              doc: string option Lazy.t;
              file: orig_file;
           (* library: string option *) }

(** The kind of elements that can be stored in the trie *)
and kind =
  | Type | Value | Exception
  | OpenType
  | Field of info | Variant of info
  | Method of info
  | Module | ModuleType
  | Class | ClassType
  | Keyword

(** Lazy trie structure holding the info on all identifiers *)
type t = (char, info) Trie.t

(* * Raised when cmi/cmt/cmti files can't be loaded. Probably a different
    version of OCaml *)
exception Bad_format of string
