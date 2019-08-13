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


(** This module contains common command-line definitions for ocp-index or other
    tools using its API. Relies on Cmdliner. *)

(** The type containing filtering informations on kinds. *)
type filter_kind = {
    t : bool ;
    v : bool ;
    e : bool ;
    c : bool ;
    m : bool ;
    s : bool ;
    k : bool ;
  }

(** The type for common command-line options *)
type t = {
  lib_info: LibIndex.t;
  color: bool;
  mutable filter: filter_kind ;
  project_root: string option;
}

val filter : t -> LibIndex.info -> bool

(** The cmdliner term to get the common options and create the [LibIndex.t]
    structure *)
val common_opts :
  ?default_filter:[ `C | `E | `K | `M | `S | `T | `V ] list ->
  unit ->  t Cmdliner.Term.t
