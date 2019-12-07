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

(** This module contains definitions for the predefined OCaml elements which are
    not in Pervasives like base types ([int], [char]...) and exceptions
    ([Match_failure]...) *)

val types: IndexTypes.info list

val variants: IndexTypes.info list

val exceptions: IndexTypes.info list

val keywords: IndexTypes.info list

val all: IndexTypes.info list
