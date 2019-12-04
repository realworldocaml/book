(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



(** This module defines the output to bare text. *)


val make : bool -> Report_generic.converter
(** Returns a converter for bare text output, the passed boolean indicates
    whether only summary should be output. *)
