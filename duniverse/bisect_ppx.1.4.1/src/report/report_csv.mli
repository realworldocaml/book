(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



(** This module defines the output to CSV. *)


val make : string -> Report_generic.converter
(** Returns a converter for CSV output, the passed string being the separator. *)
