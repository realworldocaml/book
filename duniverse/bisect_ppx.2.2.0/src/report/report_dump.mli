(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



(** This module defines the output to dump. *)


val make : unit -> Report_generic.converter
(** Returns a converter for dump output. *)
