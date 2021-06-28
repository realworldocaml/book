(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



(** This module defines the output to CSV. *)


val make : string -> Report_generic.converter
(** Returns a converter for CSV output, the passed string being the separator. *)
