(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



(** This module merges coverage files into a single coverage file, summing counters pointwise *)


val output :
  to_file:string ->
  coverage_files:string list ->
  coverage_paths:string list ->
    unit
