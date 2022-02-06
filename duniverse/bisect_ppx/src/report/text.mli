(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



val output :
  per_file:bool ->
  coverage_files:string list ->
  coverage_paths:string list ->
  expect:string list ->
  do_not_expect:string list ->
    unit
