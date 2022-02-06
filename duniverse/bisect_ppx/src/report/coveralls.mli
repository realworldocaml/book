(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



(** This module defines the output to Coveralls JSON. *)


val output :
  to_file:string ->
  service_name:string ->
  service_number:string ->
  service_job_id:string ->
  service_pull_request:string ->
  repo_token:string ->
  git:bool ->
  parallel:bool ->
  coverage_files:string list ->
  coverage_paths:string list ->
  source_paths:string list ->
  ignore_missing_files:bool ->
  expect:string list ->
  do_not_expect:string list ->
    unit

val output_and_send :
  service:[ `Codecov | `Coveralls ] ->
  service_name:string ->
  service_number:string ->
  service_job_id:string ->
  service_pull_request:string ->
  repo_token:string ->
  git:bool ->
  parallel:bool ->
  dry_run:bool ->
  coverage_files:string list ->
  coverage_paths:string list ->
  source_paths:string list ->
  ignore_missing_files:bool ->
  expect:string list ->
  do_not_expect:string list ->
    unit
