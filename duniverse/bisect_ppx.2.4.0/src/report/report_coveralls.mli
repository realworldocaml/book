(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



(** This module defines the output to Coveralls JSON. *)


val output :
  (string -> unit) ->
  string -> string -> string -> string -> string -> string -> bool -> bool ->
  (string -> string option) ->
  (string, int array) Hashtbl.t ->
  (string, string) Hashtbl.t ->
    unit
(** [output verbose file service_name service_job_id repo_token resolver data points]
    writes a Coveralls JSON [file] for [data]. [verbose] is used for verbose
    output. [service_name], [service_job_id], and [repo_token] are each used for the
    respective field in the Coveralls JSON, if not the emptry string. [resolver]
    associates actual paths to given filenames. [points] gives the marshalled
    locations of the points in the file. *)
