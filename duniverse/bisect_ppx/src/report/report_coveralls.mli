(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



(** This module defines the output to Coveralls JSON. *)


val output :
  (string -> unit) -> string -> string -> string -> string -> (string -> string option) ->
  (string, int array) Hashtbl.t -> (string, string) Hashtbl.t ->
    unit
(** [output verbose file service_name service_job_id repo_token resolver data points]
    writes a Coveralls JSON [file] for [data]. [verbose] is used for verbose
    output. [service_name], [service_job_id], and [repo_token] are each used for the
    respective field in the Coveralls JSON, if not the emptry string. [resolver]
    associates actual paths to given filenames. [points] gives the marshalled
    locations of the points in the file. *)
