(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



(** This module defines the output to HTML. *)


val output :
  (string -> unit) -> string -> int -> string -> (string -> string option) ->
  (string, int array) Hashtbl.t -> (string, string) Hashtbl.t ->
    unit
(** [output verbose dir tab_size title resolver data points] writes all the HTML
    files for [data] in the directory [dir]. [verbose] is used for verbose
    output, [tab_size] is the number of space characters to use as a replacement
    for tabulations, [title] is the title for generated pages, and [resolver]
    associates actual paths to given filenames. [points] gives the marshalled
    locations of the points in the file. *)
