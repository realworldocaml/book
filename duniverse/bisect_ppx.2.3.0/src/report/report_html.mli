(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



(** This module defines the output to HTML. *)


type theme = [
  | `Light
  | `Dark
  | `Auto
]

val output :
  (string -> unit) ->
  string ->
  int ->
  string ->
  theme ->
  (string -> string option) ->
  (string, int array) Hashtbl.t ->
  (string, string) Hashtbl.t ->
    unit
(** [output verbose dir tab_size title theme resolver data points] writes all
    the HTML files for [data] in the directory [dir]. [verbose] is used for
    verbose output, [tab_size] is the number of space characters to use as a
    replacement for tabulations, [title] is the title for generated pages, and
    [resolver] associates actual paths to given filenames. [points] gives the
    marshalled locations of the points in the file. *)
