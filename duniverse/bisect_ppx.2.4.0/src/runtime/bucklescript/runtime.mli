(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



val register_file :
  bisect_file:string option ->
  bisect_silent:string option ->
  string ->
  point_count:int ->
  point_definitions:string ->
    [`Staged of (int -> unit)]
(** [register_file file ~bisect_file ~bisect_silent ~point_count
    ~point_definitions] indicates that the file [file] is part of the
    application that has been instrumented. [point_definitions] is a serialized
    [Common.point_definition list] giving the locations of all points in the
    file. The returned callback is used to increment visitation counts.

    [~bisect_file] and [~bisect_silent] are ignored. *)

val get_coverage_data : unit -> string option
(** Returns the binary coverage data accumulated by the program so far. This
    should eventually be written to a file, to be processed by
    [bisect-ppx-report]. *)

val write_coverage_data : unit -> unit
(** On Node.js, writes the same coverage data that is returned by
    {!get_coverage_data} to a [.coverage] file with a randomized name in the
    current directory. *)

val write_coverage_data_on_exit : unit -> unit
(** Registers {!write_coverage_data} to be called automatically on process
    exit. *)

val reset_coverage_data : unit -> unit
(** [reset_coverage_data ()] clears accumulated coverage statistics. *)
